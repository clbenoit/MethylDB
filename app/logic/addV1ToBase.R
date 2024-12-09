box::use(
  IlluminaHumanMethylation450kanno.ilmn12.hg19[],
  IlluminaHumanMethylation450kmanifest[],
  minfi[read.metharray.exp, getAnnotation],
  modelTsne[],
  ComplexHeatmap[],
  IlluminaHumanMethylationEPICmanifest[],
  dplyr[],
  DBI[dbExistsTable, dbReadTable, dbWriteTable],
  RSQLite[]
)

#' @export
addV1ToBase <- function(annotations, rawdata, db_path) {

  library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
  library(minfi)
  library(DBI)
  library(RSQLite)
  library(dplyr)

  rawdata <- "app/data/testdata/EPICv1/"
  rgSet <- read.metharray.exp(rawdata,
                              force = TRUE, verbose = TRUE)

  detP <- data.frame(detectionP(rgSet))
  colnames(detP) <- gsub("_.*", "", colnames(detP))
  colnames(rgSet) <- gsub("_.*", "", colnames(rgSet))
  keep <- colMeans(detP) < 0.05
  rgSet <- rgSet[, keep]
  detP <- detP[, keep]

  # Perform preprocessing (Noob method)
  mSetSq <- preprocessNoob(rgSet)

  # Filter based on detection p-values
  detP <- detP[match(featureNames(mSetSq), rownames(detP)),]
  #detP <- detP[match(featureNames(mSetSq), names(detP))]

  keep <- rowSums(detP < 0.01) == ncol(mSetSq)
  table(keep)
  mSetSqFlt <- mSetSq[keep, ]

  # Free up memory
  rm(mSetSq)
  rm(rgSet)
  gc()

  # Remove CHRXY regions using the IlluminaHumanMethylation450k annotation
  annEPIC <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

  keep <- !(featureNames(mSetSqFlt) %in% annEPIC$Name[annEPIC$chr %in% c("chrX", "chrY")])
  mSetSqFlt <- mSetSqFlt[keep, ]
  BValsC <- as.data.frame(getBeta(mSetSqFlt))
  #BValsC <- BValsC[1:100,]
  BValsC$cgID <- rownames(BValsC)
  #MVals <- getM(mSetSqFlt)

  # Print message if any required column is missing
  annotations <- "app/data/annotations/GSE225810_classes.csv"
  annotations <- read.table(annotations, header = TRUE, sep = ",")
  # Check if required columns are present
  required_columns <- c("class", "subclass", "sample", "cohort","chip")
  missing_columns <- setdiff(required_columns, names(annotations))
  if (length(missing_columns) > 0) {
    print("Check file format")
  } else {
    print("All required columns are present")
  }

  annotations <- annotations %>% filter(sample %in% colnames(BValsC))
  BValsC <- BValsC[, c(annotations$sample,"cgID")]

  # Save data into database
  db_path <- file.path("app/data/testdata", "testdb.db")
  con <- dbConnect(SQLite(), db_path)

  if (dbExistsTable(conn = con, "BValsC")) {
    print("merging BValsC tables")

    # Step 1: Write the new data to a temporary table
    dbWriteTable(conn = con, name = "BValsC_temp", value = BValsC, overwrite = TRUE)

    # Step 2: Get the column names for both tables
    existing_columns <- dbGetQuery(con, "PRAGMA table_info(BValsC)")$name
    temp_columns <- dbGetQuery(con, "PRAGMA table_info(BValsC_temp)")$name

    # Exclude the primary key "cgID" from update columns
    columns_to_update <- intersect(existing_columns, temp_columns)
    columns_to_update <- columns_to_update[columns_to_update != "cgID"]

    # Step 3: Dynamically build the SET clause for the UPDATE statement
    if(length(columns_to_update) > 0){

    set_clause <- paste(
      sprintf("%s = (SELECT %s FROM BValsC_temp WHERE BValsC.cgID = BValsC_temp.cgID)",
              columns_to_update, columns_to_update),
      collapse = ", "
    )

    # Update existing rows based on cgID for matching columns
    update_query <- sprintf("
    UPDATE BValsC
    SET %s
    WHERE EXISTS (SELECT 1 FROM BValsC_temp WHERE BValsC.cgID = BValsC_temp.cgID)
    ", set_clause)

    # Execute the update query
    dbExecute(conn = con, update_query)
    }

    # Step 4: Insert new rows from BValsC_temp where cgID is not present in BValsC
    insert_query <- sprintf("
    INSERT INTO BValsC (%s)
    SELECT %s FROM BValsC_temp
    WHERE cgID NOT IN (SELECT cgID FROM BValsC)
  ", paste(temp_columns, collapse = ", "), paste(temp_columns, collapse = ", "))

    # Execute the insert query
    dbExecute(conn = con, insert_query)

    # Step 5: Add new columns in BValsC for any columns unique to BValsC_temp
    for (col in columns_to_update) {
      # Assuming all new columns are of type REAL; adjust as necessary
      alter_query <- sprintf("ALTER TABLE BValsC ADD COLUMN %s REAL", col)
      dbExecute(conn = con, alter_query)

      # Now, populate the new columns with data from BValsC_temp
      populate_query <- sprintf("
      UPDATE BValsC
      SET %s = (SELECT %s FROM BValsC_temp WHERE BValsC.cgID = BValsC_temp.cgID)
      WHERE EXISTS (SELECT 1 FROM BValsC_temp WHERE BValsC.cgID = BValsC_temp.cgID)
    ", col, col)
      dbExecute(conn = con, populate_query)
    }
    # Step 6: Drop the temporary table after the merge
    dbRemoveTable(conn = con, "BValsC_temp")

  } else {
    # If BValsC does not exist, create it directly with BValsC data
    print("writing first BValsC table")
    dbWriteTable(conn = con, name = "BValsC", value = BValsC)
  }

  if (dbExistsTable(conn = con, "annotations")) {
    print("merging annotations")
    annotations_old <- dbReadTable(conn = con, "annotations") %>%
      filter(!(sample %in% annotations$sample))
    annotations <- rbind(annotations, annotations_old)
    #annotations <- annotations_old %>% left_join(annotations, by = "sample", suffix = c("",""))
    annotations <- annotations[,c("sample","class","subclass","cohort", "chip")]
    dbWriteTable(conn = con, "annotations", value = annotations, overwrite = TRUE)
  } else {
    print("write first annotations")
    dbWriteTable(conn = con, "annotations", value = annotations)
  }

}

