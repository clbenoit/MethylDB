box::use(
  IlluminaHumanMethylation450kanno.ilmn12.hg19[],
  IlluminaHumanMethylation450kmanifest[],
  minfi[read.metharray.exp, getAnnotation],
  modelTsne[],
  ComplexHeatmap[],
  IlluminaHumanMethylationEPICmanifest[],
  dplyr[filter],
  DBI[dbExistsTable, dbReadTable, dbWriteTable],
  RSQLite[]
)

#' @export
addV2ToBase <- function(annotations, rawdata, db_path) {

  library(sesame)
  library(DBI)
  library(RSQLite)
  library(dplyr)

  rawdata <- "app/data/testdata/EPICv2/"
  betas <- openSesame(rawdata, platform = "EPICv2")

  # Transform EPICV2 into a 850K dataframe
  ###### ATTENTION ON FAIT LE SHIFT AVANT BASE CAR ON PEUT TESTER QUE SUR 100 LIGNES ET ON VEUT SASSURER
  #QUIL Y A DES LIGNES EN BASE CORRESPONDANT DEJA AUX V1
  #betas <- mLiftOver(x = betas, target_platform = "EPIC")
  BValsC_V2 <- as.data.frame(betas)
  # BValsC_V2 <- na.omit(BValsC_V2)[1:100,]

  annotations <- "app/data/annotations/GSE109381_classes.csv"

  annotations <- read.table(annotations, header = TRUE, sep = ",")
  # Check if required columns are present
  required_columns <- c("class", "subclass", "sample", "cohort","chip")
  missing_columns <- setdiff(required_columns, names(annotations))
  if (length(missing_columns) > 0) {
    print("Check file format")
  } else {
    print("All required columns are present")
  }

  colnames(BValsC_V2) <- gsub("_.*", "", colnames(BValsC_V2))
  annotations <- annotations %>% dplyr::filter(sample %in% colnames(BValsC_V2))
  BValsC_V2$cgID <- rownames(BValsC_V2)
  BValsC_V2 <- BValsC_V2[, c(annotations$sample, "cgID")]

  # Save data into database
  db_path <- file.path("app/data/testdata", "testdb.db")
  con <- dbConnect(SQLite(), db_path)

  if (dbExistsTable(conn = con, "BValsC_V2")) {
    print("merging BValsC_V2 tables")

    # Step 1: Write the new data to a temporary table
    dbWriteTable(conn = con, name = "BValsC_V2_temp", value = BValsC_V2, overwrite = TRUE)

    # Step 2: Get the column names for both tables
    existing_columns <- dbGetQuery(con, "PRAGMA table_info(BValsC_V2)")$name
    temp_columns <- dbGetQuery(con, "PRAGMA table_info(BValsC_V2_temp)")$name

    # Exclude the primary key "cgID" from update columns
    columns_to_update <- intersect(existing_columns, temp_columns)
    columns_to_update <- columns_to_update[columns_to_update != "cgID"]

    #Step 3: Dynamically build the SET clause for the UPDATE statement
    if(length(columns_to_update) > 0){
    set_clause <- paste(
      sprintf("%s = (SELECT %s FROM BValsC_V2_temp WHERE BValsC_V2.cgID = BValsC_V2_temp.cgID)",
              columns_to_update, columns_to_update),
      collapse = ", "
    )

    # Update existing rows based on cgID for matching columns
    update_query <- sprintf("
    UPDATE BValsC_V2
    SET %s
    WHERE EXISTS (SELECT 1 FROM BValsC_V2_temp WHERE BValsC_V2.cgID = BValsC_V2_temp.cgID)
    ", set_clause)

    # Execute the update query
    dbExecute(conn = con, update_query)
    }

    # Step 4: Insert new rows from BValsC_V2_temp where cgID is not present in BValsC_V2
    insert_query <- sprintf("
    INSERT INTO BValsC_V2 (%s)
    SELECT %s FROM BValsC_V2_temp
    WHERE cgID NOT IN (SELECT cgID FROM BValsC_V2)
  ", paste(temp_columns, collapse = ", "), paste(temp_columns, collapse = ", "))

    # Execute the insert query
    dbExecute(conn = con, insert_query)

    # Step 5: Add new columns in BValsC_V2 for any columns unique to BValsC_V2_temp
    for (col in columns_to_update) {
      # Assuming all new columns are of type REAL; adjust as necessary
      alter_query <- sprintf("ALTER TABLE BValsC_V2 ADD COLUMN %s REAL", col)
      dbExecute(conn = con, alter_query)

      # Now, populate the new columns with data from BValsC_V2_temp
      populate_query <- sprintf("
      UPDATE BValsC_V2
      SET %s = (SELECT %s FROM BValsC_V2_temp WHERE BValsC_V2.cgID = BValsC_V2_temp.cgID)
      WHERE EXISTS (SELECT 1 FROM BValsC_V2_temp WHERE BValsC_V2.cgID = BValsC_V2_temp.cgID)
    ", col, col)
      dbExecute(conn = con, populate_query)
    }
    # Step 6: Drop the temporary table after the merge
    dbRemoveTable(conn = con, "BValsC_V2_temp")

  } else {
    print("writing first BValsC_V2 table")
    dbWriteTable(conn = con, name = "BValsC_V2", value = BValsC_V2)
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

