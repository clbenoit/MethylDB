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
addV2ToBase <- function(annotations, rawdata, db_path = NULL) {

  # library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
  # library(minfi)
  # library(DBI)
  # library(RSQLite)
  # library(dplyr)
  #
  rawdata <- "app/data/testdata/EPICv2/"
  betas <- openSesame(rawdata, platform = "EPICv2", BPPARAM = BiocParallel::MulticoreParam(workers = 13))

  # Transform EPICV2 into a 850K dataframe
  ###### ATTENTION ON FAIT LE SHIFT AVANT BASE CAR ON PEUT TESTER QUE SUR 100 LIGNES ET ON VEUT SASSURER
  #QUIL Y A DES LIGNES EN BASE CORRESPONDANT DEJA AUX V1
  #betas <- mLiftOver(x = betas, target_platform = "EPIC")
  BValsC_V2 <- as.data.frame(betas)
  # BValsC_V2 <- na.omit(BValsC_V2)[1:100,]

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

  # colnames(BValsC_V2) <- gsub("_.*", "", colnames(BValsC_V2))
  annotations <- annotations %>% dplyr::filter(sample %in% colnames(BValsC_V2))
  BValsC_V2$cgID <- rownames(BValsC_V2)
  BValsC_V2 <- BValsC_V2[, c(annotations$sample, "cgID")]

  # Save data into database
  #db_path <- "/home/T_SNE/data/dbs/CHUGA_V2.db"
  db_path <- file.path("app/data/testdata", "testdb.db")
  con <- dbConnect(SQLite(), db_path)

  if (dbExistsTable(conn = con, "BValsC_V2")) {
    print("merging BValsC_V2 tables")


    # Step 1: Write the new data to a temporary table
    dbWriteTable(conn = con, name = "BValsC_V2_temp", value = BValsC_V2, overwrite = TRUE)

    existing_columns <- dbGetQuery(con, "PRAGMA table_info(BValsC_V2)")$name
    temp_columns <- dbGetQuery(con, "PRAGMA table_info(BValsC_V2_temp)")$name

    # Step 2: Determine columns to update (existing columns) and columns to add (new columns)
    columns_to_update <- setdiff(intersect(existing_columns, temp_columns), "cgID")
    columns_to_add <- setdiff(temp_columns, existing_columns)

    if (length(columns_to_add) > 0) {
      message("Adding new columns to BValsC_V2...")
      for (col in columns_to_add) {
        # Get the column type from the temporary table
        table_info <- dbGetQuery(con, "PRAGMA table_info(BValsC_V2_temp)")
        col_type <- table_info$type[table_info$name == col]

        # Check if col_type is found
        if (length(col_type) > 0) {
          alter_query <- sprintf("ALTER TABLE BValsC_V2 ADD COLUMN \"%s\" %s", col, col_type)
          dbExecute(con, alter_query)
        } else {
          warning(sprintf("Column '%s' not found in BValsC_V2_temp schema.", col))
        }
      }
    }

    # Step 4: Update existing columns and new columns in a single query
    all_columns_to_update <- c(columns_to_update, columns_to_add)
    if (length(all_columns_to_update) > 0) {
      message("Updating columns in BValsC_V2...")

      # Generate the SET clause for updating all relevant columns
      populate_clause <- paste(
        sprintf('"%s" = "BValsC_V2_temp"."%s"', all_columns_to_update, all_columns_to_update),
        collapse = ", "
      )

      # Construct the update query
      populate_query <- sprintf("
    UPDATE BValsC_V2
    SET %s
    FROM BValsC_V2_temp
    WHERE BValsC_V2.cgID = BValsC_V2_temp.cgID
  ", populate_clause)

      # Print the query for debugging
      print(populate_query)

      # Execute the update query within a transaction for better performance
      dbBegin(con)
      dbExecute(con, populate_query)
      dbCommit(con)

      message("Columns updated successfully.")

      # Step 5: Drop the temporary table
      dbRemoveTable(con, "BValsC_V2_temp")
    }
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
