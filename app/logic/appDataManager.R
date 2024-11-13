box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req, reactive],
  RSQLite[SQLite],
  dplyr[`%>%`, filter],
  stats[setNames],
  config[get],
  shinybusy[remove_modal_spinner, show_modal_spinner],
  DBI[dbReadTable, dbGetQuery, dbExistsTable, dbExecute, dbSendQuery],
  utils[read.table],
  sesame[mLiftOver]
)

#' @export
appDataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    con = NULL,
    selectors = reactiveValues(classes = "All" , subclasses = "All", cohorts = "All"),
    data = reactiveValues(current_samples_dataframe = NULL, annotations = NULL, BValsC = NULL, BValsC_V2 = NULL),
    loadAppData = function(con) {
      print("inside load DB")
      self$con <- con
      #print(con)
      shinybusy::show_modal_spinner(
        spin = "double-bounce", color = "#112446",
        text = "Loading database metadata")

      # Different sidebars according to selected tab
      if (dbExistsTable(conn = con, "annotations")) {
        print("Loading annotations")
        self$data$annotations <- DBI::dbReadTable(conn = con, name = "annotations")
      } else {
        print("Can't find annotations table in base, check you database")
      }

      if (dbExistsTable(conn = con, "BValsC")) {
        print("Loading Beta values")
        self$data$BValsC <- DBI::dbReadTable(conn = con, name = "BValsC")
      } else {
        print("Can't find beta values table in base, check you database")
      }

      if (dbExistsTable(conn = con, "BValsC_V2")) {
        print("Loading Beta values")
        BValsC_V2 <- DBI::dbReadTable(conn = con, name = "BValsC_V2")
        # print(utils::head(BValsC_V2))

        rownames(BValsC_V2) <- BValsC_V2$cgID
        BValsC_V2$cgID <- NULL
        #print(utils::head(BValsC_V2))
        #print(as.numeric(BValsC_V2))
        # print(class(BValsC_V2$GSM2402855))
        #BValsC_V2 <- data.frame(lapply(BValsC_V2, as.numeric))
        # print(class(BValsC_V2$GSM2402855))
        # library(sesame)
        # sesameData::sesameDataCache()
        print("zebi")
        print(utils::head(BValsC_V2))
        print(nrow(BValsC_V2))
        #save(file = "/home/ptngs/test.rda", BValsC_V2)
        #a <- as.data.frame(mLiftOver(x = BValsC_V2, target_platform = "HM450"))
        BValsC_V2 <- as.data.frame(mLiftOver(x = as.matrix(BValsC_V2), target_platform = "HM450"))
        print(nrow(BValsC_V2))
        print(utils::head(BValsC_V2))
        #print(utils::head(BValsC_V2))
        #print(utils::head(BValsC_V2))
        self$data$BValsC_V2 <- BValsC_V2
      } else {
        print("Can't find V2 beta values table in base, check you database")
      }

      remove_modal_spinner()
    }
  )
)
