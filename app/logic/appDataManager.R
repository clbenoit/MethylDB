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
    selectors = reactiveValues(classes = "All" , subclasses = "All", cohorts = "All", chips = "All"),
    data = reactiveValues(current_samples_dataframe = NULL, annotations = NULL, BValsC = NULL, BValsC_V2 = NULL),
    loadAppData = function(con) {
      print("inside load DB")
      self$con <- con
      shinybusy::show_modal_spinner(
        spin = "double-bounce", color = "#112446",
        text = "Loading database metadata")

      # Different sidebars according to selected tab
      if (dbExistsTable(conn = con, "annotations")) {
        print("Loading annotations")
        self$data$annotations <- DBI::dbReadTable(conn = con, name = "annotations", check.names = FALSE)
      } else {
        print("Can't find annotations table in base, check you database")
      }

      if (dbExistsTable(conn = con, "BValsC")) {
        print("Loading V1 Beta values")
        self$data$BValsC <- DBI::dbReadTable(conn = con, name = "BValsC", check.names = FALSE)
      } else {
        print("Can't find V1 beta values table in base, check you database")
      }

      if (dbExistsTable(conn = con, "BValsC_V2")) {
        print("Loading V2 Beta values")
        BValsC_V2 <- DBI::dbReadTable(conn = con, name = "BValsC_V2", check.names = FALSE)

        rownames(BValsC_V2) <- BValsC_V2$cgID
        BValsC_V2$cgID <- NULL
        #a <- as.data.frame(mLiftOver(x = BValsC_V2, target_platform = "HM450"))
        #BValsC_V2 <- as.data.frame(mLiftOver(x = as.matrix(BValsC_V2), target_platform = "HM450"))
        self$data$BValsC_V2 <- BValsC_V2
      } else {
        print("Can't find V2 beta values table in base, check you database")
      }

      remove_modal_spinner()
    }
  )
)
