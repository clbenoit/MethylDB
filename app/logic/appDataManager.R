box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req, reactive],
  RSQLite[SQLite],
  dplyr[`%>%`, filter],
  stats[setNames],
  config[get],
  shinybusy[remove_modal_spinner, show_modal_spinner],
  DBI[dbReadTable, dbGetQuery, dbExistsTable, dbExecute, dbSendQuery],
  utils[read.table]
)

#' @export
appDataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    con = NULL,
    selectors = reactiveValues(tab = "VariantView", variant = NULL),
    annotations = NULL,
    BValsC = NULL,
    loadAppData = function(con) {
      print("inside load DB")
      self$con <- con
      print(con)
      shinybusy::show_modal_spinner(
        spin = "double-bounce", color = "#112446",
        text = "Loading database metadata")

      # Different sidebars according to selected tab
      if (dbExistsTable(conn = con, "annotations")) {
        self$annotations <- DBI::dbReadTable(conn = con, name = "annotations")
      } else {
        print("Can't find annotations table in base, check you database")
      }

      if (dbExistsTable(conn = con, "BValsC")) {
        self$BValsC <- DBI::dbReadTable(conn = con, name = "BValsC")
      } else {
        print("Can't find beta values table in base, check you database")
      }

      remove_modal_spinner()
    }
  )
)
