box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        shinyOptions],
  config[get],
  cachem[cache_disk],
  DBI[dbConnect],
  RSQLite[SQLite]
)


box::use(
  app/logic/appDataManager[appDataManager],
  app/view/tsne_view,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    tsne_view$ui(ns("tsne_view"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## shiny options ##
    options(future.globals.maxSize = 10000*1024^2)

    # set up cache directory ##
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    #config <- get()
    tempdir <- tempdir()
    print(get("cache_directory"))
    if (get("cache_directory") ==  "default") {
      dir.create(file.path(tempdir, "cache"))
      print(paste0("using following cache directory : ", file.path(tempdir, "cache")))
      shinyOptions(cache = cache_disk(file.path(tempdir,"cache")))
    } else {
      print(paste0("using following cache directory : ",
                   get("cache_directory")))
      shinyOptions(cache = cache_disk(get("cache_directory")))
    }
    # Set up default user
    if(Sys.getenv("SHINYPROXY_USERNAME") == ""){
      Sys.setenv(SHINYPROXY_USERNAME = "Me")
    }

    ## load database ##
    # db_name <- file.path(get("db_path"), paste0(get("prefix"), ".db"))
    # con <- dbConnect(SQLite(), db_name)
    db_name <- file.path("app/data/testdata", "testdb.db")
    con <- dbConnect(SQLite(), db_name)

    appDataManager <- appDataManager$new()
    appDataManager$loadAppData(con)


    tsne_view$server("tsne_view", appData = appDataManager, main_session = session)

  })
}
