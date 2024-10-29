box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        shinyOptions, p, icon],
  config[get],
  cachem[cache_disk],
  DBI[dbConnect],
  RSQLite[SQLite],
  bslib[page_fluid, page_navbar, nav_panel, nav_spacer, nav_menu, nav_item,
        bs_theme]
)


box::use(
  app/logic/appDataManager[appDataManager],
  app/view/tsne_view,
  app/view/select_samples,
)

link_github <- tags$a(
  icon("github"),"Code",
  href = "https://github.com/clbenoit/MethylDB",
  target = "_blank"
)
link_doc <- tags$a(
  icon("book")," Documentation",
  href = "https://clbenoit.github.io/portfolio/",
  target = "_blank"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
  page_navbar(
    title = "MethylDB",
    theme = bs_theme(bootswatch = "minty",
                     bg = "#FCFDFD",
                     fg = "rgb(25, 125, 85)"),
    underline = TRUE,
    nav_panel(title = "Select Samples",
              select_samples$ui(ns("select_samples"))),
    nav_panel(title = "Run T SNE",
              tsne_view$ui(ns("tsne_view"))),
    nav_panel(title = "Run differential merhylation analysis",
              p("Third tab content")),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_github),
      nav_item(link_doc)
      )
    ))
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
    select_samples$server("select_samples", appData = appDataManager, main_session = session)

  })
}
