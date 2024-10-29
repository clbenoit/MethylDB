#app/view/tsne_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel,
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateTabsetPanel,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent,
        updateSelectizeInput, fluidPage, bindCache, reactive,
        observe, reactiveValues, bindEvent, isolate],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange, inner_join, rename],
  reactable,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
        reactable$reactableOutput(ns("annotations"))
      )
    )
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    req(appData$BValsC)

    current_tsne_dataframe <- reactive({
      print("okidoki")
      #req(appData$selectors$classes)
      BValsC <- appData$BValsC %>%
        filter()
      return(BValsC)
    }) #%>% bindCache({list()})
    #
    # observe({
    #   req(current_tsne_dataframe())
    #   output$annotations <- reactable$renderReactable(
    #     #print(head(appData$current_tsne_dataframe())),
    #     reactable$reactable(current_tsne_dataframe(), filterable = TRUE)
    #   )
    # })

  })
}

