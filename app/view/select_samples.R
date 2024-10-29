#app/view/select_samples.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel,
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateTabsetPanel,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent,
        updateSelectizeInput, fluidPage, bindCache, reactive,
        observe, reactiveValues, bindEvent, isolate, icon,
        mainPanel, div, actionButton,
        HTML, checkboxGroupInput, updateCheckboxGroupInput, need, validate],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange,
        inner_join, rename, summarise, group_by_at, n, sym],
  reactable,
  shinydashboard[infoBox],
  shinydashboardPlus[box],
  shinyWidgets[pickerInput, updatePickerInput],
  plotly[plotlyOutput, renderPlotly, layout, plot_ly],
  bslib[card, card_header, card_body, value_box, layout_column_wrap]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(br(),
    card(id = "card0", width = 12, min_height = '300px',full_screen = TRUE,
         card_header("Available samples in database"),
         card_body(reactable$reactableOutput(ns("annotations")))
    ),
    card(id = "card1", width = 12, full_screen = FALSE, min_height = '250px',
        card_header("Select samples to compute T SNE"),
        card_body(
          fluidRow(
            layout_column_wrap(
              width = 1/3,
              pickerInput(ns("select_classes"), label = "class",
                          selected = NULL, choices = NULL, multiple = TRUE,
                          width = "100%"),
              pickerInput(ns("select_subclasses"), label = "subclass",
                          selected = NULL, choices = NULL, multiple = TRUE,
                          width = "100%"),
              checkboxGroupInput(ns("select_cohorts"), "cohorts", width = "100%", inline = TRUE)
              )
            ))),
        card(id = "card2", width = 12, full_screen = TRUE, min_height = '500px',
             card_header("Some metrics about selected cohort"),
             card_body(
            fluidRow(uiOutput(ns('selected_samples_info'))),
            fluidRow(layout_column_wrap(
                             width = 1/2,
                              plotlyOutput(ns("classes_pie")),
                              plotlyOutput(ns("subclasses_pie")),
                              plotlyOutput(ns("cohorts_pie")))
                             )
                   ))
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    all_samples_dataframe <- reactive({
      return(appData$data$annotations)
    }) #%>% bindCache({list()})

    observe({
      req(all_samples_dataframe())
      output$annotations <- reactable$renderReactable(
        reactable$reactable(all_samples_dataframe(),
                            filterable = TRUE)
      )
    })

    observe({
      req(appData$data$annotations)
      print("update select_classes input")
      updatePickerInput(inputId = "select_classes",
                        session = session,
                        choices = c("All",
                                    unique(appData$annotations$class)),
                        selected = "All")
      updatePickerInput(inputId = "select_subclasses",
                        session = session,
                        choices = c("All",
                                    unique(appData$annotations$subclass)),
                        selected = "All")
      updateCheckboxGroupInput(inputId = "select_cohorts",
                               session = session,
                               inline = TRUE,
                               choices = c("All",
                                           unique(appData$annotations$cohort)),
                               selected = "All")
    })

    observeEvent(input$select_classes, {
      req(input$select_classes)
      appData$selectors$classes <- input$select_classes
    })
    observeEvent(input$select_subclasses, {
      req(input$select_subclasses)
      appData$selectors$subclasses <- input$select_subclasses
    })
    observeEvent(input$select_cohorts, {
      req(input$select_cohorts)
      appData$selectors$cohorts<- input$select_cohorts
      print(appData$selectors$cohorts)
    })

    current_samples_dataframe <- reactive({
      req(appData$selectors$subclasses)
      req(appData$selectors$classes)
      req(appData$selectors$cohorts)
      req(all_samples_dataframe())
      shiny::validate(
        shiny::need(!is.null(input$select_classes) && length(input$select_classes) > 0, "Please select at least one class."),
        shiny::need(!is.null(input$select_subclasses) && length(input$select_subclasses) > 0, "Please select at least one subclass."),
        shiny::need(!is.null(input$select_cohorts) && length(input$select_cohorts) > 0, "Please select at least one cohort.")
      )
      print("filtering samples...")
      current_samples_dataframe <- all_samples_dataframe() %>%
        {
          if (!("All" %in% appData$selectors$classes)) {
            filter(., class %in% appData$selectors$classes)
          } else {
            .
          }
        } %>%
        {
          if (!("All" %in% appData$selectors$subclasses)) {
            filter(., subclass %in% appData$selectors$subclasses)
          } else {
            .
          }
        } %>%
        {
          if (!("All" %in% appData$selectors$cohorts)) {
            filter(., cohort %in% appData$selectors$cohorts)
          } else {
            .
          }
        }
      appData$data$current_samples_dataframe <- current_samples_dataframe
      return(current_samples_dataframe)
    })

    output$selected_samples_info <- renderUI({
        req(current_samples_dataframe())
              value_box(title = "N° of selected samples",
                      value = as.character(nrow(current_samples_dataframe())),
                      showcase = icon("person"), width = "100%",
                      showcase_layout = c("left center"),
                      theme = "primary")
    })

    output$classes_pie <- renderPlotly({
      req(current_samples_dataframe())
      print("rendering classes pie")
      data <- current_samples_dataframe() %>%
        group_by_at("class") %>%
        summarise(count = n())

      plot_ly(data, labels = ~class, values = ~count, type = 'pie') %>%
        layout(title = paste("Pie chart of", "classes"),
               showlegend = TRUE)
    })


    output$subclasses_pie <- renderPlotly({
      req(current_samples_dataframe())
      data <- current_samples_dataframe() %>%
        group_by_at("subclass") %>%
        summarise(count = n())

      plot_ly(data, labels = ~subclass, values = ~count, type = 'pie') %>%
        layout(title = paste("Pie chart of", "subclasses"),
               showlegend = TRUE)
      })

    output$cohorts_pie <- renderPlotly({
      req(current_samples_dataframe())
      data <- current_samples_dataframe() %>%
        group_by_at("cohort") %>%
        summarise(count = n())

      plot_ly(data, labels = ~cohort, values = ~count, type = 'pie') %>%
        layout(title = paste("Pie chart of", "cohorts"),
               showlegend = TRUE)
    })

  })
}

