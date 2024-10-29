#app/view/select_samples.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel,
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateTabsetPanel,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent,
        updateSelectizeInput, fluidPage, bindCache, reactive,
        observe, reactiveValues, bindEvent, isolate, icon,
        mainPanel, div, actionButton, HTML],
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
    card(id = "card1", width = 12, full_screen = FALSE, min_height = '150px',
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
              pickerInput(ns("select_cohorts"), label = "cohort",
                          selected = NULL, choices = NULL, multiple = TRUE,
                          width = "100%")
              )
            ))),
        #fluidRow(actionButton(ns("btn1"), "More info about selected samples", class = "btn btn-link")),
        card(id = "card2", width = 12, full_screen = TRUE, min_height = '300px',
             card_header("Some metrics about selected cohort"),
             card_body(
            #div(id = "card-info", style = "display: block;",
            fluidRow(uiOutput(ns('selected_samples_info'))),
            fluidRow(layout_column_wrap(
                             width = 1/2,
                              plotlyOutput(ns("classes_pie")),
                              plotlyOutput(ns("subclasses_pie")),
                              plotlyOutput(ns("cohorts_pie")))
                             )
                           #)
                   ))#,
            # tags$script(HTML("
            # Shiny.addCustomMessageHandler('toggleDiv', function(message) {
            #   var div = document.getElementById('card2');
            #   if (div.style.display === 'none') {
            #     div.style.display = 'block';
            #   } else {
            #     div.style.display = 'none';
            #   }
            # });"))
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    req(appData$annotations)

    all_samples_dataframe <- reactive({
      return(appData$annotations)
    }) #%>% bindCache({list()})

    observe({
      req(all_samples_dataframe())
      output$annotations <- reactable$renderReactable(
        reactable$reactable(all_samples_dataframe(),
                            filterable = TRUE)
      )
    })

    observe({
      req(appData$annotations)
      print("update select_classes input")
      print(appData$annotations$class)
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
      updatePickerInput(inputId = "select_cohorts",
                        session = session,
                        choices = c("All",
                                    unique(appData$annotations$cohort)),
                        selected = "All")
    })

    observeEvent(input$select_classes, {
      req(input$select_classes)
      print("appData$selectors$classes <- input$select_classes")
      appData$selectors$classes <- input$select_classes
    })
    observeEvent(input$select_subclasses, {
      req(input$select_subclasses)
      print("appData$selectors$subclasses <- input$select_subclasses")
      appData$selectors$subclasses <- input$select_subclasses
      print(appData$selectors$subclasses)
    })
    observeEvent(input$select_cohorts, {
      req(input$select_cohorts)
      print("appData$selectors$subclasses <- input$select_cohorts")
      appData$selectors$cohorts<- input$select_cohorts
      print(appData$selectors$cohorts)
    })

    current_samples_dataframe <- reactive({
      req(appData$selectors$subclasses)
      req(appData$selectors$classes)
      req(appData$selectors$cohorts)
      req(all_samples_dataframe())
      print("filtering samples...")
      current_samples_dataframe <- all_samples_dataframe() %>%
        {
          if (appData$selectors$classes != "All") {
            . %>% filter(class %in% appData$selectors$classes)
          } else {
            .
          }
        } %>%
        {
          if (appData$selectors$subclasses != "All") {
            . %>% filter(subclass %in% appData$selectors$subclasses)
          } else {
            .
          }
        } %>%
        {  if (appData$selectors$cohorts != "All") {
            . %>% filter(class %in% appData$selectors$cohorts)
          } else {
            .
          }
        } %>%
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
      # Group by selected column and count occurrences
      req(current_samples_dataframe())
      print("rendering classes pie")
      data <- current_samples_dataframe() %>%
        group_by_at("class") %>%
        summarise(count = n()) #%>%
        #filter(!is.na(!!sym("class"))) # Remove NA values

      # Plot pie chart
      plot_ly(data, labels = ~class, values = ~count, type = 'pie') %>%
        layout(title = paste("Pie chart of", "classes"),
               showlegend = TRUE)
    })


    output$subclasses_pie <- renderPlotly({
      # Group by selected column and count occurrences
      req(current_samples_dataframe())
      print(utils::head(current_samples_dataframe()))
      data <- current_samples_dataframe() %>%
        group_by_at("subclass") %>%
        summarise(count = n()) #%>%
        #filter(!is.na(!!sym("subclass"))) # Remove NA values

      # Plot pie chart
      plot_ly(data, labels = ~subclass, values = ~count, type = 'pie') %>%
        layout(title = paste("Pie chart of", "subclasses"),
               showlegend = TRUE)
      })

    output$cohorts_pie <- renderPlotly({
      # Group by selected column and count occurrences
      req(current_samples_dataframe())
      print(utils::head(current_samples_dataframe()))
      data <- current_samples_dataframe() %>%
        group_by_at("cohort") %>%
        summarise(count = n()) #%>%
      #filter(!is.na(!!sym("subclass"))) # Remove NA values

      # Plot pie chart
      plot_ly(data, labels = ~cohort, values = ~count, type = 'pie') %>%
        layout(title = paste("Pie chart of", "cohorts"),
               showlegend = TRUE)
    })

  # observeEvent(input$btn1, {
  #   print(input$btn1)
  #   session$sendCustomMessage("toggleDiv", list())
  # })
  #
  # session$sendCustomMessage("toggleDiv", list())

  })
}

