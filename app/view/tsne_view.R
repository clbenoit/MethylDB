#app/view/tsne_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel,
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateTabsetPanel,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent,
        updateSelectizeInput, fluidPage, bindCache, reactive,
        observe, reactiveValues, bindEvent, isolate, radioButtons, div],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange, inner_join, rename],
  reactable,
  stats[na.omit],
  plotly[plotlyOutput, renderPlotly, layout, plot_ly,
         plotlyProxy, plotlyProxyInvoke],
  htmlwidgets[onRender],
)

box::use(
  app/logic/selectVariables[selectVariables],
  app/logic/modelTsne[modelTsne],
)

#' @export
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             div(style = "width: 100%; text-align: center;",
                 radioButtons(ns("color_by"), label = "Color TSNE by",
                              inline = TRUE,
                              choices = c("class", "subclass", "cohort"),
                              width = "100%")
             )
      ),
      br(),
      column(width = 12,
             selectInput(ns("sample_selector"), "Select Samples to annotate on graph",
                         choices = NULL, width = '100%',
                         multiple = TRUE),  # Enable multiple selection
             plotlyOutput(ns("current_tsne_plot"), height = "800px"))
    )
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    annotations <- shiny::reactiveVal(list())  # Store plot annotations

    # Update selectInput choices dynamically
    observeEvent(appData$data$current_samples_dataframe$sample, {
      shiny::updateSelectInput(session, inputId = "sample_selector",
                               choices = appData$data$current_samples_dataframe$sample)
    })

    # Reactive for current t-SNE data frame
    current_tsne_dataframe <- reactive({
      req(appData$data$BValsC, appData$data$current_samples_dataframe)
      BValsC <- appData$data$BValsC
      rownames(BValsC) <- BValsC$cgID
      BValsC$cgId <- NULL
      BValsC <- BValsC[, appData$data$current_samples_dataframe$sample]
      selectVariables(data = na.omit(BValsC), method = "SD", no.variables = 25000, threads = 13)
    }) %>% bindCache(list(appData$data$BValsC, appData$data$current_samples_dataframe))

    # Reactive for current t-SNE model
    current_tsne_model <- reactive({
      req(current_tsne_dataframe())
      modelTsne(current_tsne_dataframe(), perplexity = 1, dims = 2)
    }) %>% bindCache(current_tsne_dataframe())

    # Render t-SNE plot
    output$current_tsne_plot <- renderPlotly({
      req(current_tsne_model(), appData$data$current_samples_dataframe, input$color_by)
      df <- current_tsne_model()$df.tsne

      plot_ly(
        x = df$tsne.dim1, y = df$tsne.dim2,
        type = "scatter", mode = "markers",
        color = appData$data$current_samples_dataframe[, input$color_by],
        text = ~paste("Sample: ", df$sample),
        hoverinfo = 'text',
        marker = list(size = 10)
      ) %>%
        layout(
          title = "2D t-SNE",
          xaxis = list(title = "t-SNE Dimension 1", zeroline = FALSE),
          yaxis = list(title = "t-SNE Dimension 2", zeroline = FALSE)
        ) %>%
        onRender(sprintf("
        function(el, x) {
          el.on('plotly_click', function(d) {
            var sampleName = d.points[0].text;

            // Add sample name to Shiny input with namespace
            Shiny.setInputValue('%s', sampleName, {priority: 'event'});
          });
        }
      ", ns("sample_click")))
    })

    # Observe plotly click to update selectInput for multiple selections
    observeEvent(input$sample_click, {
      sample_name <- strsplit(input$sample_click, "[: ]+")[[1]][2]
      selected_samples <- input$sample_selector

      # Update selectInput to include clicked sample if not already selected
      if (!sample_name %in% selected_samples) {
        selected_samples <- c(selected_samples, sample_name)
        shiny::updateSelectInput(session, "sample_selector", selected = selected_samples)
      }
    })

    # Observe selectInput changes to manage annotations on the plot
    observeEvent(input$sample_selector, {
      req(input$sample_selector, current_tsne_model())
      df <- current_tsne_model()$df.tsne

      # Gather annotations for each selected sample
      new_annotations <- lapply(input$sample_selector, function(sample) {
        point <- df %>% filter(sample == !!sample)
        list(
          x = point$tsne.dim1,
          y = point$tsne.dim2,
          text = paste("Sample:", sample),
          showarrow = TRUE,
          arrowhead = 7
        )
      })

      # Update stored annotations and apply to the plot
      annotations(new_annotations)
      plotlyProxy("current_tsne_plot", session) %>%
        plotlyProxyInvoke("relayout", list(annotations = new_annotations))
    })
  })
}
