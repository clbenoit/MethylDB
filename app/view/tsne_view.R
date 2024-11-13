#app/view/tsne_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel,
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateTabsetPanel,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent,
        updateSelectizeInput, fluidPage, bindCache, reactive, updateSelectInput,
        observe, reactiveValues, bindEvent, isolate, radioButtons, div],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange, inner_join, rename],
  reactable,
  stats[na.omit],
  plotly[plotlyOutput, renderPlotly, layout, plot_ly,
         plotlyProxy, plotlyProxyInvoke],
  htmlwidgets[onRender],
  bslib[card, card_header, card_body, value_box, layout_column_wrap]
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
    br(),
    card(id = "cardcustomizeTsne", width = 12, full_screen = FALSE, min_height = '250px',
         card_header("Select samples to compute T SNE"),
         card_body(
           fluidRow(
             layout_column_wrap(
               width = 1/2,
               radioButtons(ns("color_by"), label = "Color TSNE by",
                            inline = TRUE,
                            choices = c("class", "subclass", "cohort", "chip"),
                            width = "100%"),
               selectInput(ns("sample_selector"), "Select Samples to annotate on graph",
                           choices = NULL, width = '100%',
                           multiple = TRUE),
             )
           ))),
      br(),
      # column(width = 12,
      #        plotlyOutput(ns("current_tsne_plot"), height = "800px"))
    conditionalPanel(
      condition = sprintf("output['%s'] < 4", ns("sample_count")),
      column(width = 12,
             tags$div("Not enough samples to render the t-SNE plot. Please select at least 4 samples.",
                      style = "color: red; font-weight: bold; text-align: center;"))
    ),
    # Conditional Panel to show the plot if there are at least 4 samples
    conditionalPanel(
      condition = sprintf("output['%s'] >= 4", ns("sample_count")),
      column(width = 12,
             plotlyOutput(ns("current_tsne_plot"), height = "800px"))
    )
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    annotations <- shiny::reactiveVal(list())  # Store plot annotations

    output$sample_count <- reactive({
      req(appData$data$current_samples_dataframe)
      #print(nrow(appData$data$current_samples_dataframe))
      #print(utils::head(appData$data$current_samples_dataframe))

      return(nrow(appData$data$current_samples_dataframe))
    })
    shiny::outputOptions(output, "sample_count", suspendWhenHidden = FALSE)

    # Update selectInput choices dynamically
    observeEvent(appData$data$current_samples_dataframe$sample, {
      updateSelectInput(session, inputId = "sample_selector",
                               choices = appData$data$current_samples_dataframe$sample)
    })

    # Reactive for current t-SNE data frame
    current_tsne_dataframe <- reactive({
      req(appData$data$BValsC, appData$data$current_samples_dataframe, appData$data$BValsC_V2)

      BValsC <- appData$data$BValsC
      rownames(BValsC) <- BValsC$cgID
      BValsC$cgId <- NULL

      BValsC <- BValsC[,(colnames(BValsC) %in% appData$data$current_samples_dataframe$sample)]

      BValsC_V2 <- appData$data$BValsC_V2

      BValsC_V2 <- BValsC_V2[,(colnames(BValsC_V2) %in% appData$data$current_samples_dataframe$sample)]

      data.table::setDT(BValsC, keep.rownames = TRUE)
      data.table::setDT(BValsC_V2, keep.rownames = TRUE)
      merged_BValsC <- as.data.frame(merge(BValsC, BValsC_V2, by = "rn", all = TRUE))
      rownames(merged_BValsC) <- merged_BValsC$rn
      merged_BValsC$rn <- NULL

      selected_variables <- selectVariables(data = na.omit(merged_BValsC), method = "SD", no.variables = 25000, threads = 13)
      return(selected_variables)

    }) #%>% bindCache(list(appData$data$BValsC, appData$data$BValsC_V2, appData$data$current_samples_dataframe))

    # Reactive for current t-SNE model
    current_tsne_model <- reactive({
      req(current_tsne_dataframe())
      print("modelTsne")
      model <- modelTsne(current_tsne_dataframe(), perplexity = 1, dims = 2)
      return(model)
    }) #%>% bindCache(current_tsne_dataframe())

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
        updateSelectInput(session, "sample_selector", selected = selected_samples)
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
