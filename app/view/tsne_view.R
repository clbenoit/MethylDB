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
                            choices = c("class", "subclass", "cohort"),
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

      # print(appData$data$current_samples_dataframe$sample)
      # print(utils::head(BValsC))
      BValsC <- BValsC[,(colnames(BValsC) %in% appData$data$current_samples_dataframe$sample)]

      print("ada")

      BValsC_V2 <- appData$data$BValsC_V2
      #print(utils::head(BValsC_V2))

      #rownames(BValsC_V2) <- BValsC_V2$cgID
      #BValsC_V2$cgID <- NULL
      #BValsC_V2$Row.names <- NULL
      #print(utils::head(BValsC_V2))
      #print(utils::head(BValsC_V2))

      BValsC_V2 <- BValsC_V2[,(colnames(BValsC_V2) %in% appData$data$current_samples_dataframe$sample)]

      # print(utils::head(BValsC))
      # print(utils::head(BValsC_V2))
      # print(ncol(BValsC))
      # print(ncol(BValsC_V2))
      print("cardano")

      #merged_BValsC <- merge(BValsC, BValsC_V2, by = 0, all = TRUE)
      #merge(as.F(BValsC), as.data.table(BValsC_V2), by = 0, all = TRUE)
      #save(file = "/home/ptngs/test.rda", list = c("BValsC", "BValsC_V2", "merged_BValsC"))
      #save(file = "/home/ptngs/test.rda", list = c("BValsC", "BValsC_V2"))

      # head(BValsC)
      # head(BValsC_V2)
      # head(merged_BValsC)
      # nrow(BValsC)
      # nrow(BValsC_V2)
      # nrow(merged_BValsC)
      #
      # a <- BValsC
      # b <- BValsC_V2
      data.table::setDT(BValsC, keep.rownames = TRUE)
      data.table::setDT(BValsC_V2, keep.rownames = TRUE)
      merged_BValsC <- as.data.frame(merge(BValsC, BValsC_V2, by = "rn", all = TRUE))
      rownames(merged_BValsC) <- merged_BValsC$rn
      merged_BValsC$rn <- NULL

      # Perform the merge
     # merged_BValsC <- merge(BValsC, BValsC_V2, by = "rn", all = TRUE)

      #print(utils::head(merged_BValsC))
      # rownames(merged_BValsC) <- merged_BValsC$Row.names
      # merged_BValsC$Row.names <- NULL
      #rownames(metas) <- betas
      #print(utils::head(merged_BValsC))

      # only_in_BValsC <- setdiff(BValsC$rn, BValsC_V2$rn)
      # only_in_BValsC_V2 <- setdiff(BValsC_V2$rn, BValsC$rn)
      # length(only_in_BValsC)  # Count of unique keys in BValsC
      # length(only_in_BValsC_V2)  # Count of unique keys in BValsC_V2
      # unmatched_BValsC <- BValsC[rn %in% only_in_BValsC]
      # unmatched_BValsC_V2 <- BValsC_V2[rn %in% only_in_BValsC_V2]

      print(utils::head(merged_BValsC))
      save(file = "/home/ptngs/test.rda", list = c("BValsC", "BValsC_V2","merged_BValsC"))

      selected_variables <- selectVariables(data = na.omit(merged_BValsC), method = "SD", no.variables = 25000, threads = 13)
      #print("selected variables")
      #print(utils::head(selected_variables))
      return(selected_variables)
      #print("a")
      #selectVariables(data = na.omit(merged_BValsC), method = "SD", no.variables = 25000, threads = 13)

    }) #%>% bindCache(list(appData$data$BValsC, appData$data$BValsC_V2, appData$data$current_samples_dataframe))

    # Reactive for current t-SNE model
    current_tsne_model <- reactive({
      req(current_tsne_dataframe())
      print("modelTsne")
      print(utils::head(current_tsne_dataframe()))
      model <- modelTsne(current_tsne_dataframe(), perplexity = 1, dims = 2)
      #print(names(model))
      #print(utils::head(model))
      #print(utils::head(model$df.tsne))
      #print("model computed")
      return(model)
    }) #%>% bindCache(current_tsne_dataframe())

    # Render t-SNE plot
    output$current_tsne_plot <- renderPlotly({
      req(current_tsne_model(), appData$data$current_samples_dataframe, input$color_by)
      df <- current_tsne_model()$df.tsne
      #df <- current_tsne_model()

      # print(utils::head(df))
      # print(utils::head(appData$data$current_samples_dataframe))
      # print(nrow(df))
      # print(nrow(appData$data$current_samples_dataframe))
      #color <- appData$data$current_samples_dataframe[, input$color_by]

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
