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
  plotly[plotlyOutput, renderPlotly, layout, plot_ly],
  htmlwidgets[onRender],
)

box::use(
  app/logic/selectVariables[selectVariables],
  app/logic/modelTsne[modelTsne],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
            div(style = "width: 100%; text-align: center;", # Center-align the radio buttons
            radioButtons(ns("color_by") , label = "color TSNE by" ,
                            inline = TRUE,
                            choices = c("class", "subclass", "cohort"),
                            width = "100%")),
      ), br(),
      column(width = 12,
             plotlyOutput(ns("current_tsne_plot"), height = "800px"))
    )
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    current_tsne_dataframe <- reactive({
      req(appData$data$BValsC)
      req(appData$data$current_samples_dataframe)
      print("selecting variables...")
      BValsC <- appData$BValsC
      rownames(BValsC) <- BValsC$cgID
      BValsC$cgId <- NULL
      BValsC <- appData$data$BValsC[,appData$data$current_samples_dataframe$sample]

      puce_samples <- colnames(BValsC)

      meth <- selectVariables(data = na.omit(BValsC), method = "SD", no.variables = 25000, threads = 13)

      save(list = c("meth", "BValsC"), file = "~/meth.rda")

      return(meth)
    }) %>% bindCache({list(appData$data$BValsC, appData$data$current_samples_dataframe)})


    current_tsne_model <- reactive({
      req(current_tsne_dataframe())
      print("buidling t sne model...")
      print(utils::head(current_tsne_dataframe()))
      tsne2 <- modelTsne(current_tsne_dataframe(),
                         #group = "cohort",
                         perplexity = 1, dims = 2)
      return(tsne2)
    }) %>% bindCache(current_tsne_dataframe())

    output$current_tsne_plot <- renderPlotly({
      req(current_tsne_model())
      req(appData$data$current_samples_dataframe)
      req(input$color_by)
      print("rendering t sne plot...")

      df <- current_tsne_model()$df.tsne
      return(plot_ly(x = df$tsne.dim1,
                  y = df$tsne.dim2,
                  type = "scatter",
                  mode = "markers",
                  color = appData$data$current_samples_dataframe[,input$color_by],
                  text = ~paste("Sample: ", df$sample),
                  hoverinfo = 'text',  # Only display the custom text (no x/y info)
                  marker = list(size = 10),
                  # marker = list(size = ~ifelse(df$is_special, 8, 6),  # Larger points for special samples
                  #               symbol = ~ifelse(df$is_special, 'x', 'circle'))
                  ) %>%
        layout(
          title = "2D t-SNE",
          xaxis = list(title = "t-SNE Dimension 1", zeroline = FALSE),
          yaxis = list(title = "t-SNE Dimension 2", zeroline = FALSE)
        )
    %>%
        onRender("
      function(el, x) {
        el.on('plotly_click', function(d) {
          var sampleName = d.points[0].text.split(': ')[1];  // Extract sample name from hover text
        // Update the layout to show an annotation with the sample name
        Plotly.relayout(el, {
          annotations: [{
            x: d.points[0].x,
            y: d.points[0].y,
            text: 'Sample: ' + sampleName,
            showarrow: true,
            arrowhead: 7
            }]
          });
        });
      }
    ")
        )
        }) %>% bindCache(list(current_tsne_model(),
                              appData$data$current_samples_dataframe,
                              input$color_by))

  })
}

