#app/view/tsne_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel,
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateTabsetPanel,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent,
        updateSelectizeInput, fluidPage, bindCache, reactive,
        observe, reactiveValues, bindEvent, isolate],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange, inner_join, rename],
  reactable,
  stats[na.omit],
  plotly[plotlyOutput, renderPlotly, layout, plot_ly],
  htmlwidgets[onRender],
)

box::use(
  app/logic/selectVariables[selectVariables],
  app/logic/modelTsne[modelTsne],
  #app/view/tsne_view,
  #app/view/select_samples,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
        reactable$reactableOutput(ns("annotations"))
      ),
      column(width = 12,
             plotlyOutput(ns("current_tsne_plot")))
      #shiny::dataTableOutput(ns("test"))
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
      BValsC <- appData$data$BValsC[,appData$data$current_samples_dataframe$sample] #%>%
        #filter()

      puce_samples <- colnames(BValsC)
      #betas <- merge(BValsC ,betas, by=0, all=TRUE)

      meth <- selectVariables(data = na.omit(BValsC), method = "SD", no.variables = 25000, threads = 13)

      save(list = c("meth", "BValsC"), file = "~/meth.rda")
      #print(utils::head(meth))

      return(meth)
    }) #%>% bindCache({list()})


    current_tsne_model <- reactive({
      req(current_tsne_dataframe())
      print("buidling t sne model...")
      print(utils::head(current_tsne_dataframe()))
      tsne2 <- modelTsne(current_tsne_dataframe(),
                         #group = "cohort",
                         perplexity = 1, dims = 2)
      return(tsne2)
    })

    output$current_tsne_plot <- renderPlotly({
      req(current_tsne_model())
      req(appData$data$current_samples_dataframe)
      print("rendering t sne plot...")

      df <- current_tsne_model()$df.tsne
      print(utils::head(current_tsne_model()$df.tsne))
      # if(length(unique(appData$data$current_samples_dataframe$cohort)) >=2){
      #   print("oki")
      #   #col.palette <- scales::hue_pal(unique(appData$data$current_samples_dataframe$cohort))
      #   #print(unique(appData$data$current_samples_dataframe$cohort))
      #   #print(col.palette)
      #   print("doki")
        return(plot_ly(x = df$tsne.dim1,
                  y = df$tsne.dim2,
                  type = "scatter",
                  mode = "markers",
                  color = appData$data$current_samples_dataframe$cohort,
                  text = ~paste("Sample: ", df$sample),
                  hoverinfo = 'text',  # Only display the custom text (no x/y info)
                  #colors = col.palette,
                  # marker = list(size = ~ifelse(df$is_special, 8, 6),  # Larger points for special samples
                  #               symbol = ~ifelse(df$is_special, 'x', 'circle'))
                  ) %>%
        layout(
          title = "2D t-SNE",
          xaxis = list(title = "t-SNE Dimension 1", zeroline = FALSE),
          yaxis = list(title = "t-SNE Dimension 2", zeroline = FALSE)
        )
       # %>%
    #     onRender("
    #   function(el, x) {
    #     el.on('plotly_click', function(d) {
    #       var sampleName = d.points[0].text.split(': ')[1];  // Extract sample name from hover text
    #     // Update the layout to show an annotation with the sample name
    #     Plotly.relayout(el, {
    #       annotations: [{
    #         x: d.points[0].x,
    #         y: d.points[0].y,
    #         text: 'Sample: ' + sampleName,
    #         showarrow: true,
    #         arrowhead: 7
    #         }]
    #       });
    #     });
    #   }
    # ")
        )
    # } else {
    #   return(plot_ly(x = df$tsne.dim1,
    #           y = df$tsne.dim2,
    #           type = "scatter",
    #           mode = "markers",
    #           text = ~paste("Sample: ", df$sample),
    #           hoverinfo = 'text',  # Only display the custom text (no x/y info)
    #           # marker = list(size = ~ifelse(df$is_special, 8, 6),  # Larger points for special samples
    #           #               symbol = ~ifelse(df$is_special, 'x', 'circle'))
    #           ) %>%
    #     layout(
    #       title = "2D t-SNE",
    #       xaxis = list(title = "t-SNE Dimension 1", zeroline = FALSE),
    #       yaxis = list(title = "t-SNE Dimension 2", zeroline = FALSE)
    #     )
    #     %>%
    #     onRender("
    #   function(el, x) {
    #     el.on('plotly_click', function(d) {
    #       var sampleName = d.points[0].text.split(': ')[1];  // Extract sample name from hover text
    #     // Update the layout to show an annotation with the sample name
    #     Plotly.relayout(el, {
    #       annotations: [{
    #         x: d.points[0].x,
    #         y: d.points[0].y,
    #         text: 'Sample: ' + sampleName,
    #         showarrow: true,
    #         arrowhead: 7
    #         }]
    #       });
    #     });
    #   }
    # ")
        #)
    #}


        })

    #output$test <- shiny::renderDataTable(current_tsne_dataframe())

  })
}

