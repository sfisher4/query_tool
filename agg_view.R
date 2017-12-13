aggViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # headerPanel("Bar Graph of Aggregated CGFTypes"),
      column(4, uiOutput(ns("x_axis_ui"))),
      column(4, radioButtons(inputId = ns("split_view"),
                             label = "View second variable by: ",
                             choices = c("Facet", "Stack", "None"),
                             selected = "Stack")),
      column(4, uiOutput(ns("fill_value_ui")))
    ),
    fluidRow(
      plotOutput(outputId = ns("cgftype_plot"))
    )
  )
}

aggView <- function(input, output, session, data_for_plots) {
  
  re_x_axis <- reactive({input$x_axis})
  re_fill <- reactive({input$fill_val})
  
  re_cgf_count_df <- reactive({
    tempdf <- data_for_plots() %>% group_by(cgf.type) %>% summarise(count = n())
    merge(tempdf, data_for_plots(), by.x = "cgf.type", by.y = "cgf.type")
  })
  
  output$x_axis_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("x_axis"),
                label = "X-axis:",
                choices = colnames(data_for_plots()),
                selected = "typing.lab")
  })
  
  output$fill_value_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("fill_val"),
                label = "X-axis:",
                choices = colnames(data_for_plots()),
                selected = re_x_axis())
  })
  
  re_stacked_plot <- reactive({
    ggplot(re_cgf_count_df(), aes(x = re_cgf_count_df()[[re_x_axis()]])) + 
      geom_bar(aes(fill = re_cgf_count_df()[[re_fill()]])) + 
      labs(x = re_x_axis(), y = "Number of Isolates") + 
      ggtitle(paste("Number of Isolates vs", re_x_axis())) 
  })
  
  re_facetted_plot <- reactive({
    ggplot(re_cgf_count_df(), aes(x = re_cgf_count_df()[[re_x_axis()]])) + 
      geom_bar(aes(fill = re_cgf_count_df()[[re_x_axis()]])) + 
      labs(x = re_x_axis(), y = "Number of Isolates") + 
      ggtitle(paste("Number of Isolates vs", re_x_axis())) +
      facet()
  })
  
  output$cgftype_plot <- renderPlot({
    if (re_fill == "Stack") {
      plot(re_stacked_plot())
    }
    else {
      plot(re_facetted_plot())
    }
  })
}

