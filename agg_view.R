aggViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, uiOutput(ns("x_axis_ui"))),
      column(4, radioButtons(inputId = ns("split_view"),
                             label = "View second variable by: ",
                             choices = c("Facet", "Stack"),
                             selected = "Stack")),
      column(4, 
             conditionalPanel(condition = sprintf("input['%s'] == 'Stack'", ns("split_view")),
                              uiOutput(ns("fill_value_ui")))),
      column(4, 
             conditionalPanel(condition = sprintf("input['%s'] == 'Facet'", ns("split_view")),
                              uiOutput(ns("facet_value_ui"))))
    ),
    fluidRow(
      conditionalPanel(condition = sprintf("input['%s'] == 'Facet'", ns("split_view")),
                       column(4, uiOutput(ns("facet_subset_ui")), offset = 8)),
      conditionalPanel(condition = sprintf("input['%s'] == 'Stack'", ns("split_view")),
                       column(4,
                              radioButtons(inputId = ns("prop_raw_chooser"),
                                           label = "Choose type of graph",
                                           choices = c("Proportional", "Number of Isolates"),
                                           selected = "Number of Isolates"),
                              offset = 8))
    ),
    fluidRow(
      plotOutput(outputId = ns("cgftype_plot"))
    ),
    fluidRow(
      DT::dataTableOutput(outputId = ns("data_table"))
    )
  )
}

aggView <- function(input, output, session, data_for_plots) {
  
  re_x_axis <- reactive({input$x_axis})
  re_fill <- reactive({input$fill_val})
  re_facet <- reactive({input$facet_val})
  re_facet_subset <- reactive({input$facet_subset})
  re_split_by <- reactive({input$split_view})
  re_fill_graph_by <- reactive({input$prop_raw_chooser})
  
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
                label = "Fill by:",
                choices = colnames(data_for_plots()),
                selected = re_x_axis())
  })
  
  output$facet_value_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("facet_val"),
                label = "Facet by:",
                choices = colnames(data_for_plots()),
                selected = re_x_axis())
  })
  
  output$facet_subset_ui <- renderUI({
    ns <- session$ns
    subset_db <- data_for_plots()[, re_facet()]
    print('subset db')
    print(unique(subset_db[[re_facet()]]))
    
    pickerInput(inputId = ns("facet_subset"),
                label = "Facet subsets:",
                choices = unique(subset_db[[re_facet()]]),
                options = list('actions-box' = TRUE),
                select = unique(subset_db[[re_facet()]]),
                multiple = TRUE)
  })
  
  re_stacked_plot <- reactive({
    ggplot(re_cgf_count_df(), aes(x = re_cgf_count_df()[[re_x_axis()]])) + 
      geom_bar(aes(fill = re_cgf_count_df()[[re_fill()]])) + 
      labs(x = re_x_axis(), y = "Number of Isolates", fill = re_fill()) + 
      ggtitle(paste("Number of Isolates vs", re_x_axis())) 
  })  
  
  re_prop_plot <- reactive({
    
    ggplot(re_cgf_count_df(), aes(x = re_cgf_count_df()[[re_x_axis()]], fill = re_cgf_count_df()[[re_fill()]])) + 
      geom_bar(position = "fill") +
      # geom_bar(aes(fill = re_cgf_count_df()[[re_fill()]])) +
      labs(x = re_x_axis(), y = "Proportion of Isolates", fill = re_fill()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(paste("Proportion of Isolates vs", re_x_axis()))
  })
  
  re_facet_df <- reactive({
    filtered_df_facet <- re_cgf_count_df()
    columns_facet <- filtered_df_facet[, colnames(filtered_df_facet) == re_facet()]
    facet_df <- filtered_df_facet[columns_facet %in% re_facet_subset(), ]
    facet_df
  })
  
  re_facetted_plot <- reactive({
    facet_df <- re_facet_df() %>%
      mutate_all(as.character)
    print('re facet df')
    print(facet_df)
    print(re_facet_df())
    
    ggplot(facet_df, aes(x = facet_df[[re_x_axis()]])) + 
      geom_bar(aes(fill = facet_df[[re_x_axis()]])) + 
      labs(x = re_x_axis(), y = "Number of Isolates", fill = re_x_axis()) + 
      ggtitle(paste("Number of Isolates vs", re_x_axis())) +
      facet_wrap(as.formula(paste("~", re_facet())))
  })
  
  output$cgftype_plot <- renderPlot({
    if (re_split_by() == "Stack") {
      if (re_fill_graph_by() == "Number of Isolates") {
        plot(re_stacked_plot())
      }
      else if (re_fill_graph_by() == "Proportional") {
        plot(re_prop_plot())
      }
    }
    else if (re_split_by() == "Facet") {
      plot(re_facetted_plot())
    }
  })
  
  output$data_table <- DT::renderDataTable({
    if(re_split_by() == "Stack") {
      table_df <- re_cgf_count_df() %>% group_by_(re_x_axis()) %>% mutate(total_all_x = n()) %>% select_(re_x_axis(), re_fill(), "total_all_x")
      table_df <- unique(table_df %>% group_by_(re_x_axis(), re_fill()) %>% mutate(sub_total = n()) %>% mutate(prop = round(sub_total/total_all_x, digits = 4)))
      table_df <- as.data.frame(table_df[c(re_x_axis(), re_fill(), "sub_total", "total_all_x", "prop")])
      table_df <- table_df[order( table_df[,1]),]
      DT::datatable(table_df, options = list(scrollX = TRUE))
    }
    else if (re_split_by() == "Facet"){
      table_df <- re_facet_df() %>% group_by_(re_x_axis(), re_facet()) %>% summarise(total = n())
      DT::datatable(table_df, options = list(scrollX = TRUE))
    }
  })
}

