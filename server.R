
#ASK:
# Currently my rank is integer based and doesn't have '.5' if there is >1 with that particular # of isolates. Still correctly ranks them though

# options(shiny.maxRequestSize = 10*1024^2)

filter_cgftype <- function(db, cgftype) {
  temp <- db %>%
    filter(cgftype %in% db.cgf_type)
}

get_order_stacked_bar_per_cgftype <- function(data) {
  stats <- data %>%
    group_by(cgf.type, source.specific_1) %>%
    summarise(type_count = n()) %>%
    mutate(prop = type_count/sum(type_count))
  all_types <- as_data_frame(stats$cgf.type) %>% group_by(cgf.type=stats$cgf.type) %>% summarise()
  order_types <- as_data_frame(x=list(cgf.type = stats$cgf.type, source.specific_1 = stats$source.specific_1, prop = stats$prop)) %>%
    filter(source.specific_1 == "Chicken" | source.specific_1 == "Human")
  merged_types <- merge(x=all_types, y=order_types, by.x="cgf.type", by.y = "cgf.type", all=TRUE)
  max_type <- merged_types %>%
    group_by(cgf.type) %>%
    summarise(max = max(prop))
  merged_type_order <- merge(x=merged_types, y=max_type, by.x=c("prop", "cgf.type"), by.y=c("max","cgf.type"))
  order <- merged_type_order %>% arrange(source.specific_1, -prop)
}


#Stacked Barplot
# stacked_levels = c("Human", "Buffalo", "Cow", "Horse", "Raccoon", "Sheep", "Skunk", "Turkey", "Water", "Chicken")

# server
server <- function(input, output, session) {
  
  subset_mod_list <- callModule(subsetData, "sidebar")
  data_for_plots <- reactive({subset_mod_list$subset_data()})
  data_inputted <- reactive({subset_mod_list$data_input()})
  # data_for_plots <- callModule(subsetData, "sidebar")

  #TAB1
  re_view_type    <- reactive({input$view_type_in})
  re_col          <- reactive({input$col_display})
  re_agg_by       <- reactive({input$cast})
  
  output$column_display_ui <- renderUI({
    if (re_view_type() == "aggregate") {
      init_sel <- c("cgf.type")
    } else {
      init_sel <- c("strain.name", "cgf.type", "typing.lab", "sample.origin", "source.specific_1")
    }
    
    pickerInput(inputId = "col_display",
                label = "Choose columns to display:",
                options = list('actions-box' = TRUE),
                choices = colnames(data_for_plots()),
                selected = init_sel,
                multiple = TRUE)
  })
  
  output$agg_by <- renderUI({
    pickerInput(inputId = "cast",
                label = "Choose variables to view in wide format:",
                options = list('actions-box' = TRUE),
                choices = colnames(data_for_plots()),
                selected = c("source.specific_1"),
                multiple = TRUE)
  })
  
  output$table <- DT::renderDataTable({
    if (re_view_type() == "strain") {
      data <- data_for_plots() %>% select(re_col())
      DT::datatable(data, options = list(scrollX = TRUE))
    }
    else if (re_view_type() == "aggregate") {
      LHS_cast_col <- paste(re_col(), collapse = " + ")
      RHS_cast_col <- paste(re_agg_by(), collapse = " + ")
    
      data <- dcast(data_for_plots(), paste(LHS_cast_col, "~", RHS_cast_col, sep=""), length)
      if (nrow(data) > 1) {
        data[(nrow(data) + 1), which(unlist(lapply(data, is.numeric)))] <- colSums(data[,which(unlist(lapply(data, is.numeric)))], na.rm=TRUE)
        data[nrow(data), "cgf.type"] = "ALL"
      }
      DT::datatable(data, options = list(scrollX = TRUE))
    }
  })
  
  
  #TAB4
  # re_fill_chosen <- reactive({input$fill_chosen})
  # 
  # output$fill_value_ui <- renderUI({
  #   selectInput(inputId = "fill_chosen",
  #               label = "Fill Value:",
  #               choices = colnames(data_for_plots()),
  #               selected = "typing.lab")
  # })
  # 
  # 
  # output$cgftypes_plot <- renderPlot({
    # bar_plot <- ggplot(data_for_plots(), aes(x=cgf.type)) +
    #   geom_bar(aes(fill = data_for_plots()[[re_fill_chosen()]])) +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   labs(x = "CGF Type", y = "Number of Isolates") +
    #   ggtitle("Number of Isolates vs CGF Types") +
    #   scale_fill_discrete(name = re_fill_chosen())
    # plot(bar_plot)
  # })
  
  #TAB5
  # re_cgf_type <- reactive({input$cgf_type_in})
  # re_cgf_count_df <- reactive({
  #   tempdf <- data_for_plots() %>% group_by(cgf.type) %>% summarise(count = n())
  #   merge(tempdf, data_for_plots(), by.x = "cgf.type", by.y = "cgf.type")
  # })
  # 
  # re_x_axis <- reactive({input$x_axis})
  # output$x_axis_ui <- renderUI({
  #   selectInput(inputId = "x_axis",
  #               label = "X-axis:",
  #               choices = colnames(data_for_plots()),
  #               selected = "typing.lab")
  # })
  # 
  # output$cgftype_plot <- renderPlot({
  #   bar_plot <- ggplot(re_cgf_count_df(), aes(x = re_cgf_count_df()[[re_x_axis()]])) + 
  #     geom_bar(aes(fill = re_cgf_count_df()[[re_x_axis()]])) + 
  #     labs(x = re_x_axis(), y = "Number of Isolates") + 
  #     ggtitle(paste("Number of Isolates vs", re_x_axis())) 
  #   plot(bar_plot)
  # })
  
  callModule(aggView, "agg_view", data_for_plots)
  
  # TAB 6: Prop of Sources
  re_graph_type <- reactive({input$graph_type})
  
  observe({
    if (re_graph_type() == 'Frequency of isolates shown using bar chart') {
      callModule(barChartStackogram, "bar_graph_all", data_for_plots, data_inputted)
    }
    else {
      callModule(propBarWidth, "wide_graph_all", data_for_plots, data_inputted)
    }
  })
  
  #Tab 7 : Bubble source vs sample plot  
  callModule(riskogram, "riskogram", data_for_plots)
  
  #Tab 8 
  callModule(attrEstStackogram, "attr_est", data_for_plots, data_inputted)

}
