
#ASK:
# Currently my rank is integer based and doesn't have '.5' if there is >1 with that particular # of isolates. Still correctly ranks them though

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
stacked_levels = c("Human", "Buffalo", "Cow", "Horse", "Raccoon", "Sheep", "Skunk", "Turkey", "Water", "Chicken")

# server
server <- function(input, output, session) {
  
  data_for_plots <- callModule(subsetData, "sidebar")

  #TAB1
  re_view_type    <- reactive({input$view_type_in})
  re_col          <- reactive({input$col_display})
  re_agg_by       <- reactive({input$agg_by})
  
  output$column_display_ui <- renderUI({
    pickerInput(inputId = "col_display",
                label = "Choose columns to display:",
                options = list('actions-box' = TRUE),
                choices = colnames(data_for_plots()),
                selected = c("strain.name", "cgf.type", "typing.lab", "sample.origin", "source.specific_1"),
                multiple = TRUE)
  })

  observeEvent(re_view_type(), {
    if (re_view_type() == "aggregate") {
      updatePickerInput(session,
                      inputId = "col_display",
                      selected = c("cgf.type"))
    }
    else {
      updatePickerInput(session,
                        inputId = "col_display",
                        selected = c("strain.name", "cgf.type", "typing.lab", "sample.origin", "source.specific_1"))
    }
  })
  
  output$table <- DT::renderDataTable({
    if (re_view_type() == "strain") {
      data <- data_for_plots() %>% select(re_col())
      DT::datatable(data, options = list(scrollX = TRUE))
    }
    else if (re_view_type() == "aggregate") {
      cast_col <- paste(test, collapse = " + ")
      data <- dcast(data_for_plots(), paste(cast_col, "~", "c(province, typing.lab)", sep=""), length)
      data[(nrow(data) + 1), which(unlist(lapply(data, is.numeric)))] <- colSums(data[,which(unlist(lapply(data, is.numeric)))], na.rm=TRUE)
      data[nrow(data), "cgf.type"] = "ALL"
      DT::datatable(data, options = list(scrollX = TRUE))
    }
  })
  
  
  #TAB4
  fillChosen <- reactive({input$fill_chosen})
  fillInput <- reactive({
    switch(fillChosen(),
           "typing.lab" = data_for_plots()$typing.lab,
           "sample.origin" = data_for_plots()$sample.origin,
           "sample.type" = data_for_plots()$sample.type,
           "source.general" = data_for_plots()$source.general,
           "source.specific_1" = data_for_plots()$source.specific_1,
           "province" = data_for_plots()$province
           )
  })
  
  output$cgftypes_plot <- renderPlot({
    bar_plot <- ggplot(data_for_plots(), aes(x=cgf.type)) +
      geom_bar(aes(fill = fillInput())) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "CGF Type", y = "Number of Isolates") + 
      ggtitle("Number of Isolates vs CGF Types") + 
      scale_fill_discrete(name = fillChosen())
    plot(bar_plot)
  })
  
  #TAB5
  re_cgf_type <- reactive({input$cgf_type_in})
  re_cgf_count_df <- reactive({
    tempdf <- data_for_plots() %>% group_by(cgf.type) %>% summarise(count = n())
    merge(tempdf, data_for_plots(), by.x = "cgf.type", by.y = "cgf.type")
  })
  
  re_x_axis <- reactive({input$x_axis})
  xaxisInput <- reactive({
    switch(re_x_axis(),
           "typing.lab" = data_for_plots()$typing.lab,
           "sample.origin" = data_for_plots()$sample.origin,
           "sample.type" = data_for_plots()$sample.type,
           "source.general" = data_for_plots()$source.general,
           "source.specific_1" = data_for_plots()$source.specific_1,
           "province" = data_for_plots()$province)
  })
  
  output$cgftype_plot <- renderPlot({
    bar_plot <- ggplot(re_cgf_count_df(), aes(x = xaxisInput())) + 
      geom_bar(aes(fill = xaxisInput())) + 
      labs(x = re_x_axis(), y = "Number of Isolates") + 
      ggtitle(paste("Number of Isolates vs", re_x_axis())) 
    plot(bar_plot)
  })
  
  # TAB 6: Prop of Sources
  re_graph_type <- reactive({input$graph_type})
  
  observe({
    if (re_graph_type() == 'Frequency of isolates shown using bar chart') {
      callModule(barChartStackogram, "bar_graph_all", data_for_plots)
    }
    else {
      print(data_for_plots())
      callModule(propBarWidth, "wide_graph_all", data_for_plots)
    }
  })
  
  #Tab 7 : Bubble source vs sample plot  
  callModule(riskogram, "riskogram", data_for_plots)
  
  #Tab 8 
  callModule(attrEstStackogram, "attr_est", data_for_plots)

}
