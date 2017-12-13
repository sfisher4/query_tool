
riskogramUI <- function(id){
  ns <- NS(id)
  
  tagList(
    titlePanel("Risk Assessment for CGF Types"),
    fluidRow(
      column(4,
             selectInput(inputId = ns("var_cat"),
                         label = "Category of Variables:",
                         choices = NULL)
      ),
      column(4,
             selectInput(inputId = ns("x_axis_var1"),
                         label= "X-axis Variable #1:",
                         choices = NULL)
      ),
      column(4,
             selectInput(inputId = ns("x_axis_var2"),
                         label= "X-axis Variable #2:",
                         choices = NULL)
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = ns("facet_choice"),
                           label = "Facet?",
                           value = FALSE)
      ),
      column(4,
             uiOutput(ns("facet_by_selector"))),
      column(4,
             uiOutput(ns("subset_view_selector")))
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = ns("aggregate"),
                           label = "Show aggregate Data",
                           value = FALSE)),
      column(4,
             checkboxInput(inputId = ns("bubble"),
                           label = "Size of Point = # of Isolates",
                           value = TRUE))
    ),
    fluidRow(
      column(12,
             plotlyOutput(ns("plot")))
    ),
    fluidRow(
      column(4,
             "'Refresh Query' to activate changes in min/max point size:"
             ),
      
      column(4,
             sliderInput(inputId = ns("min_slider"),
                         label = "Min Size of Points ",
                         min = 1,
                         max = 50,
                         value = 1)),
      column(4,
             sliderInput(inputId = ns("max_slider"),
                         label = "Max Size of Points ",
                         min = 1,
                         max = 50,
                         value = 8))
    ),
    fluidRow(
      DT::dataTableOutput(outputId = ns("data_table"))
    )
  )
}

riskogram <- function(input, output, session, data_for_plots) {
  
  r_var_type <- reactive({input$var_cat})
  r_var1 <- reactive({input$x_axis_var1})
  r_var2 <- reactive({input$x_axis_var2})
  r_facet_by <- reactive({input$facet_by})
  re_subset_facet <- reactive({input$subset_view})
  r_facet_choice <- reactive({input$facet_choice})
  r_aggregate <- reactive({input$aggregate})
  r_size <- reactive({input$bubble})
  r_min_size <- reactive({input$min_slider})
  
  # test <- cgfdb_testset2 %>% group_by(cgf.type, source.specific_1, province) %>% summarise(count = n())
  # test <- dcast(test, cgf.type + province ~ source.specific_1, value.var = "count")
  # test[is.na(test)] <- 0
  # test <- test %>% mutate(other = rowSums(test[, -c(which(colnames(test) == "cgf.type"), which(colnames(test) == "province"), which(colnames(test) == "Chicken"), which(colnames(test) == "Human"))]))
  # test <- test %>% mutate(total = rowSums(test[, -c(which(colnames(test) == "cgf.type"), which(colnames(test) == "province"), which(colnames(test) == "other"))]))
  # No facets:
  # myNumCols <- which(unlist(lapply(test, is.numeric)))
  # test[(nrow(test) + 1), myNumCols] <- colSums(test[,myNumCols], na.rm=TRUE)
  # With facets:
   # agg_test <- merge(aggregate(Chicken ~ province, data=test, sum), aggregate(Human ~ province, data=test, sum))
   # temp <- merge(aggregate(other ~ province, data=test, sum) , aggregate(total ~ province, data=test, sum))
   # merge(temp, agg_test)
  
  re_cgfdf_facet <- reactive({
    long_df <- data_for_plots() %>%
      group_by_("cgf.type", r_var_type(), r_facet_by()) %>%
      summarise(count = n())
    long_df <- dcast(long_df, paste("cgf.type", "+", r_facet_by(), "~", r_var_type(), sep=""), value.var = "count")
    long_df[is.na(long_df)] <- 0
    cgf_type_index = which(colnames(long_df) == "cgf.type")
    facet_index = which(colnames(long_df) == r_facet_by())
    var1_index = which(colnames(long_df) == r_var1())
    var2_index = which(colnames(long_df) == r_var2())
    long_df <- long_df %>%
      mutate(total_obs = rowSums(long_df[, -c(cgf_type_index, facet_index, which(colnames(long_df) == "other"))])) %>%
      mutate(other = total_obs - (long_df[[r_var1()]] + long_df[[r_var2()]])) 
    if (r_aggregate()) {
      agg1 <- merge(aggregate(formula(paste0(r_var1(), '~', r_facet_by())), data=long_df, sum), aggregate(formula(paste0(r_var2(), '~', r_facet_by())), data=long_df, sum))
      agg2 <- merge(aggregate(formula(paste0("other", '~', r_facet_by())), data=long_df, sum) , aggregate(formula(paste0("total_obs", "~", r_facet_by())), data=long_df, sum))
      long_df <- merge(agg1, agg2)
    }
    
    long_df
  })  
  
  re_cgfdf_no_facet <- reactive({
    long_df <- data_for_plots() %>%
      group_by_("cgf.type", r_var_type()) %>%
      summarise(count = n())
    long_df <- dcast(long_df, paste("cgf.type", "~", r_var_type(), sep=""), value.var = "count")
    long_df[is.na(long_df)] <- 0
    
    cgf_type_index = which(colnames(long_df) == "cgf.type")
    var1_index = which(colnames(long_df) == r_var1())
    var2_index = which(colnames(long_df) == r_var2())
    long_df <- long_df %>%
      mutate(total_obs = rowSums(long_df[, -c(cgf_type_index, which(colnames(long_df) == "other"))])) %>%
      mutate(other = total_obs - (long_df[[r_var1()]] + long_df[[r_var2()]])) 
    
    if (r_aggregate()) {
      num_cols <- which(unlist(lapply(long_df, is.numeric)))
      long_df[(nrow(long_df) + 1), num_cols] <- colSums(long_df[,num_cols], na.rm=TRUE)
      long_df <- tail(long_df, 1)
    }
    
    long_df
  })
  
  observe({
    updateSelectInput(session, 
                      inputId = "var_cat",
                      choices = colnames(data_for_plots()),
                      selected = "source.specific_1")
  })
  observe({
    updateSelectInput(session,
                      inputId = "x_axis_var1",
                      choices = data_for_plots()[[r_var_type()]],
                      selected = "Chicken")
    updateSelectInput(session,
                      inputId = "x_axis_var2",
                      choices = data_for_plots()[[r_var_type()]],
                      selected = "Human")
  })
  
  output$facet_by_selector <- renderUI({
    ns <- session$ns
    if (r_facet_choice()) {
      selectInput(inputId = ns("facet_by"),
                  label = "View by:",
                  choices = c("", colnames(data_for_plots())), #TODO: unless the column name starts with 'cj'
                  selected = "province")
      }
  })

  output$subset_view_selector <- renderUI({
    ns <- session$ns
    if (r_facet_choice()) {
      subset <- re_cgfdf_facet()[, r_facet_by()]
      pickerInput(inputId = ns("subset_view"),
                  label = paste("Select", r_facet_by(), "subset to view: "),
                  choices = unique(subset),
                  options = list('actions-box' = TRUE),
                  select = unique(subset),
                  multiple = TRUE
      )
    }
  })
  
  re_no_facet_plot <- reactive({
    
    x_axis = re_cgfdf_no_facet()[[r_var1()]] / (re_cgfdf_no_facet()[[r_var1()]] + re_cgfdf_no_facet()[[r_var2()]])
    y_axis = re_cgfdf_no_facet()$other / re_cgfdf_no_facet()$total_obs
    
    tooltip_text = paste("Total Obs:", re_cgfdf_no_facet()$total_obs, "\n", 
                         "CGF Type:", re_cgfdf_no_facet()$cgf.type, "\n", 
                         r_var1(), "to", r_var2(), "ratio (x-axis): ", x_axis, "\n",
                         "%", r_var_type(), "not", r_var1(), "or", r_var2(), "(y_axis): ", y_axis)
    if (r_aggregate()) {
      colour_legend = "none"
    }
    else {
      colour_legend = re_cgfdf_no_facet()$cgf.type
    }
    
    if (r_size()) {
      size_legend = re_cgfdf_no_facet()$total_obs
    }
    else {
      size_legend = r_min_size()
    }
    
    ggplot(re_cgfdf_no_facet(), aes(x=x_axis, y=y_axis, text = tooltip_text)) +
      geom_point(aes(size = size_legend, colour = colour_legend), alpha = 0.5) +
      labs(x = paste(r_var1(), "to", r_var2(), "Ratio"), y = paste("Other", r_var_type())) +
      scale_fill_discrete(name="Total Observations of CGF Type") +
      theme_gray() +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
      scale_size_continuous(range = c(r_min_size(), input$max_slider)) +
      guides(size = "none")+
      theme(legend.position = "none")
  })
  
  re_facet_plot <- reactive({
    
    filtered_df_facet <- re_cgfdf_facet()
    columns_facet <- filtered_df_facet[, colnames(filtered_df_facet) == r_facet_by()]
    facet_df <- filtered_df_facet[columns_facet %in% re_subset_facet(), ]
    
    x_axis = facet_df[[r_var1()]] / (facet_df[[r_var1()]] + facet_df[[r_var2()]])
    y_axis = facet_df$other / facet_df$total_obs
    tooltip_text = paste("Total Obs:", facet_df$total_obs, "\n", 
                         "CGF Type:", facet_df$cgf.type, "\n", 
                         r_var1(), "to", r_var2(), "ratio (x-axis): ", x_axis, "\n",
                         "%", r_var_type(), "not", r_var1(), "or", r_var2(), "(y_axis)", y_axis)
    if (r_aggregate()) {
      colour_legend = "blue"
    }
    else {
      colour_legend = facet_df$cgf.type
    }
    
    if (r_size()) {
      size_legend = facet_df$total_obs
    }
    else {
      size_legend = r_min_size()
    }
    
    ggplot(facet_df, aes(x=x_axis, y=y_axis, text = tooltip_text)) +
      geom_point(aes(size = size_legend, colour = colour_legend, alpha = 0.5)) +
      labs(x = paste(r_var1(), "to", r_var2(), "Ratio"), y = paste("Other", r_var_type())) +
      scale_fill_discrete(name="Total Observations of CGF Type") + 
      theme_gray() +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      scale_x_continuous(labels = scales::percent, limits = c(0,100)) +
      scale_size_continuous(range = c(r_min_size(), input$max_slider)) +
      facet_wrap(as.formula(paste("~", r_facet_by()))) +
      guides(size = "none" ) +
      theme(legend.position = "none")
  })
  
  output$plot <- renderPlotly({
    if (r_facet_choice() == 'TRUE') {
      if (r_aggregate()){
        # plot(re_facet_plot())
        ggplotly(re_facet_plot())
      }
      else {
        # plot(re_facet_plot())
        ggplotly(re_facet_plot(), tooltip = "text")
      }
    } 
    else if (r_facet_choice() == 'FALSE') {
      if (r_aggregate()){
        # plot(re_no_facet_plot())
        ggplotly(re_no_facet_plot()) %>% config()
      }
      else {
        # plot(re_no_facet_plot())
        ggplotly(re_no_facet_plot(), tooltip = "text")
      }
    }
  })
  
  output$data_table <- DT::renderDataTable({
    if (r_facet_choice() == 'TRUE') {
      total_obs <- re_cgfdf_facet()$total_obs
      cgf_types <- re_cgfdf_facet()$cgf.type
      x_axis <- re_cgfdf_facet()[[r_var1()]] / (re_cgfdf_facet()[[r_var1()]] + re_cgfdf_facet()[[r_var2()]])
      y_axis <- re_cgfdf_facet()$other / re_cgfdf_facet()$total_obs
      facet_variable <- re_cgfdf_facet()[[r_facet_by()]]
      no_facet_df <- data.frame(cgf_types, facet_variable, total_obs, x_axis, y_axis, stringsAsFactors = FALSE)
      DT::datatable(no_facet_df, options = list(scrollX = TRUE))
    }
    
    else if (r_facet_choice() == 'FALSE') {
      total_obs <- re_cgfdf_no_facet()$total_obs
      cgf_types <- re_cgfdf_no_facet()$cgf.type
      x_axis <- re_cgfdf_no_facet()[[r_var1()]] / (re_cgfdf_no_facet()[[r_var1()]] + re_cgfdf_no_facet()[[r_var2()]])
      y_axis <- re_cgfdf_no_facet()$other / re_cgfdf_no_facet()$total_obs
      facet_df <- data.frame(cgf_types, total_obs, x_axis, y_axis, stringsAsFactors = FALSE)
      DT::datatable(facet_df, options = list(scrollX = TRUE))
    }
    
  })
}