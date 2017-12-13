
bubbleSourceSampleUI <- function(id){
  ns <- NS(id)
  
  tagList(
    titlePanel("Percentage of Source vs Percentage of Sample for CGF Types"),
    fluidRow(
      column(4,
             selectInput(inputId = ns("source_opt"),
                         label = "Source Options (X-Axis):",
                         choices = NULL)
      ),
      column(4,
             selectInput(inputId = ns("sample_opt"),
                         label= "Sample Options (Y-Axis):",
                         choices = NULL)
      ),
      column(4,
             checkboxInput(inputId = ns("facet_choice"),
                           label = "Facet?",
                           value = FALSE)
      ),
      column(4, uiOutput(ns("view_by_selector")))
    ),
    fluidRow(
      column(4,
             sliderInput(inputId = ns("min_slider"),
                         label = "Min Size of Points",
                         min = 1,
                         max = 50,
                         value = 1)),
      column(4,
             sliderInput(inputId = ns("max_slider"),
                         label = "Max Size of Points",
                         min = 1,
                         max = 50,
                         value = 8)),
      column(4,
             uiOutput(ns("subset_view_selector")))
    ),
    fluidRow(
      column(12,
             plotlyOutput(ns("plot")))
    )
  )
}

bubbleSourceSample <- function(input, output, session, data_for_plots) {
  
  #Tab 7 : Bubble source vs sample plot  
  r_source <- reactive({input$source_opt})
  r_sample <- reactive({input$sample_opt})
  re_facet <- reactive({input$view_by})
  re_facet_subset <- reactive({input$subset_view})
  r_facet_choice <- reactive(input$facet_choice)
  
  observe({
    updateSelectInput(session, 
                      inputId = "source_opt",
                      choices = data_for_plots()$source.specific_1)
    updateSelectInput(session,
                      inputId = "sample_opt",
                      choices = data_for_plots()$sample.type)
  })
  
  
  output$view_by_selector <- renderUI({
    ns <- session$ns
    if (r_facet_choice()) {
      selectInput(inputId = ns("view_by"),
                label = "View by:",
                choices = c("", colnames(data_for_plots())), #TODO: unless the column name starts with 'cj'
                selected = "province")
    }
    
  })

  output$subset_view_selector <- renderUI({
    ns <- session$ns
    if (r_facet_choice()) {
      subset <- data_for_plots()[, re_facet()]
      options = unique(subset)
      pickerInput(inputId = ns("subset_view"),
                  label = paste("Select", re_facet(), "subset to view: "),
                  options = list('actions-box' = TRUE),
                  choices = options,
                  selected = options,
                  multiple = TRUE)
    }
  })
  
  # test <- cgfdb_testset2 %>% group_by(cgf.type) %>% mutate(total = n())
  # test1 <- head(dcast(test, cgf.type + total ~ c(source.specific_1, sample.type), length))
  # test1 <- test1 %>% mutate(prop_x = Chicken/total, prop_y = Clinical/total)
  
  re_filtered_df_no_facet <-reactive({
    df <- data_for_plots() %>%
      group_by(cgf.type) %>%
      mutate(total = n())
    df <- dcast(df, cgf.type + total ~ c(source.specific_1, sample.type), length)
    df <- df %>%
      mutate_(.dots = setNames(list(paste(r_source(), "/", "total")), "source_prop")) %>%
      mutate_(.dots = setNames(list(paste(r_sample(), "/", "total")), "sample_prop"))
    print(df)
    df
  })
  
  re_filtered_df_facet <- reactive({
    
    df <- data_for_plots() %>%
      group_by_("cgf.type", re_facet()) %>%
      mutate(total = n())
    df <- dcast(df, paste("cgf.type", "+", "total", "+", re_facet(), "~", "c(source.specific_1, sample.type)", sep=""), length)
    df <- df %>%
      mutate_(.dots = setNames(list(paste(r_source(), "/", "total")), "source_prop")) %>%
      mutate_(.dots = setNames(list(paste(r_sample(), "/", "total")), "sample_prop"))
    df
  })
  
  re_no_facet_plot <- reactive({
    df <- re_filtered_df_no_facet()
    tooltip_text = paste("Total Obs:", df$total_obs, "\n", 
                         "CGF Type:", df$cgf.type, "\n", 
                         "%", r_source(), "in Source (x-axis)", df$source_prop, "\n",
                         "%", r_sample(), "in Sample (y-axis)", df$sample_prop)
    
    ggplot(df, aes(x=source_prop, y=sample_prop, text = tooltip_text)) +
      geom_point(aes(size = total, colour = cgf.type)) +
      labs(x = paste("SOURCE: %", r_source()), y = paste("SAMPLE: %", r_sample())) +
      scale_fill_discrete(name="Total Observations of CGF Type") + 
      theme_gray() +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      scale_size_continuous(range = c(input$min_slider, input$max_slider)) 
  })
  
  re_facet_plot <- reactive({
    filtered_df_facet <- re_filtered_df_facet()
    columns_facet <- filtered_df_facet[, colnames(filtered_df_facet) == re_facet()]
    facet_df <- filtered_df_facet[columns_facet %in% re_facet_subset(), ]
    tooltip_text = paste("Total Obs:", facet_df$total_obs, "\n", 
                         "CGF Type:", facet_df$cgf.type, "\n", 
                         "%", r_source(), "in Source (x-axis)", facet_df$source_prop, "\n",
                         "%", r_sample(), "in Sample (y-axis)", facet_df$sample_prop)
    
    ggplot(facet_df, aes(x=source_prop, y=sample_prop, text = tooltip_text)) +
      geom_point(aes(size = total, colour = cgf.type)) +
      labs(x = paste("SOURCE: %", r_source()), y = paste("SAMPLE: %", r_sample())) +
      scale_fill_discrete(name="Total Observations of CGF Type") + 
      theme_gray() +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      scale_size_continuous(range = c(input$min_slider, input$max_slider)) + 
      facet_wrap(as.formula(paste("~", re_facet())))
  })
  
  output$plot <- renderPlotly({
    if (r_facet_choice()) {
      ggplotly(re_facet_plot(), tooltip = "text")
    }
    else {
      ggplotly(re_no_facet_plot(), tooltip = "text")
    }
  })
}