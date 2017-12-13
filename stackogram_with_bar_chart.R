barChartStackogramUI <- function(id) {
  ns <- NS(id)
  
  tagList(
         # fluidRow(
         #   plotOutput(ns("stackogram_attr_est"))
         # ),
    fluidRow(
      column(6, uiOutput(ns("fill_var"))),
      column(6, uiOutput(ns("cluster_var")))
    ),
    fluidRow(
      plotOutput(ns("prop_graph"))
    ),
    fluidRow(
      plotOutput(ns("bar_graph"))
    ),
    fluidRow(
      column(4,
             sliderInput(inputId = ns("width"),
                         label = "Width of Bars:",
                         min = 0.8,
                         max = 1.0,
                         value = 1.0)),
      column(4,
             radioButtons(inputId = ns("scale"),
                          label = "Frequency Graph Scale:",
                          choices = c("linearly", "log10", "sqrt"))
             ),
      column(4, 
             radioButtons(ns("clustering_method"),
                          label = "Choose Clustering Method: ",
                          choices = c("ward.D", "ward.D2", "single", "complete", "average"),
                          selected = "average"))
   ),
   fluidRow(
     h4("Choose bar colours: "),
     uiOutput(ns("bar_colour_changes"))
   )
  )
}

filter_cgftype <- function(db, cgftype) {
  temp <- db %>%
    filter(cgftype %in% db.cgf_type)
}


barChartStackogram <- function(input, output, session, data_for_plots, data_inputted) {
  
  re_width <- reactive({
    input$width
  })
  
  re_scale <- reactive({
    switch(input$scale,
           "linearly" = "identity",
           "log10" = "log10",
           "sqrt" = "sqrt"
    )
  })
  
  stack_levels = reactive({
    unique(data_inputted()[[re_fill_var()]])
  })
  
  re_fill_var <- reactive({input$fill_chosen})
  
  output$fill_var <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("fill_chosen"),
                label = "Fill Value:",
                choices = colnames(data_for_plots()),
                selected = "typing.lab")
  })  
  
  re_cluster_var <- reactive({input$cluster_by})
  
  output$cluster_var <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("cluster_by"),
                label = "Variable to Cluster by:",
                choices = colnames(data_for_plots()),
                selected = re_fill_var())
  })
  
  re_long_df <- reactive ({
    cgfdf <- data_for_plots() %>%
      select_("cgf.type", "strain.name", re_cluster_var()) %>%
      group_by(cgf.type) %>% mutate(cgf_count = n()) %>%
      group_by_("cgf.type", re_cluster_var()) %>%
      mutate(type_count = n(), prop = n()/cgf_count) 
    print('cgfdf')
    per_cgf_df <- cgfdf %>% group_by_("cgf.type", re_cluster_var(), "type_count") %>% summarise()
    print('per cgf df')
    long_df <- merge(cgfdf, per_cgf_df, by.x = "cgf.type", by.y = "cgf.type")
    print(long_df)
    long_df <- expandRows(long_df, 'type_count.y')
    print(long_df)
    return(long_df)
  })
  
  re_order <- reactive ({
    cast_df <- dcast(re_long_df(), paste0("strain.name + cgf.type + cgf_count ~", re_cluster_var(), ".y"))
    if (nrow(cast_df) == 1) {
      cast_df["cgf_count"] <- NULL
      return(cast_df[1, ])
    }
    
    if (is.vector(cast_df[,c(-1,-2,-3)])) {
      cast_df[ , 4] <- 1  
      cast_df["cgf_count"] <- NULL
    } else {
      cast_df <- cbind(cast_df[, c(1,2)], cast_df[, c(-1,-2,-3)]/rowSums(cast_df[, c(-1,-2,-3), drop = FALSE]))
    }
    
    dup_cgf_df <- cast_df %>% select(-which(colnames(cast_df) == "strain.name")) 
    cast_df <- unique(dup_cgf_df)
    clusters <- hclust(dist(cast_df[ , -c(1, 2)]), method = input$clustering_method)
    order_ind <- order.dendrogram(as.dendrogram(clusters))
    
    return (cast_df[order_ind, ])
  })
  
  CPCOLS <- reactive({
    options <- unique(data_inputted()[[re_fill_var()]])
    colour_options = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
    names(colour_options) <- options
    
    colours <- c()
    lapply(1:length(options), function(x) {
      colours <<- append(colours, input[[paste0("colour_picker", options[x])]])
    })
    
    colours
  })
  
  output$bar_colour_changes <- renderUI({
    ns <- session$ns
    
    colour_options = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
    options <- unique(data_inputted()[[re_fill_var()]])
    if (length(options) > length(colour_options)) {
      colour_options <- rep(colour_options, length.out = length(options))
    }
    
    names(colour_options) <- options
    
    lapply(unique(data_inputted()[[re_fill_var()]]), function(n) {
      colourInput(inputId = ns(paste0("colour_picker", n)),
                  label = paste("Choose colour for: ", n),
                  value = colour_options[n],
                  palette = "limited",
                  allowedCols = colour_options)}
    )
  })
  
  output$prop_graph <- renderPlot({
    ggplot(data_for_plots() %>% select(cgf.type, re_fill_var()), aes(x=cgf.type, y=1, width=re_width(),
                                                                         # fill= factor(source.specific_1, levels = stack_levels()),
                                                                         ordered=TRUE)) +
      geom_bar(aes(fill = factor(data_for_plots()[[re_fill_var()]], levels = stack_levels())),
               position=position_fill(), stat="identity") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "CGF Type", y = "Proportion", fill = re_fill_var()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            # axis.text.x = element_blank(),
            # axis.title.x = element_blank(),
            # axis.ticks.x = element_blank(),
            # plot.margin = unit(c(1,2,0,1), "cm"),
            legend.position = "bottom") +
      # scale_fill_brewer(palette = "RdYlBu") +
      scale_fill_manual(values = CPCOLS(), drop = FALSE) +
      scale_x_discrete(limits = re_order()$cgf.type) 
  })
  
  output$bar_graph <- renderPlot({
    
    ggplot(data_for_plots() %>% select(cgf.type, re_fill_var()), aes(x=cgf.type, ordered = TRUE)) +
      geom_bar(aes(fill = factor(data_for_plots()[[re_fill_var()]], levels = stack_levels())), width = re_width()) +
      labs(x = "CGF Type", y = paste(re_scale(), "(Frequency)"), fill = re_fill_var()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            # plot.margin = unit(c(0, 1, 1, 1), "cm"),
            legend.position = "bottom") +
      # scale_fill_brewer(palette = "RdYlBu") +
      scale_fill_manual(values = CPCOLS(), drop = FALSE) +
      scale_x_discrete(limits = re_order()$cgf.type) +
      scale_y_continuous(trans = re_scale())
  })
  
  # callModule(propBarWidth, "attr_est_wide_graph", data_for_plots)
  
  
  #FOR MAKING PLOTS JOIN TOGETHER
  # r_prop_plot <- reactive({
  #   ggplot(data_for_plots() %>% select(cgf.type, source.specific_1), aes(x=cgf.type, y=1, width=re_width(),
  #                                                                        # fill= factor(source.specific_1, levels = stack_levels()),
  #                                                                        ordered=TRUE)) +
  #     geom_bar(aes(fill = factor(source.specific_1, levels = stack_levels())),
  #              position=position_fill(), stat="identity") +
  #     scale_y_continuous(labels = scales::percent) +
  #     labs(x = "CGF Type", y = "Proportion", fill = "Source") +
  #     theme(axis.text.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.ticks.x = element_blank(),
  #           plot.margin = unit(c(1,2,0,1), "cm"),
  #           legend.position = "none") +
  #     scale_fill_brewer(palette = "RdYlBu") 
  #     # scale_x_discrete(limits = get_order()$cgf.type)
  # })
  # 
  # r_count_plot <- reactive({
  #   ggplot(data_for_plots() %>% select(cgf.type, source.specific_1), aes(x=cgf.type, ordered = TRUE)) +
  #     geom_bar(aes(fill = factor(source.specific_1, levels = stack_levels())), width = re_width()) +
  #     labs(x = "CGF Type", y = paste(re_scale(), "(Frequency)"), fill ="Source") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1),
  #           plot.margin = unit(c(0, 1, 1, 1), "cm"), legend.position = "bottom") +
  #     scale_fill_brewer(palette = "RdYlBu") +
  #     # scale_x_discrete(limits = get_order()$cgf.type) +
  #     scale_y_continuous(trans = re_scale())
  # })
  # 
  # output$stackogram_attr_est <- renderPlot({
  #   align_plot <- ggarrange(r_prop_plot(), r_count_plot(), heights = c(2, 4), ncol = 1, nrow = 2, align = "v")
  #   plot(align_plot)
  # })
}




#todo: DELETE ME
# get_order_stacked_bar_per_cgftype <- function(data) {
#   stats <- data %>%
#     group_by(cgf.type, source.specific_1) %>%
#     summarise(type_count = n()) %>%
#     mutate(prop = type_count/sum(type_count))
#   all_types <- as_data_frame(stats$cgf.type) %>% group_by(cgf.type=stats$cgf.type) %>% summarise()
#   order_types <- as_data_frame(x=list(cgf.type = stats$cgf.type, source.specific_1 = stats$source.specific_1, prop = stats$prop)) %>%
#     filter(source.specific_1 == "Chicken" | source.specific_1 == "Human")
#   merged_types <- merge(x=all_types, y=order_types, by.x="cgf.type", by.y = "cgf.type", all=TRUE)
#   max_type <- merged_types %>%
#     group_by(cgf.type) %>%
#     summarise(max = max(prop))
#   merged_type_order <- merge(x=merged_types, y=max_type, by.x=c("prop", "cgf.type"), by.y=c("max","cgf.type"))
#   order <- merged_type_order %>% arrange(source.specific_1, -prop)
# }