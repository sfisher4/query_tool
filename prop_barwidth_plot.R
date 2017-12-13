require(shiny)
require(ggplot2)
require(magrittr)
require(tibble)
require(data.table)
require(dplyr)
require(reshape2)
require(plotly)
require(ggpubr)
require(scales)
require(splitstackshape)
require(colourpicker)

propBarWidthUI <- function(id, label = "Frequency as Bar Width") {
  ns <- NS(id)
  
  tagList(
    # titlePanel("Proportion and Frequency of Sources vs CGF Type"),
    fluidRow(
      column(6, uiOutput(ns("fill_var"))),
      column(6, uiOutput(ns("cluster_var")))
    ),
    fluidRow(
      plotOutput(ns("freq_width_plot"))
    ),
    
    fluidRow(
      DT::dataTableOutput(outputId = ns("data_to_show"))
    ),
    
    fluidRow(
      radioButtons(ns("clustering_method"),
                   label = "Choose Clustering Method: ",
                   choices = c("ward.D", "ward.D2", "single", "complete", "average"),
                   selected = "average")
    ),
    fluidRow(
      h4("Choose bar colours: "),
      uiOutput(ns("bar_colour_changes"))
    )
  )
}
# CPCOLS <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

propBarWidth <- function(input, output, session, data_for_plots, data_inputted) {
  re_col_colour_opt <- reactive({input$col_colour_opt})
  re_cluster_var <- reactive({input$cluster_by})
  re_fill_var <- reactive({input$fill_chosen})
  
  output$fill_var <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("fill_chosen"),
                label = "Fill Value:",
                choices = colnames(data_for_plots()),
                selected = "typing.lab")
  })  
  
  output$cluster_var <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("cluster_by"),
                label = "Variable to Cluster by:",
                choices = colnames(data_for_plots()),
                selected = re_fill_var())
  })
  
  re_long_df <- reactive ({
    cgfdf <- data_for_plots() %>%
      select_("cgf.type", "strain.name", re_fill_var()) %>%
      group_by(cgf.type) %>% mutate(cgf_count = n()) %>%
      group_by_("cgf.type", re_fill_var()) %>%
      mutate(type_count = n(), prop = n()/cgf_count) 
    per_cgf_df <- cgfdf %>% group_by_("cgf.type", re_fill_var(), "type_count") %>% summarise()
    long_df <- merge(cgfdf, per_cgf_df, by.x = "cgf.type", by.y = "cgf.type")
    print(long_df)
    long_df <- expandRows(long_df, 'type_count.y')
    print(long_df)
    return(long_df)
  })
  
  re_cluster_df <- reactive({
    cgfdf <- data_for_plots() %>%
      select_("cgf.type", "strain.name", re_cluster_var()) %>%
      group_by(cgf.type) %>% mutate(cgf_count = n()) %>%
      group_by_("cgf.type", re_cluster_var()) %>%
      mutate(type_count = n(), prop = n()/cgf_count) 
    per_cgf_df <- cgfdf %>% group_by_("cgf.type", re_cluster_var(), "type_count") %>% summarise()
    long_df <- merge(cgfdf, per_cgf_df, by.x = "cgf.type", by.y = "cgf.type")
    print(long_df)
    long_df <- expandRows(long_df, 'type_count.y')
    print(long_df)
    return(long_df)
  })
  
  re_order <- reactive ({
    cast_df <- dcast(re_cluster_df(), paste0("strain.name + cgf.type + cgf_count ~", re_cluster_var(),".y"))
    
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
    
    clusters <- hclust(dist(cast_df[ , c(-1, -2)]), method = input$clustering_method)
    order_ind <- order.dendrogram(as.dendrogram(clusters))
      
    return (cast_df[order_ind, ])
  })
  
  stacked_levels = reactive({
    unique(data_inputted()[[re_fill_var()]])
  })
  
  # colourList <- c(Human = "red", Buffalo = "green", Cow = "orange", Horse = "yellow", Raccoon = "white", Sheep = "blue", Skunk = "black", Turkey = "purple", Water = "indigo", Chicken = "pink")
  #, '#a6cee3'
  
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
  
  # output$col_colour_changes <- renderUI({
  #   ns <- session$ns
  #   pickerInput(inputId = ns("col_colour_opt"),
  #               label = "Choose column to change colours: ",
  #               multiple = TRUE,
  #               options = list('actions-box' = TRUE),
  #               choices = unique(re_long_df()$source.specific_1.y))
  # })
  
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
  
  
  output$freq_width_plot <- renderPlot ({
    plot <- ggplot(re_long_df(), aes(x=strain.name, y=1, width=1.0,
                                ordered=TRUE)) +
      geom_bar(aes(fill = factor(re_long_df()[[paste0(re_fill_var(), ".y")]], levels = stacked_levels())), 
               position=position_fill(), stat="identity", lwd=0.01, color="white") +
      scale_y_continuous(labels = scales::percent) +
      labs(x="Strain", y="Proportion of Sources", fill="Source") +
      theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
      # scale_fill_brewer(palette = "RdYlBu", drop = FALSE) +
      scale_fill_manual(values = CPCOLS(), drop = FALSE) +
      scale_x_discrete(limits = re_order()$strain.name) 
    plot(plot)
  })
  
  output$data_to_show <- DT::renderDataTable({
    print('!!reorder')
    print(re_order())
    if (nrow(re_order()) == 1) {
      return(DT::datatable(re_order(), options = list(scrollX = TRUE)))
    }
    
    dataset_full <- re_order() %>%
      group_by(cgf.type) %>% 
      mutate(total = n()) %>%
      select(-strain.name) 
    
    dataset_with_perc <- as.data.frame(apply(dataset_full[, -c(which(colnames(dataset_full) == "cgf.type"), 
                                                               which(colnames(dataset_full) == "total"))], c(1,2), function(x) round(x * 100)))
    colnames(dataset_with_perc) <- paste(colnames(dataset_with_perc), "%")
    
    dataset_to_display <- cbind(as.data.frame(dataset_full[, c("cgf.type", "total")]), dataset_with_perc)
    dataset_to_display <- dataset_to_display[!duplicated(dataset_to_display), ]
    dataset_to_display[(nrow(dataset_to_display) + 1), which(unlist(lapply(dataset_to_display, is.numeric)))] <-
      round((colSums(dataset_to_display[ , which(unlist(lapply(dataset_to_display, is.numeric)))], na.rm=TRUE))/nrow(dataset_to_display))
    dataset_to_display[nrow(dataset_to_display), "cgf.type"] = "ALL" 
    dataset_to_display[nrow(dataset_to_display), "total"] = (dataset_to_display[nrow(dataset_to_display), "total"] * (nrow(dataset_to_display) - 1))
    DT::datatable(dataset_to_display, options = list(scrollX = TRUE))
  })
}
