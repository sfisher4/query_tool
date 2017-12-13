require(shiny)
require(ggplot2)
require(readr)

#Module UI function
subsetDataInput <- function(id, label = "Fingerprint:") {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(3, h3("Subset Data:"))
    ),
    fluidRow(
      column(12, fileInput(inputId = ns("cgfdb_input"),
                           label = "Choose dataset",
                           accept = c('text/csv', 'text/comma-separated-values', '.csv')))
    ),
    fluidRow(
      radioButtons(inputId = ns("comparison_method"),
                   label = "Comparison Option: ",
                   choices = c("by_pattern", "by_cgf_type"))
    ),
    fluidRow(
      conditionalPanel(condition = paste0("input['", ns("comparison_method"), "'] == 'by_pattern'"),
                       column(3, textInput(inputId = ns("pattern_in"),
                                           label = "Fingerprint to Query: ")),
                       conditionalPanel(condition = paste0("input['", ns("pattern_in"), "'] != ''"),
                                        column(3, selectInput(inputId = ns("min_perc_sim_in"),
                                                              label = "Minimum % Similarity:",
                                                              choices = seq(50, 100, by=2.5),
                                                              selected = 85)),
                                        column(3, textInput(inputId = ns("min_isol_in"),
                                                            label = "Minimum # of Isolates:",
                                                            value = 10))
                       )
      ),
      conditionalPanel(condition = paste0("input['", ns("comparison_method"), "'] == 'by_cgf_type'"),
                       column(3, textInput(inputId = ns("cgf_type_in"),
                                           label = "CGF Type to Query: ")),
                       conditionalPanel(condition = paste0("input['", ns("cgf_type_in"), "'] != ''"),
                                        column(3, selectInput(inputId = ns("per_sim_cgftype"),
                                                              label = "Minimum % Similarity:",
                                                              choices = c(90, 95, 100),
                                                              selected = 95)),
                                        column(3, textInput(inputId = ns("min_isol_cgftype"),
                                                            label = "Minimum # of Isolates:",
                                                            value = 10))
                       )
      )
    ),
    fluidRow(
     uiOutput(ns("filters"))
    ),
    uiOutput(ns("subset_filters")),
    actionButton(ns("refresh"), "REFRESH QUERY")
  )
}

#Module server function
subsetData <- function(input, output, session) {
  
  # original_data <- reactiveVal(cgfdb_testset2)
  original_data   <- reactive({
    infile <- input$cgfdb_input
    if (is.null(infile)) {
      return(NULL)
    }
    # read.csv(infile$datapath, header = TRUE, check.names = FALSE)
    read_csv(infile$datapath)
  })
  
  # data_values     <- reactiveValues(original_data = cgfdb_testset2, filtered_data = cgfdb_testset2)
  re_pattern      <- reactive({
    if (input$pattern_in != "") {
      validate(need(nchar(input$pattern_in) == 79,
                    "Invalid pattern length. Please input a pattern with 40 binary characters separated by tabs, spaces or commas."))
      validate(need((count.fields(textConnection(input$pattern_in), sep = "\t") == 40) | 
                      (count.fields(textConnection(input$pattern_in), sep = " ") == 40) | 
                      (count.fields(textConnection(input$pattern_in), sep = ",") == 40), 
                    "Invalid pattern separation. Please separate 40 binary character pattern using tabs, spaces or commas."))
    }
    input$pattern_in
  })
  re_cgf_type     <- reactive({input$cgf_type_in})
  re_min_isol     <- reactive({input$min_isol_in})
  re_min_perc_sim <- reactive({input$min_perc_sim_in})
  re_filter_opt   <- reactive({input$filter_options})
  re_compare_method <- reactive({input$comparison_method})
  refresh_button <- reactive({input$refresh})
  
  output$filters <- renderUI({
    ns <- session$ns
    pickerInput(inputId = ns("filter_options"),
                label = "Choose options to filter by: ",
                multiple = TRUE,
                options = list('actions-box' = TRUE),
                choices = colnames(original_data()))
  })
  
  output$subset_filters <- renderUI({
    ns <- session$ns
    lapply(re_filter_opt(), function (x) {
      pickerInput(inputId = ns(paste0("col" , x)),
                  label = paste0("Choose subset of ", x),
                  options = list('actions-box' = TRUE),
                  choices = unique(original_data()[[x]]),
                  multiple = TRUE)
    })
  })
  
  # observe({
  #   if str_len(re_cgf_type()) > 
  # })
  
  query_data <- eventReactive(refresh_button(), {
    if (re_pattern() != "" & re_compare_method() == "by_pattern") {
      pattern_df <- reactive({perc_sim(original_data(), re_pattern(), re_min_perc_sim(), re_min_isol())})
    }
    else if (re_cgf_type() != "" & re_compare_method() == "by_cgf_type") {
      pattern_df <- reactive({cgf_type_sim(original_data(), re_cgf_type(), input$per_sim_cgftype, input$min_isol_cgftype)})
    }
    else {
      pattern_df <- reactive({original_data()})
    }
    
    merged_filters_df <- reactive({
      all_single_filters <- lapply(re_filter_opt(), function(x) {
        single_filter2 <- original_data()[original_data()[[x]] %in% input[[paste0("col", x)]], ]
      })
      merged_data <- Reduce(function(x,y) {merge(x,y)}, all_single_filters)
      return(merged_data)
    })
    
    if (re_pattern() == "" & re_cgf_type() == "" & is.null(re_filter_opt())){
      return(original_data())
    }
    else if (is.null(re_filter_opt())) {
      return(pattern_df())
    }
    else {
      all_filters_df <- reactive({merge(pattern_df(), merged_filters_df())})
      return(all_filters_df())
    }
    
  })
  
  return(list(subset_data = query_data, data_input = original_data))
}

# helpers
perc_sim <- function(dataset, text, min_sim, min_isol) {
  print('text')
  print(nchar(text))
  
  cgfdb <- dataset %>%
    unite(col = "Pattern", grep("cj", colnames(dataset)), sep = " ")
  
  if (count.fields(textConnection(text), sep = "\t") == 40) {
    text_sep <- "\t"
  }
  else if (count.fields(textConnection(text), sep = " ") == 40) {
    text_sep <- " "
  }
  else if (count.fields(textConnection(text), sep = ",") == 40) {
    text_sep <- ","
  }
  
  temp_diff <- t(mapply('==',
                        strsplit(as.character(text), split = text_sep),
                        strsplit(as.character(cgfdb$Pattern), split = " ")))
  differences <- as.data.frame(apply(temp_diff, 1, paste, collapse = " "))
  
  names(differences) <- "logical"
  remove(temp_diff)
  cgfdb <- cgfdb %>%
    mutate(logical = differences$logical,
           num_F = str_count(differences$logical, "FALSE"),
           perc_sim = (100 * (40 - num_F) / 40))
  cgfdb <- cgfdb %>%
    group_by(cgf.type) %>%
    mutate(num_isol = n())
  rank_cgf_df <- cgfdb %>%
    group_by(cgf.type) %>%
    summarise(count = n()) %>%
    mutate(rank = rank(count, ties.method = "min"))
  cgfdb <- merge(cgfdb, rank_cgf_df, by.x = "cgf.type", by.y = "cgf.type")
  
  data <- cgfdb
  data[data$perc_sim >= as.numeric(min_sim) & data$num_isol >= as.numeric(min_isol), ]
}

min_isol_dataset <- function(dataset, min_isol) {
  dataset <- dataset %>%
    group_by(cgf.type) %>%
    mutate(num_isol = n())
  
  dataset[dataset$num_isol >= as.numeric(min_isol),]
}

cgf_type_sim <- function(dataset, text, min_sim, min_isol) {
  min_isol_df <- min_isol_dataset(dataset, min_isol)
  if (min_sim == 100) {
    return(min_isol_df %>% filter(cgf.type == text))
  }
  else if (min_sim == 95) {
    text_95_mark <- paste(strsplit(text, split="_(?=[^_]+$)", perl=TRUE)[[1]][1], "_", sep="")
    return(min_isol_df[grep(text_95_mark, min_isol_df$cgf.type), ])
  }
  else if (min_sim == 90) {
    sim_data_90 <- dataset %>% mutate("90sim" = paste(sapply(strsplit(dataset$cgf.type, split="[_]", perl=TRUE), '[', 1), "_", sep = ""))
    sim_data_90 <- sim_data_90 %>% group_by(cgf.type) %>% mutate(num_isol = n()) %>% filter(num_isol >= as.numeric(min_isol))
    text_90_mark <- paste(strsplit(text, "_")[[1]][1], "_", sep="")
    return(sim_data_90[text_90_mark == sim_data_90$"90sim", ])
  }
  
}
