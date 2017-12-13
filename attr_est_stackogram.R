require(splitstackshape)


attrEstStackogramUI <- function(id) {
  ns <- NS(id)
  
  tagList(titlePanel("Stackogram with Attributes Estimated"),
          fluidRow(
            plotOutput(ns("freq_width_plot"))
          ),
          fluidRow(
            column(4,
                   checkboxInput(inputId = ns("do_estimation"),
                                 label = "Estimate Attributes?",
                                 value = FALSE)),
            conditionalPanel(condition = paste0("input['", ns("do_estimation"), "']"),
                             column(4,
                                    numericInput(inputId = ns("min_isol"),
                                    label = "Estimate Attributes with Total Isolates <= :",
                                    value = 5)),
                             column(4,
                                    selectInput(inputId = ns("min_perc_sim"),
                                                label = "Min Percentage Similarity for Attribute Estimation",
                                                choices = c(90, 92.5, 95, 97.5),
                                                selected = 95))
            ),
            conditionalPanel(condition = paste0("!input['", ns("do_estimation"), "']"),
                             column(4,
                                    numericInput(inputId = ns("min_isol_rm"),
                                                 label = "Remove Total Isolates <= :",
                                                 value = 5))
            )
          ),
          fluidRow(
            column(12,
                   fileInput(ns("db_in_for_est"),
                             label = "Option: Input database with pattern and # of isolates for estimation"
                   ))
          ),
          fluidRow(
            column(12,
                   radioButtons(ns("clustering_method"),
                                label = "Choose Clustering Method: ",
                                choices = c("ward.D", "ward.D2", "single", "complete", "average"),
                                selected = "average")
            )
          ),
          fluidRow(
            DT::dataTableOutput(outputId = ns("all_data_table"))
          )
           # fluidRow(
           #   selectInput(inputId = ns("graph_type"),
           #               label = "Choose Type of Graph to Display:",
           #               choices = c("Frequency of isolates shown using bar chart",
           #                           "Frequency of isolates shown as width of bars"),
           #               selected = "")),
           # 
           # conditionalPanel(condition = paste0("input['", ns("graph_type"), "'] == 'Frequency of isolates shown using bar chart'"),
           #                  barChartStackogramUI(ns("attr_est_bar_graph"))
           # ),
           
           # conditionalPanel(condition = paste0("input['", ns("graph_type"), "'] == 'Frequency of isolates shown as width of bars'"),
                            # propBarWidthUI(ns("attr_est_wide_graph"))
           # )
  )
}


estimate_attr <- function(df_to_estimate, p_sim, min_isol, wide_df) {
  
  tester <- df_to_estimate %>%
    mutate(perc_sim = lapply(df_to_estimate$Pattern, function(a)
      which((40 - mapply(function(x,y) sum(x != y), strsplit(as.character(wide_df$Pattern), " "), strsplit(a, " ")))/40 >= p_sim)))

  tester <- tester %>% mutate(new_total = as.integer(lapply(tester$perc_sim, function(x) tail(cumsum(lapply(x, function(y) wide_df[y, "total_no_hum"][[1]])), n=1))))  
  print('tester')
  print(tester)
  print('wide df ajfklsajl')
  print(wide_df)
  # col_names <- colnames(tester[ , c(-which(colnames(tester) == "cgf.type"),
  #                                   -which(colnames(tester) == "Pattern"),
  #                                   -which(colnames(tester) == "total_no_hum"),
  #                                   -which(colnames(tester) == "total_with_hum"),
  #                                   -which(colnames(tester) == "new_total"),
  #                                   -which(colnames(tester) == "perc_sim")), drop = FALSE])
  
  col_names <- colnames(wide_df[ , c(-which(colnames(wide_df) == "cgf.type"),
                                    -which(colnames(wide_df) == "Pattern"),
                                    -which(colnames(wide_df) == "total_no_hum"),
                                    -which(colnames(wide_df) == "total_with_hum")), drop = FALSE])
  print('colnames!!!')
  print(col_names)
  
  lo_col_sums <- lapply(
    tester$perc_sim,
    function(x) {
      lapply(col_names, function(y) {
        sum(wide_df[x, y])
      })
    }
  )
  print('lo col sums')
  
  est_data <- data.frame(matrix(unlist(lo_col_sums), nrow = nrow(tester), byrow=T), stringsAsFactors = FALSE)
  est_data[is.na(est_data)] <- 0
  colnames(est_data) <- col_names
  est_data <- est_data %>% mutate(cgf.type = tester$cgf.type, Pattern = tester$Pattern, total_no_hum = tester$total_no_hum, total_with_hum = tester$total_with_hum, new_total = tester$new_total, perc_sim = tester$perc_sim)
  
  # already_good_data <- wide_df %>% filter(total > min_isol)
  # all_data <- merge(already_good_data, est_data, all.x=TRUE, all.y=TRUE)
  
  #return just the estimated data
  return(est_data)
}


attrEstStackogram <- function(input, output, session, data_for_plots, data_inputted) {
  re_graph_type <- reactive({input$graph_type})
  re_min_isol <- reactive({input$min_isol})
  re_min_perc_sim <- reactive({input$min_perc_sim})
  re_db_in_for_est   <- reactive({
    infile <- input$db_in_for_est
    if (is.null(infile)) {
      return(NULL)
    }
    # read.csv(infile$datapath, header = TRUE, check.names = FALSE)
    in_df <- read_csv(infile$datapath)
    in_df %>% unite(col = "Pattern", grep("cj", colnames(data_for_plots())), sep = " ")
  })
  
  re_data_before_estimates <- reactive({
    wide_df <- data_for_plots() %>% unite(col = "Pattern", grep("cj", colnames(data_for_plots())), sep = " ")
    wide_df <- dcast(wide_df, cgf.type + Pattern ~ source.specific_1)
    wide_df <- wide_df %>% mutate(total_with_hum = rowSums(wide_df[, -c(which(colnames(wide_df) == "cgf.type"), which(colnames(wide_df) == "Pattern")), drop=FALSE]))
    wide_df$Human <- NULL
    wide_df <- wide_df %>% mutate(total_no_hum = rowSums(wide_df[, -c(which(colnames(wide_df) == "cgf.type"), 
                                                                      which(colnames(wide_df) == "total_with_hum"),
                                                                      which(colnames(wide_df) == "Pattern")), drop=FALSE])) #,which(colnames(wide_df) == "old_total")
    print('wide df')
    print(wide_df)
    wide_df
  })
  
  #FOR ESTIMATED
  wide_data_with_estimates <- reactive({
    
    # perc_sim_choices = c(0.95, 0.925, 0.90)
    # 
    # all_est_data_97.5 <- estimate_attr(data_for_plots(), 0.975, re_min_isol(), re_data_before_estimates())
    # est_data_97.5 <- all_est_data_97.5 %>% filter(total > re_min_isol() | new_total > re_min_isol())
    # data <- all_est_data_97.5 %>% filter(total <= re_min_isol() & new_total <= re_min_isol())
    # 
    # lapply(perc_sim_choices, function(x) {
    #   all_est_data_95 <- estimate_attr(data, x, re_min_isol(), re_data_before_estimates())
    #   est_data_95 <- all_est_data_95 %>% filter(total > re_min_isol() | new_total > re_min_isol())
    #   data <- all_est_data_95 %>% filter(total <= re_min_isol() & new_total <= re_min_isol())
    #   duprows <- rownames(est_data_97.5) %in% rownames(est_data_95)
    #   est <- rbind(est_data_97.5, est_data_95[!duprows, ])
    # })
    
    print('min perc sim')
    print(re_min_perc_sim())
    print('data before est')
    print(re_data_before_estimates())
    
    if (!is.null(re_db_in_for_est())) {
      print('data to estimate!!!')
      print(re_db_in_for_est())
      data_to_est <- re_db_in_for_est() %>% filter(total_no_hum <= re_min_isol())
    }
    else {
      data_to_est <- re_data_before_estimates() %>% filter(total_no_hum <= re_min_isol())
    }
    
    print('!!!data_to_est')
    print(data_to_est)
    
    if (nrow(data_to_est) != 0) {
      all_est_data_97.5 <- estimate_attr(data_to_est, 0.975, re_min_isol(), re_data_before_estimates())
      est_data_97.5 <- all_est_data_97.5 %>% filter(total_no_hum > re_min_isol() | new_total > re_min_isol()) %>% mutate(sim_used = "97.5%")
      data_to_est <- all_est_data_97.5 %>% filter(total_no_hum <= re_min_isol() & new_total <= re_min_isol())
      est <- est_data_97.5
      
      print('est1')
      print(est_data_97.5 %>% mutate(sim_used = "97.5%"))
      
    }
    
    if (re_min_perc_sim() <= 95 & nrow(data_to_est) != 0) {
      all_est_data_95 <- estimate_attr(data_to_est, 0.95, re_min_isol(), re_data_before_estimates())
      est_data_95 <- all_est_data_95 %>% filter(total_no_hum > re_min_isol() | new_total > re_min_isol()) %>% mutate(sim_used = "95%")
      data_to_est <- all_est_data_95 %>% filter(total_no_hum <= re_min_isol() & new_total <= re_min_isol())
      # est <- rbind(est, est_data_95)
      est <- bind_rows(est, est_data_95 %>% mutate(sim_used = "95%"))
      print('est2')
      print(bind_rows(est, est_data_95 %>% mutate(sim_used = "95%")))
      print(est)
    }

    if (re_min_perc_sim() <= 92.5 & nrow(data_to_est) != 0) {
      all_est_data_925 <- estimate_attr(data_to_est, 0.925, re_min_isol(), re_data_before_estimates())
      est_data_925 <- all_est_data_925 %>% filter(total_no_hum > re_min_isol() | new_total > re_min_isol()) %>% mutate(sim_used = "92.5%")
      data_to_est <- all_est_data_925 %>% filter(total_no_hum <= re_min_isol() & new_total <= re_min_isol())
      # est <- rbind(est, est_data_925)
      est <- bind_rows(est, est_data_925 %>% mutate(sim_used = "92.5%"))
      print('est3')
      print(bind_rows(est, est_data_925 %>% mutate(sim_used = "92.5%")))
      print(est_data_925)
      print(est)
    }

    if (re_min_perc_sim() <= 90 & nrow(data_to_est) != 0) {
      all_est_data_90 <- estimate_attr(data_to_est, 0.9, re_min_isol(), re_data_before_estimates())
      print('all est data 90')
      print(all_est_data_90)
      est_data_90 <- all_est_data_90 %>%
        filter(total_no_hum > re_min_isol() | new_total > re_min_isol()) %>%
        mutate(sim_used = "90%")
      data_to_est <- all_est_data_90 %>% filter(total_no_hum <= re_min_isol() & new_total <= re_min_isol())
      # est <- rbind(est, est_data_90)
      est <- bind_rows(est, est_data_90 %>% mutate(sim_used = "90%"))
      
      print('est4')
      print(bind_rows(est, est_data_90 %>% mutate(sim_used = "90%")))
      print(est_data_90)
      print(est)
    }
      
    non_estimated_data <- re_data_before_estimates() %>% filter(total_no_hum > re_min_isol())
    all_data <- merge(non_estimated_data, est, all.x=TRUE, all.y =TRUE)
    if(any(is.na(all_data$sim_used))) {
    all_data[is.na(all_data$sim_used), ]$sim_used <- "100%"
    }
    print('all data')
    print(all_data)
    return(all_data)
  })
  
  long_data_with_estimates <- reactive({
    #1 Split into each strain
    #2 expand rows according to type count.
    print('wide data with est')
    print(wide_data_with_estimates())
    long_expanded <- expandRows(wide_data_with_estimates(), "total_with_hum") %>% mutate(row_num = as.character(row_number())) %>% select(-perc_sim)
    print('long expanded')
    print(long_expanded)
    wide <- melt(long_expanded,
         id.vars = c("cgf.type", "Pattern", "row_num", "new_total", "total_no_hum", "sim_used"),
         variable.name = "source.specific_1", value.name = "type_count") %>%
      filter(type_count != 0)
    
    print('wide expanded')
    print(expandRows(wide, "type_count"))
    expandRows(wide, "type_count")
  })
  
  re_get_order <- reactive({
    if (nrow(wide_data_with_estimates()) == 1) {
      return(wide_data_with_estimates() %>% select(-perc_sim))
    }

    #1. Get proportion for each source
    wide_data <- wide_data_with_estimates() %>% select(-perc_sim)
    df_with_prop <- sweep(wide_data %>%
                            select(-cgf.type, -Pattern, -total_no_hum, -total_with_hum, -new_total, -sim_used),
                          MARGIN = 1, FUN="/", STATS = rowSums(wide_data %>%
                                                                 select(-cgf.type, -Pattern, -total_no_hum, -total_with_hum, -new_total, -sim_used)))

    all_df <- cbind(wide_data %>% select(cgf.type, Pattern, total_no_hum, total_with_hum, new_total), df_with_prop)
    #2. Split into each strain
    by_strain_df <- expandRows(all_df, "total_with_hum")
    
    all_clusters <- hclust(dist(by_strain_df[ , c(-which(colnames(by_strain_df) == "cgf.type"),
                                                  -which(colnames(by_strain_df) == "Pattern"),
                                                  -which(colnames(by_strain_df) == "total_with_hum"),
                                                  -which(colnames(by_strain_df) == "total_no_hum"),
                                                  -which(colnames(by_strain_df) == "new_total"))]), method = input$clustering_method)
    order_rows <- order.dendrogram(as.dendrogram(all_clusters))
    by_strain_df <- by_strain_df %>% mutate(row_num = row_number())
    return(by_strain_df[order_rows, ])
  })
  
  #TODO: change so user can choose which option goes on the bottom and so it is universal!! (ie. data_for_plots()$source.specific_1)
  # stack_levels = c("Buffalo", "Cow", "Horse", "Raccoon", "Sheep", "Skunk", "Turkey", "Water", "Chicken")
  stack_levels = reactive({
    data <- data_inputted() %>% filter(source.specific_1 != "Human") 
    unique(data$source.specific_1)
  })
  
  
  re_long_df <- reactive ({
    cgfdf <- data_for_plots() %>%
      filter(source.specific_1 != "Human") %>%
      select(cgf.type, strain.name, source.specific_1) %>%
      group_by(cgf.type) %>% mutate(cgf_count = n()) %>%
      group_by(cgf.type, source.specific_1) %>%
      mutate(type_count = n(), prop = n()/cgf_count)
    per_cgf_df <- cgfdf %>% group_by(cgf.type, source.specific_1, type_count) %>% summarise()
    long_df <- merge(cgfdf, per_cgf_df, by.x = "cgf.type", by.y = "cgf.type")
    long_df <- expandRows(long_df, 'type_count.y') %>% filter(cgf_count > input$min_isol_rm)
    return(long_df)
  })
  
  # re_order <- reactive ({
  #   cast_df <- dcast(re_long_df(), strain.name + cgf.type + cgf_count ~ source.specific_1.y)
  #   cast_df <- cbind(id = cast_df[, c(1,2)], cast_df[, c(-1,-2,-3)]/rowSums(cast_df[, c(-1,-2,-3)]))
  #   clusters <- hclust(dist(cast_df[ , c(-1, -2)]), method = input$clustering_method)
  #   order_ind <- order.dendrogram(as.dendrogram(clusters))
  #   return (cast_df[order_ind, ])
  # })
  
  re_order <- reactive ({
    print(re_long_df())
    cast_df <- dcast(re_long_df(), strain.name + cgf.type + cgf_count ~ source.specific_1.y)
    print('!!!')
    print(cast_df)
    
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
  
  #FOR NON ESTIMATED PLOTTING
  re_non_est_plot <- reactive({
    ggplot(re_long_df(), aes(x=strain.name, y=1, width=1.0,
                             ordered=TRUE)) +
      geom_bar(aes(fill = factor(source.specific_1.y, levels = stack_levels())),
               position=position_fill(), stat="identity", lwd=0.01, color="white") +
      scale_y_continuous(labels = scales::percent) +
      labs(x="Strain", y="Proportion of Sources", fill="Source") +
      theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
      scale_fill_brewer(palette = "RdYlBu", drop = FALSE) +
      scale_x_discrete(limits = re_order()$strain.name)
  })
  
  re_est_plot <- reactive({
    ggplot(long_data_with_estimates(), aes(x=row_num, y=1, width=1.0,
                                           ordered=TRUE)) +
      geom_bar(aes(fill = factor(source.specific_1, levels = stack_levels())), 
               position=position_fill(), stat="identity", lwd=0.01, color="white") +
      scale_y_continuous(labels = scales::percent) +
      labs(x="Strain", y="Proportion of Sources", fill="Source") +
      theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
      scale_fill_brewer(palette = "RdYlBu", drop = FALSE) +
      scale_x_discrete(limits = re_get_order()$row_num)
  })
  
  
  output$freq_width_plot <- renderPlot ({
    
    if (input$do_estimation) {
      plot(re_est_plot())
    }
    else {
      plot(re_non_est_plot())
    }
    
  })
  
  output$all_data_table <- DT::renderDataTable({
    if (input$do_estimation) {
      all_data <- wide_data_with_estimates() %>%
        mutate("Columns used for Estimation: " = sapply(perc_sim, function(x) { if (is.na(x)) {return("NA")} else {re_data_before_estimates()[x,"cgf.type"]}}),
               "% Similarity Used:" = sim_used,
               "Total Strains with Human" = total_with_hum,
               "Total Strains without Human" = total_no_hum,
               "Total Strains in Estimation" = new_total,
               "Fingerprint" = Pattern) %>%
        select(-c(Pattern, perc_sim, total_with_hum, total_no_hum, sim_used, new_total))
      
      DT::datatable(all_data, options = list(scrollX = TRUE))
    }
    else {
      if (length(re_order()) == 1) {
        
        DT::datatable(re_order(), options = list(scrollX = TRUE))
      }
      else {
        dataset_full <- re_order() %>% group_by(cgf.type) %>% mutate(total = n()) %>% select(-strain.name)
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
      }
    }
  })
  
  # observe({
    # if (re_graph_type() == 'Frequency of isolates shown using bar chart') {
    #   callModule(barChartStackogram, "attr_est_bar_graph", data_with_estimates())
    # }
    # else {
      # callModule(stackWidthEstAttr, "attr_est_wide_graph", data_with_estimates())
    # }
  # })
}



























# test <- cgfdb_testset2 %>% unite(col = "Pattern", grep("cj", colnames(cgfdb_testset2)), sep = " ")
# test <- dcast(test, cgf.type + Pattern ~ source.specific_1)
# test$Human <- NULL
# test <- test %>% mutate(total = rowSums(test[, -c(which(colnames(test) == "cgf.type"), which(colnames(test) == "Pattern"))]))
# test_low_total <- test %>% filter(total < 10)
# 
# tester <- test_low_total %>%
#   mutate(perc_sim = lapply(test_low_total$Pattern, function(a) which((40 - mapply(function(x,y) sum(x!=y), strsplit(as.character(test$Pattern), " "), strsplit(a, " ")))/40 > 0.95)))
# tester <- tester %>% mutate(new_total = as.integer(lapply(tester$perc_sim, function(x) tail(cumsum(lapply(x, function(y) test[y, "total"][[1]])), n=1))))
# 
# col_names <- colnames(tester[ , -c(which(colnames(tester) == "cgf.type"), which(colnames(tester) == "Pattern"), which(colnames(tester) == "total"), which(colnames(tester) == "new_total"), which(colnames(tester) == "perc_sim"))])
# lo_test <- lapply(
#   tester$perc_sim,
#   function(x) {
#     lapply(col_names, function(y) {
#       sum(test[x, y])
#     })
#   }
# )
# 
# est_data <- data.frame(matrix(unlist(lo_test), nrow = nrow(tester), byrow=T), stringsAsFactors = FALSE)
# est_data <- est_data/rowSums(est_data)
# est_data[is.na(est_data)] <- 0
# colnames(est_data) <- col_names
# est_data <- est_data %>% mutate(cgf.type = tester$cgf.type, Pattern = tester$Pattern, total = tester$new_total)
# 
# already_good_data <- test %>% filter(total >= 10)
# good_prop <- sweep(already_good_data %>% select(-cgf.type, -Pattern, -total), MARGIN = 1, FUN="/", STATS = rowSums(already_good_data %>% select(-cgf.type, -Pattern, -total)))
# good_data <- cbind(already_good_data%>%select(cgf.type, Pattern, total), good_prop)
# all_data <- merge(good_data, est_data, all.x=TRUE, all.y=TRUE)
# expanded_data <- expandRows(all_data, "total")
# 
# clusters <- hclust(dist(expanded_data[ , c(-which(colnames(wide_data_with_estimates()) == "cgf.type"), 
#                                             -which(colnames(wide_data_with_estimates()) == "Pattern"), 
#                                             -which(colnames(wide_data_with_estimates()) == "row_num"))]), method = "average")
# order_ind <- order.dendrogram(as.dendrogram(clusters))
# new <- expanded_data[order_ind, ]
# new %>% mutate(row_num = row_number())