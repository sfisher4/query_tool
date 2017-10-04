library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(magrittr)
library(stringr)
library(ggpubr)

#ASK:
# Currently my rank is integer based and doesn't have '.5' if there is >1 with that particular # of isolates. Still correctly ranks them though

# helpers
perc_sim <- function(text, min_sim, min_iso) {
  #CONSTRAINTS: The Pattern must start on the 2nd column and go to the 41st column.
  cgfdb <- cgfdb_testset2 %>%
    unite(col = "Pattern", 2:41, sep = " ")
  temp_diff <- t(mapply('==', 
                        strsplit(as.character(text), split = " "), 
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
  
  #TODO: change so user has option for output
  # data <- cgfdb %>% select(-logical, -Pattern)
  data <- cgfdb %>% select(strain.name, cgf.type, perc_sim, num_isol, rank,
                           typing.lab, dataset.id_2, sample.origin, dataset.id_1,
                           sample.type, source.general, source.specific_1, country,
                           province, city, date.collected, campy.species, num_F)
  data[data$perc_sim >= as.numeric(min_sim) & data$num_isol >= as.numeric(min_iso), ]
}

#Stacked Barplot
stacked_levels = c("Human", "Buffalo", "Cow", "Horse", "Raccoon", "Sheep", "Skunk", "Turkey", "Water", "Chicken")

# server
server <- function(input, output, session) {
  re_min_sim <- reactive({input$min_perc_sim})
  re_pattern <- reactive({input$pattern})
  re_min_isol <- reactive({input$min_iso})
  re_data_perc_sim <- reactive({perc_sim(re_pattern(), re_min_sim(), re_min_isol())})
  re_data_by_cgf_type <- reactive({
    re_data_perc_sim() %>%
      group_by(cgf.type, num_isol, rank) %>%
      summarise()
    })
  re_view <- reactive({
    switch(input$view_type,
           "strain.name" = re_data_perc_sim(),
           "cgf.type" = re_data_by_cgf_type())
    })
  
  #TAB1
  output$table <- DT::renderDataTable(DT::datatable({
    data <- re_view()
    data
  }))
  
  #TAB3
  fillChosen <- reactive({input$fill_chosen})
  fillInput <- reactive({
    switch(fillChosen(),
           "typing.lab" = re_data_perc_sim()$typing.lab,
           "sample.origin" = re_data_perc_sim()$sample.origin,
           "sample.type" = re_data_perc_sim()$sample.type,
           "source.general" = re_data_perc_sim()$source.general,
           "source.specific_1" = re_data_perc_sim()$source.specific_1,
           "province" = re_data_perc_sim()$province
           )
  })
  
  output$cgftypes_plot <- renderPlot({
    bar_plot <- ggplot(re_data_perc_sim(), aes(x=cgf.type)) +
      geom_bar(aes(fill = fillInput())) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "CGF Type", y = "Number of Isolates") + 
      ggtitle("Number of Isolates vs CGF Types") + 
      scale_fill_discrete(name = fillChosen())
    plot(bar_plot)
  })
  
  #TAB4
  re_cgf_type <- reactive({input$cgf_type_in})
  re_cgf_type_df <- reactive ({
    re_data_perc_sim() %>% filter(cgf.type == as.character(re_cgf_type())) 
  })
  re_cgf_count_df <- reactive({
    tempdf <- re_cgf_type_df() %>% group_by(cgf.type) %>% summarise(count = n())
    merge(tempdf, re_cgf_type_df(), by.x = "cgf.type", by.y = "cgf.type")
  })
  
  re_x_axis <- reactive({input$x_axis})
  xaxisInput <- reactive({
    switch(re_x_axis(),
           "typing.lab" = re_cgf_type_df()$typing.lab,
           "sample.origin" = re_cgf_type_df()$sample.origin,
           "sample.type" = re_cgf_type_df()$sample.type,
           "source.general" = re_cgf_type_df()$source.general,
           "source.specific_1" = re_cgf_type_df()$source.specific_1,
           "province" = re_cgf_type_df()$province)
  })
  
  observe({re_data_perc_sim()
    updateSelectInput(session = session,
                      inputId = "cgf_type_in",
                      choices = re_data_perc_sim()$cgf.type)
  })
  
  output$cgftype_plot <- renderPlot({
    bar_plot <- ggplot(re_cgf_count_df(), aes(x = xaxisInput())) + 
      geom_bar(aes(fill = xaxisInput())) + 
      labs(x = re_x_axis(), y = "Number of Isolates") + 
      ggtitle(paste("Number of Isolates vs", re_x_axis())) 
    plot(bar_plot)
  })
  
  # TAB 5
  get_order <- reactive({
    stats <- re_data_perc_sim() %>%
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
  })
  
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
  re_prop_plot <- reactive({
    ggplot(re_data_perc_sim() %>% select(cgf.type, source.specific_1), aes(x=cgf.type, y=1, width=re_width(),
                      # fill= factor(source.specific_1, levels = stacked_levels),
                      ordered=TRUE)) +
      geom_bar(aes(fill = factor(source.specific_1, levels = stacked_levels)), 
               position=position_fill(), stat="identity") +
      scale_y_continuous(labels = scales::percent) +
      labs(x="CGF Type", y = "Proportion", fill="Source") +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = unit(c(1,2,0,1), "cm"),
            legend.position = "none") +
      scale_fill_brewer(palette = "RdYlBu") +
      scale_x_discrete(limits = get_order()$cgf.type)
  })
  re_count_plot <- reactive({
    ggplot(re_data_perc_sim() %>% select(cgf.type, source.specific_1), aes(x=cgf.type, ordered = TRUE)) +
      geom_bar(aes(fill = factor(source.specific_1, levels = stacked_levels)), width = re_width()) + 
      labs(x="CGF Type", y = paste(re_scale(), "(Frequency)"), fill="Source") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.margin = unit(c(0, 1, 1, 1), "cm"), legend.position = "bottom") +
      scale_fill_brewer(palette = "RdYlBu") +
      scale_x_discrete(limits = get_order()$cgf.type) +
      scale_y_continuous(trans=re_scale())
  })
  output$stacked_prop_plot <- renderPlot({
    align_plot <- ggarrange(re_prop_plot(), re_count_plot(), heights = c(2, 4), ncol = 1, nrow = 2, align = "v")
    plot(align_plot)
  })
  
  #Tab 8
  output$comp_graph_plot <- renderPlotly({
    xaxisInput <- reactive({
      switch(input$x_axis_comp,
             "typing.lab" = re_data_perc_sim()$typing.lab,
             "sample.origin" = re_data_perc_sim()$sample.origin,
             "sample.type" = re_data_perc_sim()$sample.type,
             "source.general" = re_data_perc_sim()$source.general,
             "source.specific_1" = re_data_perc_sim()$source.specific_1,
             "province" = re_data_perc_sim()$province)
    })
    comp_plot <- ggplot(re_data_perc_sim(), aes(x=xaxisInput(), fill=source.general)) + 
      geom_bar()
    plot(comp_plot)
  })
}
