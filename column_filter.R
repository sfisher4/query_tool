
columnFilterUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      uiOutput(outputId = "filter")
    )
  )
}

columnFilter <- function(input, output, session, column_name, original_data) {
  re_col_filter <- reactive({input$column_filter_by})
  
  output$filter <- uiOutput({
    pickerInput(inputId = ns("column_filter_by"),
                # label = "Choose options to filter by: ", #update label later
                options = list('actions-box' = TRUE),
                choices = unique(original_data$typing.lab),
                multiple = TRUE)
  })
  
  new_data <- reactive({
    original_data[original_data$typing.lab %in% re_col_filter(), ]
  })
  
  return(new_data)
}