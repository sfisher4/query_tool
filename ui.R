library(shiny)


text ="0 0 1 1 1 1 0 1 1 1 1 0 0 0 0 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 0 0 1 1 0 1 0 0 0"

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Query Tool Setup",
             headerPanel("Fingerprint Query Tool"),
             fluidRow(
               column(3, textInput(inputId = "pattern",
                                   label = "CGF Pattern to Search: ",
                                   value = text)),
               column(3, selectInput(inputId = "min_perc_sim",
                                     label = "Minimum % Similarity:",
                                     choices = seq(0, 100, 5),
                                     selected = 85)),
               column(3, textInput(inputId = "min_iso",
                                   label = "Minimum # of Isolates:",
                                   value = 10))
             ),
             fluidRow(
               column(3, h3("Summary of Data:"),
                      offset = 0.25)
             ),
             fluidRow(
               column(3, selectInput(inputId = "view_type",
                                     label = "View by: ",
                                     choices = c("strain.name", "cgf.type")))
             ),
             fluidRow(
               DT::dataTableOutput(outputId = "table")
             )
    ),
    tabPanel("Pattern Match",
             headerPanel("Pattern Match")
    ),
    tabPanel("Metadata",
             headerPanel("Metadata")
    ),
    tabPanel("All CGF Types",
             headerPanel("Bar Graph of all CGF Types"),
             fluidRow(
               column(3, selectInput(inputId = "fill_chosen",
                                     label = "Fill Value:",
                                     choices = c("typing.lab", "sample.origin", "sample.type", "source.general",
                                                 "source.specific_1", "province"),
                                     selected = "typing.lab"))
             ),
             fluidRow(
               plotOutput(outputId = "cgftypes_plot")
             )
    ),
    tabPanel("Individual CGF Types",
             fluidRow(
               headerPanel("Bar Graph of Individual CGF Type"),
               column(3, selectInput(inputId = "cgf_type_in",
                                     label = "CGF Type",
                                     choices = NULL)),
               column(3, selectInput(inputId = "x_axis",
                                     label = "X-axis:",
                                     choices = c("typing.lab", "sample.origin", "sample.type", "source.general",
                                                 "source.specific_1", "province"),
                                     selected = "typing.lab"))
             ),
             fluidRow(
               plotOutput(outputId = "cgftype_plot")
             )
    ),
    tabPanel("Proportion of Sources",
             titlePanel("Proportion and Frequency of Sources vs CGF Type"),
             fluidRow(
               plotOutput("stacked_prop_plot")
             ),
             fluidRow(
               column(4, 
                      sliderInput(inputId = "width",
                                  label = "Width of Bars:",
                                  min = 0.8,
                                  max = 1.0,
                                  value = 0.968),
                      offset = 1),
               column(4,
                      radioButtons(inputId = "scale",
                                   label = "Frequency Graph Scale:",
                                   choices = c("linearly", "log10", "sqrt")),
                      offset = 2)
             )
    ),
    tabPanel("Source vs Sample",
             titlePanel("Percentage of Source vs Percentage of Sample for CGF Types")
    ),
    tabPanel("Composition Graphs",
             titlePanel("Composition Graph"),
             fluidRow(
               column(3, selectInput(inputId = "x_axis_comp",
                                     label = "X-axis:",
                                     choices = c("typing.lab", "sample.origin", "sample.type", "source.general",
                                                 "source.specific_1", "province"),
                                     selected = "typing.lab"))
             ),
             fluidRow(
               plotlyOutput("comp_graph_plot")
             )
    )
  )
)