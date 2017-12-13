
input_example ="0 0 1 1 1 1 0 1 1 1 1 0 0 0 0 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 0 0 1 1 0 1 0 0 0"


ui <- fluidPage(
   sidebarPanel(
     headerPanel("Query Tool"),
     subsetDataInput("sidebar")
   ),
   mainPanel(
     tabsetPanel(
       tabPanel("Data Table",
                 mainPanel(
                   fluidRow(
                     DT::dataTableOutput(outputId = "table")
                   ),
                   fluidRow(
                     column(12, selectInput(inputId = "view_type_in",
                                           label = "View: ",
                                           choices = c("By Strain" = "strain", "Aggregate" = "aggregate")))
                   ),
                   fluidRow(
                     column(6, uiOutput("column_display_ui")),
                     conditionalPanel(condition = "input.view_type_in == 'aggregate'",
                      column(6, uiOutput("agg_by"))
                     )
                   )
                 )
        ),
        # tabPanel("Individual CGF Type",
        #          headerPanel("Bar Graph of Individual CGF Type"),
        #          fluidRow(
        #            column(12, 
        #            uiOutput("fill_value_ui"))
        #          ),
        #          fluidRow(
        #            plotOutput(outputId = "cgftypes_plot")
        #          )
        # ),
        tabPanel("General Distributions",
                 # titlePanel("General Distributions"),
                 fluidRow(
                   selectInput(inputId = "graph_type",
                               label = "Choose Type of Graph to Display:",
                               choices = c("Frequency of isolates shown using bar chart",
                                           "Frequency of isolates shown as width of bars"),
                               selected = "Frequency of isolates shown as width of bars")
                 ),
                 
                 conditionalPanel(condition =  "input.graph_type == 'Frequency of isolates shown using bar chart'",
                                  barChartStackogramUI("bar_graph_all")
                 ),
                 
                 conditionalPanel(condition = "input.graph_type == 'Frequency of isolates shown as width of bars'",
                                  propBarWidthUI("wide_graph_all")
                 )
                 
        ),
        tabPanel("Aggregate View",
                 aggViewUI("agg_view")
        ),
       tabPanel("Source Attribution",
                attrEstStackogramUI("attr_est")
       ),
       tabPanel("Risk Assessment",
                riskogramUI("riskogram")
       )
      )
   )
)