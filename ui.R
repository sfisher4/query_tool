
text ="0 0 1 1 1 1 0 1 1 1 1 0 0 0 0 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 0 0 1 1 0 1 0 0 0"


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
                     column(3, selectInput(inputId = "view_type_in",
                                           label = "View: ",
                                           choices = c("By Strain" = "strain", "Aggregate" = "aggregate"))),
                     column(8, uiOutput("column_display_ui")
                     )
                   ),
                   fluidRow(
                     uiOutput("agg_by")
                   )
                 )
        ),
        tabPanel("Pattern Match",
                 headerPanel("Pattern Match")
        ),
        tabPanel("Metadata",
                 headerPanel("Metadata")
        ),
        tabPanel("Individual CGF Type",
                 headerPanel("Bar Graph of Individual CGF Type"),
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
        tabPanel("Aggregate CGFTypes",
                 fluidRow(
                   headerPanel("Bar Graph of Aggregated CGFTypes"),
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
                   selectInput(inputId = "graph_type",
                               label = "Choose Type of Graph to Display:",
                               choices = c("Frequency of isolates shown using bar chart",
                                           "Frequency of isolates shown as width of bars"),
                               selected = "")
                 ),
                 
                 conditionalPanel(condition =  "input.graph_type == 'Frequency of isolates shown using bar chart'",
                                  barChartStackogramUI("bar_graph_all")
                 ),
                 
                 conditionalPanel(condition = "input.graph_type == 'Frequency of isolates shown as width of bars'",
                                  propBarWidthUI("wide_graph_all")
                 )
                 
        ),
        tabPanel("Risk Assessment",
                 riskogramUI("riskogram")
         
        ),
       tabPanel("Stackogram with Attribute Estimation",
                attrEstStackogramUI("attr_est"))
     )
   )
)