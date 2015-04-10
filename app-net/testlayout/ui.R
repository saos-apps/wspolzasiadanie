shinyUI(fluidPage( 
  titlePanel("Judging teams in Poland"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("select.court")
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("Tests", textOutput("times"),textOutput("text1"),dataTableOutput("table1"),dataTableOutput("table2"),dataTableOutput("table3")),
        tabPanel("Description", helpText("
               The application has been made to present the structure of judging teams in Polish courts. We can choose the court from the list in the sidebar on the left.
                The 'Net mark' and 'Net pie' bookmarks show the network od judges in chosen court. Each node means judge and link between two judges
                means that they were sitting in the same judging team at least once. Every color of node/mark in the background coresponds with different division/chamber 
                in the court. Divisions are grouped into one color if they deal with the similar cases (e.g. Civil, Insurances etc.).
                The shape of the node corresponds to sex of the judge (circle - woman,triangle - man).")                
                 ),
        tabPanel("Net mark",
                 fluidRow(
                   column(6,align="left",
                          plotOutput("plot.net")),
                   column(5,align="right",
                          plotOutput("plottemp")))
                
        ),
        tabPanel("Net pie",
                            fluidRow(
                              column(6,align="left",
                                     plotOutput("plot.pie")),
                              column(5,align="right",
                                     plotOutput("plot.legend")))
                 ),
        tabPanel("Stats",plotOutput("plot.multi")),
        tabPanel("Top chart",  plotOutput("plot.top.chart"))
      )
    )
  )
)
)