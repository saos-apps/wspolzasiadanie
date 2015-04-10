fluidPage(
  titlePanel("Judging teams in Poland"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("select.court")
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("Tests", textOutput("times"),textOutput("text1"),dataTableOutput("table1"),dataTableOutput("table2"),dataTableOutput("table3")),
        tabPanel("Description", helpText("Description...
               The application has been made to present the structure of judging teams in Polish courts.")),
        tabPanel("Net mark", plotOutput("plot.graph")),
        tabPanel("Net pie", plotOutput("plot.pie")),
         tabPanel("Stats",plotOutput("plot.multi")),
         tabPanel("Top chart",  plotOutput("plot.top.chart"))
      )
    )
  )
)