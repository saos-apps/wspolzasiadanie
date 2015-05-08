shinyUI(fluidPage( 
  titlePanel("Judging teams in Poland"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("select.court"),
      helpText("
               The application has been made to present the structure of judging teams in Polish courts. Judging teams are divided into specific courts which we can
              choose from the list above."),
      helpText("The 'Network' bookmark show the network od judges in chosen court. Each node means one judge. Link between two judges
                means that they were sitting in the same judging team at least once. Every color of node in the background coresponds with different division/chamber
                in the court. Node frame colour specifies sex of the judge."),
      helpText("Divisions are grouped into one color if they deal with the cases in one type of law (e.g. Civil, Criminal etc.)."),
      helpText("Stats bookmark shows several statistics of the specified network."),
      helpText("Top chart shows top 10 judges in specified court who have top 10 number of judgments in they careers.")
    ),
    mainPanel( 
      tabsetPanel(
        #tabPanel("Tests", textOutput("times"),textOutput("text1")),#,dataTableOutput("table1"),dataTableOutput("table2"),dataTableOutput("table3")),
        tabPanel("Stats",plotOutput("plot.multi")),
        tabPanel("Network",imageOutput("pieImage")),
        tabPanel("Top chart new",  imageOutput("topImage"))
      )
    )
  )
)
)