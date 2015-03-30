fluidPage(
  titlePanel("Wspólpraca sędziów w Polsce"),
  sidebarLayout(
    sidebarPanel(
      helpText("Opis aplikacji...
               Wskaźnik 'współpracy' = l.krawędzi / l. możliwych krawędzi (w danym sądzie i wydziale"),
      uiOutput("select.court")
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("Tests", textOutput("times"),textOutput("text1"),dataTableOutput("table1"),dataTableOutput("table2")),
        tabPanel("Net mark", plotOutput("plot.graph")),
        tabPanel("Net pie", plotOutput("plot.pie")),
         tabPanel("Stats",plotOutput("plot.multi")),
         tabPanel("Top chart",  plotOutput("plot.top.chart"))
      )
    )
  )
)