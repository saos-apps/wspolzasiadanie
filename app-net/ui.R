fluidPage(
  titlePanel("Wspólpraca sędziów w Polsce"),
  sidebarLayout(
    sidebarPanel(
      helpText("Opis aplikacji...
               Wskaźnik 'współpracy' = l.krawędzi / l. możliwych krawędzi (w danym sądzie i wydziale"),
      uiOutput("select.court")
      #selectInput("select.court",label=h3("Wybierz sąd:"),choices=as.list(locations$name)),
      #selectInput("select.div",label=h3("Wybierz wydział/izbę:"),choices=as.list(locations$courts.name)),
      #uiOutput("select.division")
     # selectInput("select.year",label=h3("Wybierz rok"),choices=as.list(seq(1990,2015)))
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("Tests", textOutput("text1"),dataTableOutput("table1")), 
        tabPanel("Net mark", plotOutput("plot.graph")), 
        tabPanel("Net pie", plotOutput("plot.pie")), 
         tabPanel("Stats",plotOutput("plot.multi")),
         tabPanel("Top chart",  plotOutput("plot.top.chart"))
      )
    )
    
#     mainPanel(
#       #dataTableOutput("text"),
#       textOutput("text1"),
#       #plotOutput("plot.net"),
#       plotOutput("plot.graph"),
#       #plotOutput("plot.comp"),
#       #plotOutput("plot.judges"),
#       plotOutput("plot.multi"),
#       plotOutput("plot.top.chart")
# #       plotOutput("plot.k"),
# #       plotOutput("plot.w"),
# #       plotOutput("plot.t"),
# #       plotOutput("plot.s"),
# #       plotOutput("plot.comp"),
# #       plotOutput("plot.judges"),
# #       plotOutput("plot.coop"),
# #       plotOutput("plot.judgments")
#       #dataTableOutput("table1")
#     #  htmlOutput("plot2")
#     )
    
  )
)
