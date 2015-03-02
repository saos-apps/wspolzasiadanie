fluidPage(
  titlePanel("Wspólpraca sędziów w Polsce"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Opis aplikacji... W aplikacji chodzi o to, żeby pokazać sieć jak dani sędziowie współpracują. Nie wiem jaka jest praktyka, ale
               może jest tak że sędziowie tworzą różne składy. Krawędź oznacza min. 1 wspólną sprawę.
               Wskaźnik współpracy = l.krawędzi / l. możliwych krawędzi (w danym sądzie i wydziale"),
      selectInput("select.court",label=h3("Wybierz sąd:"),choices=as.list(locations$name)),
      #selectInput("select.div",label=h3("Wybierz wydział/izbę:"),choices=as.list(locations$courts.name)),
      uiOutput("select.division"),
      selectInput("select.year",label=h3("Wybierz rok"),choices=as.list(seq(1990,2015)))
    ),
    mainPanel(
      #dataTableOutput("text"),
     # textOutput("text2"),
      plotOutput("plot"),
      htmlOutput("plot2")
    )
    
  )
)