fluidPage(
  titlePanel("Mapa sądów w Polsce"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Opis aplikacji..."),
      selectInput("select.type",label=h3("Wybierz dane do wyświetlenia"),choices=list("Obciążenie sądów (liczba orzeczeń/sędzia)"=1,
                                                                                      "Całkowita liczba orzeczeń"=2,"mobilność sędziów"=3,"liczba sędziów"=4)),
      helpText("Wybierz zakres dat:"),
      selectInput("year.from",label=h3("od roku:"),choices=as.list(seq(2010,2015))),
      selectInput("year.too",label=h3("do roku:"),choices=as.list(seq(2010,2015))),
      #dateRangeInput("dates",label=h3("Wybierz zakres dat:"),start=1990,end=2015,format="yyyy",startview="decade"),
      selectInput("select.type",label=h3("Wybierz typ orzeczenia:"),choices=types.list)
    ),
    mainPanel(
      #textOutput("text2"),
      htmlOutput("plot"),
      htmlOutput("plot2"),
      dataTableOutput("text")
    )
    
  )
)