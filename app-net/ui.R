shinyUI(fluidPage( 
  titlePanel("Współzasiadanie w składach orzekających w Polsce"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("select.court"),
      helpText("Aplikacja ma na celu zaprezentowanie struktury składów orzekających w polskich sądach. Interesujący nas sąd możemy wybrać z list powyżej.
              W prawej części aplikacji prezentowane są: sieć oraz Top chart sędziów  danym sądzie. Ponadto w zakładce Statystyki możemy zobaczyć podstawową analizę sieci.
               "),
      helpText("W zakładce 'Sieć współzasiadania' zobaczyć możemy sieć złożoną z sędziów orzekających w danym sądzie.
               Każdy wierzchołek tej sieci reprezentuje jednego sędziego. Połączenie pomiędzy wierzchołkami oznacza, że dwóch konkretnych sędziów
               zasiadało w tym samym składzie sędziowskim przynajmniej jeden raz. 
               Każdy wierzchołek jest wypełniony kolorem (bądź kolorami) Każdt kolor odnosi się do wydziału/ izby w której dany sędzia orzekał.
               Kolor obramowania wierzchołka opisuje płeć sędziego"),
      helpText("Wydziały są zgrupowane w jeden jeżeli zajmują się tym samym typem spraw (np. Wydział I i II Karny jako Wydział Karny)"),
      helpText("Wykres typu 'Top chart' prezentuje dziesięciu sędziów orzekających w danym sądzie z największą liczbą orzeczeń")
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("Testy",dataTableOutput("table1")),#,textOutput("text1"), textOutput("times"),,dataTableOutput("table1"),dataTableOutput("table2"),dataTableOutput("table3")),
        tabPanel("Sieć współzasiadania",imageOutput("pieImage")),
        tabPanel("Top chart",  imageOutput("topImage")),
        navbarMenu("Statystyki",
                   #tabPanel("Multi",plotOutput("plot.multi")),
                   tabPanel("Liczba orzeczeń w czasie",plotOutput("plot.judgments"),helpText("Opis wykresu...")),
                   tabPanel("Liczba sędziów w czasie",plotOutput("plot.judges"),helpText("Opis wykresu...")),
                   tabPanel("Typy składów orzekających",plotOutput("plot.team.types"),helpText("Opis wykresu...")),
                   tabPanel("Wielkości składów orzekających",plotOutput("plot.team.size"),helpText("Opis wykresu...")),
                   tabPanel("Rozkład k",plotOutput("plot.k"),helpText("Opis wykresu...")),
                   tabPanel("Rozkład w",plotOutput("plot.w"),helpText("Opis wykresu..."))
        )
      )
    )
  )
)
)