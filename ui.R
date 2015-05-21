shinyUI(fluidPage( 
  titlePanel("Współzasiadanie w składach orzekających w Polsce"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("select.court"),
      helpText("Aplikacja ma na celu zaprezentowanie struktury składów orzekających w polskich sądach. Interesujący nas sąd możemy wybrać z list powyżej.
              W prawej części aplikacji, w kolejnych zakładkach prezentowane są: sieć współzasiadania, wykres 10 sędziów orzekających w największej liczbie
               spraw w danym sądzie oraz kilka statystyk dla wybranej sieci. Opisy modułów znajdują się w odpowiednich zakładkach."),
      br(),br(),
      img(src = "icm_logo.png", height = 100*0.75, width = 156*0.75)
      
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("Testy",dataTableOutput("table1"),dataTableOutput("table2"),dataTableOutput("table3"),dataTableOutput("table4")),#,textOutput("text1"), textOutput("times"),,dataTableOutput("table1"),dataTableOutput("table2"),dataTableOutput("table3")),
        tabPanel("Sieć współzasiadania",imageOutput("pieImage"),
                 br(),br(),br(),br(),br(),br(),br(),br(),br(),br(), br(),br(),br(),br(),br(),br(),br(),
                 helpText("Powyższy rysunek przedstawia sieć złożoną z sędziów orzekających w wybranym sądzie.
               Każdy wierzchołek tej sieci reprezentuje jednego sędziego. Połączenie pomiędzy wierzchołkami oznacza, że dwóch konkretnych sędziów
               zasiadało w tym samym składzie orzekających przynajmniej jeden raz. 
               Każdy wierzchołek jest wypełniony kolorem (bądź kolorami), które odnoszą się do wydziału/ izby w której dany sędzia orzekał.
               Kolor obramowania wierzchołka określa płeć sędziego"),
                 helpText("Wydziały są zgrupowane w jeden jeżeli zajmują się tym samym typem spraw (np. Wydział I i II Karny jest przedstawiony jako
                          Wydział Karny)")
                 ),
        tabPanel("Sędziowie",  imageOutput("topImage"),
                 br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                 helpText("Wykres prezentuje dziesięciu sędziów orzekających w danym sądzie w największej liczbie spraw. Na osi poziomej
                          odłożona została liczba spraw, w których dany sędzia orzekał.")
                 ),
        navbarMenu("Statystyki",
                   tabPanel("Liczba orzeczeń w czasie",plotOutput("plot.judgments"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            helpText("Na wykresie możemy porównać liczbę orzeczeń wydawanych w kolejnych miesiącach w wybranym przez nas sądzie.
                                     Należy wziąć pod uwagę, że wykres pokazuje tylko orzeczenia które są dostępne przez API, co może wpływać na 
                                     kształt wykresu")),
                   tabPanel("Liczba orzekających sędziów w czasie",plotOutput("plot.judges"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            helpText("Wykres przedstawia liczbe sędziów, którzy byli członkami przynajmniej jednego składu orzekającego na wydanym 
                                      orzeczeniu w kolejnych miesiącach. Należy wziąć pod uwagę, że wykres pokazuje tylko orzeczenia które są 
                                      dostępne przez API, co może wpływać na kształt wykresu")),
                   tabPanel("Typy składów orzekających v.1",plotOutput("plot.team.types"),helpText("Opis wykresu...")),
                   tabPanel("Typy składów orzekających v.2",plotOutput("plot.team.types2"),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            helpText("Wykres przedstawia typy składów orzekających z uwzględnieniem proporcji kobiet do mężczyzn.
                                     Typ składu oznacza skład pod względem kombinacji `liczba kobiet/liczba mężczyzn`.
                                     Kolory i odpowiadające im wysokości słupków oznaczają ile procentowo było składów o danej kombinacji z przewagą kobiet,
                                     a ile z przewagą mężczyzn. Szerokość kolejnych słupków odpowiada ilości orzeczeń wydanych przez odpowiednie
                                     składy sędziowskie. Kolor zielony oznacza składy z przewagą kobiet, niebieski składy z przewagą mężczyzn,
                                     czerwony natomiast składy gdzie nie było przewagi (składy typu 1/1)")),
                   tabPanel("Typy składów orzekających v.2b",plotOutput("plot.team.types2b")),
                   tabPanel("Typy składów orzekających v.3",plotOutput("plot.team.types3"),br(),br(),helpText("Opis wykresu...")),
                   tabPanel("Wielkości składów orzekających",plotOutput("plot.team.size"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            helpText("Na wykresie możemy zobaczyć ile było składów o danej wielkości (pod względem liczby orzekających sędziów)
                                     w danym sądzie. Wysokość słupka odpowiada liczbie składów orzekających o danej wielkości.")),
                   tabPanel("Rozkład k",plotOutput("plot.k"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            helpText("Wykres pokazuje stopnie wierzchołków w sieci współzasiadania sędziów. Stopień wierzchołka odpowiada liczbie
                                     bezpośrednich połączeń danego sędziego z innymi sędziami w danym sądzie. Bezpośrednie połączenie występuje
                                     w przypadku, kiedy dwóch sędziów wydało razem przynajmniej jedno orzeczenie.
                                     Wysokości słupków oznaczają liczbę sędziów o danej liczbie bezpośrednich sąsiadów.")),
                   tabPanel("Rozkład w",plotOutput("plot.w"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            helpText("Wykres przedstawia wagi krawędz w sieci współzasiadania sędziów. Krawędź jest to linia łącząca wierzchołki 
                                     czyli dwóch sędziów. Sędziowie połączeni są krawędzią w przypadku, kiedy wydali razem przynajmniej jedno 
                                     orzeczenie. Waga krawędzi to liczba orzeczeń, przy których wydawaniu uczestniczyło razem dwóch sędziów
                                     oznaczonych przez wierzchołki. Wysokości słupków oznaczają liczbę połączeń o danej wadze. Wykres jest dobrym
                                     odzwierciedleniem skłonności sędziów do utrzymywania stałych składów orzekających."))
        )
      )
    )
  )
)
)