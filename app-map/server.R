require(shiny)
library(saos)
require(googleVis)
require(ggplot2)
require(knitr)
require(WDI)
require(RgoogleMaps)
require(sqldf)
# server.R
locations<-read.table("data/locations.csv",stringsAsFactors = F)
table1<-read.table("data/judges.csv",stringsAsFactors = F)
dates<-read.table("data/dates.csv",stringsAsFactors = F)
#locations<-read.table("app-map/data/locations.csv",stringsAsFactors = F)

shinyServer(function(input, output) {
#   location<-reactive({
#     read.table("app-map/data/locations.csv")
#   })
#   
  no.judgments.year<-reactive({
    sum.judgments<-count(dates,"year")
  })
  no.judges.year<-reactive({
    judges.year<-count(table1,c("year","name"))
    names(judges.year)[3]<-"a"
    judges.year<-count(judges.year,"year")
    judges.year<-judges.year[order(judges.year$year),]
  })
  no.judgments.judge<-reactive({
    judge.x.case<-count(table1,"year")
    judge.x.case<-judge.x.case[order(judge.x.case$year),]
    transform(judge.x.case,case.per.judge=judge.x.case$freq/no.judges.year()$freq)
  })
  output$text<-renderDataTable({no.judgments.judge()})
  output$text2<-renderText({input$select.type})
  output$plot <- renderGvis({
    g<-gvisGeoMap(locations,"lat.lon",hovervar = "name",numvar="no.judg",options=list(region="155", dataMode="markers",title="Liczba orzeczeń z podziałem na sądy (wszystkie lata)",width=600, height=400))
    return(g)
  })
  output$plot2<-renderGvis({
    g<-gvisLineChart(no.judgments.judge(),xvar="year",yvar="case.per.judge",chartid = "Chart1")
    g<-switch(as.numeric(input$select.type),
           gvisLineChart(no.judgments.judge(),xvar="year",yvar="case.per.judge",chartid = "Chart1"),
           gvisLineChart(no.judgments.year(),xvar="year",yvar="freq",chartid = "Chart1"),
           gvisLineChart(no.judges.year(),xvar="year",yvar="freq",chartid = "Chart1"),
           gvisLineChart(no.judges.year(),xvar="year",yvar="freq",chartid = "Chart1"),
           )
    #ifelse(input$select.type==1,g<-gvisLineChart(no.judgments.judge(),xvar="year",yvar="case.per.judge",chartid = "Chart1"),
     #      g<-gvisLineChart(no.judgments.year(),xvar="year",yvar="freq",chartid = "Chart1")
      #     )
    return(g)
  })
  
})