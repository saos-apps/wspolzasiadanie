require(shiny)
library(saos)
require(googleVis)
require(ggplot2)
require(knitr)
require(WDI)
require(RgoogleMaps)
require(igraph)
# server.R
source("funkcje.R")
locations<-read.table("data/locations.csv",stringsAsFactors = F)
net<-read.table("data/net.csv",stringsAsFactors = F)
divisions<-read.table("data/divisions.csv",stringsAsFactors = F)
table1<-read.table("data/judges.csv",stringsAsFactors = F)

shinyServer(function(input, output) {

#  all.courts<-reactive({get_dump_courts(T)})
  output$select.division<-renderUI({
    divisions.un<-divisions[!duplicated(divisions$name),]
    list1<-as.list(divisions.un$division.id)
    names(list1)<-divisions.un$name
    selectInput("select.div",label=h3("Wybierz wydział/izbę:"),choices=list1)
  })
  
  subset.division<-reactive({
    s<-subset(net,divisionid==as.numeric(input$select.div),select=c("name1","name2","year"))
    g<-graph.data.frame(s,directed = F,vertices=NULL)
    g<-simplify(g,remove.multiple = T,remove.loops = T,edge.attr.comb = "first")
    s2<-get.data.frame(g)
  })
  
  no.judges.year<-reactive({
    judges.year<-count(table1,c("year","name"))
    names(judges.year)[3]<-"a"
    judges.year<-count(judges.year,"year")
    judges.year<-judges.year[order(judges.year$year),]
  })
  
  division.coop<-reactive({
    coop<-count(subset.division(),"year")  
    no.judges<-subset(no.judges.year(),year %in% coop$year)
    coop<-transform(coop,wsp=coop$freq/choose(no.judges$freq,2))
    coop
  })
  
  
  subgraph.division<-reactive({
    g<-graph.data.frame(subset.division(),directed = F,vertices=NULL)
    g<-simplify(g,remove.multiple = T,remove.loops = T)
    g
  })
  
  #output$text<-renderDataTable({subset.division()})
  #output$text2<-renderText({input$select.div})
  output$plot <- renderPlot({
    layg<-layout.fruchterman.reingold(subgraph.division())
    plog2(subgraph.division(),layg)
  },width=400,height=400)
  
  output$plot2<-renderGvis({
    #no.rand<-data.frame(year=seq(1990,2015,length.out=26),number=seq(30,100,length.out = 26))
    g<-gvisLineChart(division.coop(),xvar="year",yvar="wsp",chartid = "Chart1")
    return(g)
  })
  
})