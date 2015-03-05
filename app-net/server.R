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
#locations<-read.table("data/locations.csv",stringsAsFactors = F)
#net<-read.table("data/net.csv",stringsAsFactors = F)
#divisions<-read.table("data/divisions.csv",stringsAsFactors = F)
#table1<-read.table("data/judges.csv",stringsAsFactors = F)

judgments<-read.table("data/judgments.csv")
judges<-read.table("data/judges.csv")
divisions<-read.table("data/divisions.csv")
judges.net<-read.table("data/judges.net.csv")

shinyServer(function(input, output) {

  output$select.court<-renderUI({
    courts.un<-divisions[!duplicated(divisions$CourtCode),]
    list1<-as.list(courts.un$CourtCode)
    names(list1)<-courts.un$CourtName
    selectInput("select.court",label=h3("Wybierz sąd:"),choices=list1)
  })
  
  output$select.division<-renderUI({
    divisions.subset<-subset(divisions,CourtCode==input$select.court)
    list1<-as.list(divisions.subset$DivisionCode)
    names(list1)<-divisions.subset$DivisionName
    selectInput("select.div",label=h3("Wybierz wydział/izbę:"),choices=list1)
  })
  
  subset.judgments<-reactive({
    subset(judges.net,CourtCode==input$select.court & DivisionCode==input$select.div )
    #g<-graph.data.frame(s,directed = F,vertices=NULL)
    #g<-simplify(g,remove.multiple = T,remove.loops = T,edge.attr.comb = "first")
    #s2<-get.data.frame(g)
  })
  
  subset.judges<-reactive({
    subset(judges,CourtCode==input$select.court & DivisionCode==input$select.div )
  })
  
  
  
  
  
#   no.judges.year<-reactive({
#     judges.year<-count(judges,c("year","name"))
#     names(judges.year)[3]<-"a"
#     judges.year<-count(judges.year,"year")
#     judges.year<-judges.year[order(judges.year$year),]
#   })
#   
#   division.coop<-reactive({
#     coop<-count(subset.division(),"year")  
#     no.judges<-subset(no.judges.year(),year %in% coop$year)
#     coop<-transform(coop,wsp=coop$freq/choose(no.judges$freq,2))
#     coop
#   })
  
  subgraph.division<-reactive({
    g<-graph.data.frame(subset.judgments(),directed = F,vertices=NULL)
    g
  })
  
  subgraph.simplified<-reactive({
    g<-simplify(subgraph.division(),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
    E(g)$weight<-sapply(E(g)$CourtCode,length)
    g
  })
  
  k.dist<-reactive({
    k<-degree(subgraph.simplified())
    as.data.frame(k)
  })
  
  w.dist<-reactive({
    data.frame(w=E(subgraph.simplified())$weight)
  })
  
  trans.dist<-reactive({
    t<-transitivity(subgraph.simplified(),"local",isolates = "zero")
    data.frame(clust=t)
  })
  
  max.component<-reactive({
    g<-subgraph.division()
    g.comp<-t(sapply(unique(E(g)$judgmentYear),function(x) {
      g.sub<-subgraph.edges(g,E(g)[E(g)$judgmentYear==x],delete.vertices = T)
      g.sub<-simplify(g.sub)
      cl<-clusters(g.sub)
      c(x,max(cl$csize)/vcount(g.sub))
    }))
    g.comp<-as.data.frame(g.comp)
    names(g.comp)<-c("year","size.max.component")
    g.comp
  })
  
judges.year<-reactive({
  df<-count(subset.judges(),"judgmentYear")
  names(df)<-c("year","number.judges")
  df
})

  #output$table1<-renderDataTable({subset.division()})
  output$table1<-renderDataTable({as.data.frame(E(subgraph.division())$judgmentYear)})
  output$text1<-renderText({})
  output$plot.net <- renderPlot({
    lay<-layout.fruchterman.reingold(subgraph.simplified())
    plog(subgraph.simplified(),lay)
  },width=400,height=400)
  
  plot.k <- reactive({
    ggplot(k.dist(),aes(x=k))+geom_histogram()
  })
  
  plot.w <- reactive({
    ggplot(w.dist(),aes(x=w))+geom_histogram()
  })
  
  plot.t <- reactive({
    ggplot(trans.dist(),aes(x=clust))+geom_histogram()
  })
  
  plot.comp <- reactive({
    ggplot(max.component(),aes(x=year,y=size.max.component)) + geom_line()
  })
  
  plot.judges <- reactive({
    ggplot(judges.year(),aes(x=year,y=number.judges))+geom_line()
  })

  output$plot.multi<-renderPlot({
    multiplot(plot.comp(),plot.judges(),plot.k(),plot.w(),plot.t(),cols=2)
  })
#   output$plot2<-renderGvis({
#     #no.rand<-data.frame(year=seq(1990,2015,length.out=26),number=seq(30,100,length.out = 26))
#     g<-gvisLineChart(division.coop(),xvar="year",yvar="wsp",chartid = "Chart1")
#     return(g)
#   })
  
})