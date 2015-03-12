require(shiny)
require(saos)
require(googleVis)
require(ggplot2)
require(knitr)
require(WDI)
require(RgoogleMaps)
require(igraph)
require(plyr)
require(data.table)
# server.R
source("funkcje.R")
judgments<-read.table("data/judgments.csv")
judges<-read.table("data/judges.csv")
divisions<-read.table("data/divisions.csv")
judges.net<-read.table("data/judges.net.csv")
courts<-read.table("data/courts.csv")

#funkcje nieużywane na razie zakomentowane (dla wydziału/izby)

shinyServer(function(input, output) {

  output$select.court<-renderUI({
    courts.un<-divisions[!duplicated(divisions$CourtCode),]
    list1<-as.list(courts.un$CourtCode)
    names(list1)<-courts.un$CourtName
    selectInput("select.court",label=h3("Wybierz sąd:"),choices=list1)
  })
  
#   
#   output$select.division<-renderUI({
#     divisions.subset<-subset(divisions,CourtCode==input$select.court)
#     list1<-as.list(divisions.subset$DivisionCode)
#     names(list1)<-divisions.subset$DivisionName
#     selectInput("select.div",label=h3("Wybierz wydział/izbę:"),choices=list1)
#   })
#   
#   
#   subset.judgments<-reactive({
#     subset(judges.net,CourtCode==input$select.court & DivisionCode==input$select.div )
#   })
  
  subset.judgments.court<-reactive({
    subset(judges.net,CourtCode==input$select.court)
  })
  
#   subset.judges<-reactive({
#     subset(judges,CourtCode==input$select.court & DivisionCode==input$select.div )
#   })
  
  subset.judges.court<-reactive({
    subset(judges,CourtCode==input$select.court)
  })
  
  judges.top.court<-reactive({
    temp<-count(subset.judges.court(),"JudgeName")
    temp<-subset(temp,!is.na(temp$JudgeName))
    top<-head(temp[order(temp$freq,decreasing = T),],min(10,nrow(temp)))
    top$JudgeName<-sapply(as.character(top$JudgeName),function(x) paste(unlist(strsplit(x," "))[1:2],collapse=" "))
    table1 <- table(top$JudgeName)
    c<-sapply(seq(min(10,nrow(temp))),function(x) which(names(table1)==top$JudgeName[x]))
    levels1 <- names(table1)[rev(c)]
    top$JudgeName <- factor(top$JudgeName, levels = levels1)
    names(top)[2]<-"N.of.judgments"
    top
  })

#   subgraph.division<-reactive({
#     g<-graph.data.frame(subset.judgments(),directed = F,vertices=NULL)
#     g
#   })
  
  subgraph.court<-reactive({
    #vert<-subset.judges.court()[!duplicated(subset.judges.court()$JudgeName),c("JudgeName","JudgeSex")]
    dt<-data.table(subset.judges.court())
    vert <- dt[, list(JudgeSex=head(JudgeSex,1), DivisionCode=paste(DivisionCode,collapse=" ")), by=c("JudgeName")]
    vert$DivisionCode<-sapply(vert$DivisionCode,function(x) unique(unlist(strsplit(x," "))))
    g<-graph.data.frame(subset.judgments.court(),directed = F,vertices=vert)
    V(g)$vertex.shape<-NA
    V(g)$vertex.shape<-ifelse(V(g)$JudgeSex=="M","ftriangle",ifelse(V(g)$JudgeSex=="F","fcircle","fstar"))
    V(g)$vertex.shape[which(is.na(V(g)$vertex.shape))]<-"fstar"
    g
  })
#   
#   subgraph.simplified<-reactive({
#     g<-simplify(subgraph.division(),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
#     E(g)$weight<-sapply(E(g)$CourtCode,length)
#     g
#   })
  
  subgraph.simplified.court<-reactive({
    g<-simplify(subgraph.court(),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
    E(g)$weight<-sapply(E(g)$CourtCode,length)
    g       
    })
    
  subgraph.mark.list<-reactive({
    
    g<-subgraph.simplified.court()
    div.un<-unique(unlist(V(g)$DivisionCode))
    list<-sapply(div.un,function(x) which(V(g)$DivisionCode==x))
    names(list)<-rainbow(length(div.un))
    list$labels<-div.un
    list
#     
#     g<-subgraph.simplified.court()
#     div.vect<-as.vector(E(g)$DivisionCode)
#     div.un<-unique(unlist(div.vect))
#     ymax<-length(div.un)
#     list<-lapply(seq(ymax),function(y) {
#     which.div<-sapply(seq(ecount(g)),function(x) div.un[y] %in% unique(div.vect[[x]]))
#     v<-get.edges(g,E(g)[which.div])
#     unique(c(v[,1],v[,2]))
#     })
#     names(list)<-rainbow(length(div.un))
#     list$labels<-div.un
#     list
  })
  
  
#   judges.coop.year<-reactive({
#   years<-unique(subset.judges()$judgmentYear)
#   list1<-as.data.frame(t(sapply(years,function(x){
#     sub.judges<-subset(subset.judges(),judgmentYear==x)
#     n.judges<-length(unique(sub.judges$JudgeName))
#     sub.judgments<-subset(subset.judgments(),judgmentYear==x)
#     g.sub<-simplify(graph.data.frame(sub.judgments,directed = F,vertices=NULL),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
#     coop.array<-count(sub.judges,"judgmentID")$freq
#     coop<-ecount(g.sub)/max.unique.links(n.judges,coop.array)
#     c(x,coop)
#   })))
#   names(list1)<-c("year","coop")
#   list1
#   })
  
  judges.coop.year<-reactive({
    years<-unique(subset.judges.court()$judgmentYear)
    if(ecount(subgraph.simplified.court())==0)
      list1=NULL
    else{
    list1<-as.data.frame(t(sapply(years,function(x){
      sub.judges<-subset(subset.judges.court(),judgmentYear==x)
      n.judges<-length(unique(sub.judges$JudgeName))
      sub.judgments<-subset(subset.judgments.court(),judgmentYear==x)
      g.sub<-simplify(graph.data.frame(sub.judgments,directed = F,vertices=NULL),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
      coop.array<-count(sub.judges,"judgmentID")$freq
      coop<-ecount(g.sub)/max.unique.links(n.judges,coop.array)
      c(x,coop)
    })))
    names(list1)<-c("year","coop")
    }
    list1
  })

#   judges.coop<-reactive({
#   n.judges<-length(unique(subset.judges()$JudgeName))
#   coop.array<-count(subset.judges(),"judgmentID")$freq
#   coop<-ecount(subgraph.simplified())/max.unique.links(n.judges,coop.array)
#   paste("vcount:",vcount(subgraph.division()),"ecount",ecount(subgraph.division()),
#         "ecount simplified:",ecount(subgraph.simplified()),"\n coop:",coop,sep="  ")
#   })

  judges.coop.court<-reactive({
    g<-subgraph.simplified.court()
    div.vect<-as.vector(E(g)$DivisionCode)
    div.un<-unique(unlist(div.vect))
    temp<-sapply(seq(length(div.un)),function(x) {
      sub.judgments.div<-subset(subset.judgments.court(),DivisionCode==div.un[x])
      g.div<-simplify(graph.data.frame(sub.judgments.div,directed = F,vertices=NULL))
      sub<-subset(subset.judges.court(), DivisionCode==div.un[x])
      n.judges<-length(unique(sub$JudgeName))
      coop.array<-count(sub,"judgmentID")$freq
      coop<-ecount(g.div)/max.unique.links(n.judges,coop.array)
    })
    paste("vcount:",vcount(subgraph.court()),"ecount",ecount(subgraph.court()),
          "ecount simplified:",ecount(subgraph.simplified.court()),"\n coop:",paste(temp,sep=";"),sep="  ")
})
  
  s.dist<-reactive({
    s<-count(subset.judges.court(),"judgmentID")$freq
    as.data.frame(s)
  })
  
  k.dist<-reactive({
    k<-as.vector(degree(subgraph.simplified.court()))
    as.data.frame(k)
  })
  
  w.dist<-reactive({
    
    if(ecount(subgraph.simplified.court())==0) w=0 else w=as.vector(E(subgraph.simplified.court())$weight)
    data.frame(w=w)
  })
  
#   trans.dist<-reactive({
#     t<-transitivity(subgraph.simplified.court(),"local",isolates = "zero")
#     data.frame(clust=t)
#   })
  
  judgments.year<-reactive({
    sub<-subset.judges.court()[!duplicated(subset.judges.court()$judgmentID),]
    df<-count(sub,"judgmentYear")
    names(df)<-c("year","number.judgments")
    df
  })
  
  max.component<-reactive({
    
    g<-subgraph.court()
    if(ecount(g)==0)
      g.comp=NULL
    else {
    g.comp<-t(sapply(unique(E(g)$judgmentYear),function(x) {
      g.sub<-subgraph.edges(g,E(g)[E(g)$judgmentYear==x],delete.vertices = T)
      g.sub<-simplify(g.sub)
      cl<-clusters(g.sub)
      c(x,max(cl$csize)/vcount(g.sub))
    }))
    g.comp<-as.data.frame(g.comp)
    names(g.comp)<-c("year","size.max.component")
    }
    g.comp
  })
  
  judges.year<-reactive({
    temp<-count(subset.judges.court(),c("JudgeName","judgmentYear"))
    names(temp)[3]<-"dd"
    df<-count(temp,"judgmentYear")
    names(df)<-c("year","number.judges")
    df
  })

  output$table1<-renderDataTable({max.component()})
  output$text1<-renderText({judges.coop.court()})
  
output$plot.net <- renderPlot({
    lay<-layout.fruchterman.reingold(subgraph.simplified())
    plog(subgraph.simplified(),lay)
  },width=400,height=400)

plot.net2 <- reactive({
  lay<-layout.fruchterman.reingold(subgraph.simplified.court())
  list<-subgraph.mark.list()[-length(subgraph.mark.list())]
  plog(subgraph.simplified.court(),lay,list)
})#,width=400,height=400)

plot.legend <- reactive({
  plog.legend(subgraph.mark.list())  
})#,width=400,height=400)

output$plot.graph<-renderPlot({
  par(mfrow=c(1,2))
  plot.net2()
  plot.legend()
})#,width=800,height=800)

plot.k <- reactive({
    ggplot(k.dist(),aes(x=k))+geom_histogram()
  })
  
plot.w <- reactive({
    ggplot(w.dist(),aes(x=w))+geom_histogram()
  })
  
# plot.t <- reactive({
#     ggplot(trans.dist(),aes(x=clust))+geom_histogram()
#   })
  
plot.s<-reactive({
    ggplot(s.dist(),aes(x=s)) + geom_histogram()
  })

plot.comp <- reactive({
  if(is.null(max.component()))
    ggplot(data.frame(x=0,y=5,t="No data to plot"),aes(x=x,y=y,label=t)) + geom_text(size=20)
  else
    ggplot(max.component(),aes(x=year,y=size.max.component)) + geom_line()
  })
  
plot.judges <- reactive({
    ggplot(judges.year(),aes(x=year,y=number.judges))+geom_line()
  })

plot.coop<- reactive({
  if(is.null(judges.coop.year()))
    ggplot(data.frame(x=0,y=5,t="No data to plot"),aes(x=x,y=y,label=t)) + geom_text(size=20)
  else
    ggplot(judges.coop.year(),aes(x=year,y=coop))+geom_line()
  })

plot.judgments <- reactive({
    ggplot(judgments.year(),aes(x=year,y=number.judgments))+geom_line()
  })

output$plot.top.chart<-renderPlot({
  ggplot(judges.top.court(),aes(x=N.of.judgments,y=JudgeName))+geom_point()
})
  output$plot.multi<-renderPlot({
    #multiplot(plot.comp(),plot.judges(),plot.coop(),plot.k(),plot.w(),plot.s(),cols=2)
    multiplot(plot.k(),plot.w(),plot.s(),plot.comp(),plot.judges(),plot.judgments(),plot.coop(),cols=2)
  })

#   output$plot2<-renderGvis({
#     #no.rand<-data.frame(year=seq(1990,2015,length.out=26),number=seq(30,100,length.out = 26))
#     g<-gvisLineChart(division.coop(),xvar="year",yvar="wsp",chartid = "Chart1")
#     return(g)
#   })
  
})
