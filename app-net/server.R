require(shiny)
require(ggplot2)
require(igraph)
require(plyr)
require(data.table)
require(RColorBrewer)
require(shinydashboard)
# server.R
source("funkcje.R")
source("helpers.R")
judgments<-readRDS("data/judgments.rds")
judges<-readRDS("data/judges.rds")
divisions<-readRDS("data/divisions.rds")
judges.net<-readRDS("data/judges.net.rds")

theme_set(theme_bw())

shinyServer(function(input, output) {

  output$select.court<-renderUI({
    courts.un<-divisions[!duplicated(divisions$CourtCode),]
    list1<-as.list(courts.un$CourtCode)
    names(list1)<-courts.un$CourtName
    selectInput("select.court",label=h3("Select the court:"),choices=list1)
  })
  
  court.divisions<-reactive({
    subset(divisions,CourtCode==input$select.court,select="DivisionName")
  })
  
  subset.judgments.court<-reactive({
    subset(judges.net,CourtCode==input$select.court)
  })
  
  subset.judges.court<-reactive({
    subset(judges,CourtCode==input$select.court)
  })
  
  judges.top.court<-reactive({
    judges.top.c(subset.judges.court())
  })
  
  subgraph.court<-reactive({
    g.court(subset.judges.court(),subset.judgments.court())
  })
  
  subgraph.simplified.court<-reactive({
    g.simplify.c(subgraph.court())
    })
    
  subgraph.mark.matrix<-reactive({
    g.mark.matrix(subgraph.simplified.court())
  })

  subgraph.mark.list<-reactive({
    g.mark.list(subgraph.simplified.court(),subgraph.mark.matrix())
  })
  
subgraph.color.pie<-reactive({
  g.color.pie(subgraph.simplified.court())
})

subgraph.layout<-reactive({
  g<-subgraph.simplified.court()
  layout.fruchterman.reingold(g,weights=E(g)$weight,area=10000*vcount(g)^2,repulserad=50000*vcount(g)^3)
})

  judges.coop.year<-reactive({
    j.coop.year(subset.judges.court(),subset.judgments.court(),subgraph.simplified.court())
  })

  subgraph.summary<-reactive({
    g<-subgraph.court()
    g.sim<-subgraph.simplified.court()
    paste("vcount:",vcount(g),"ecount",ecount(g),
          "ecount simplified:",ecount(g.sim),sep="  ")
})
  
  s.dist<-reactive({
    s<-plyr::count(subset.judges.court(),"judgmentID")$freq
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
  
  judgments.year<-reactive({
    judgm.year(subset.judges.court())
  })
  
  judges.year<-reactive({
    j.year(subset.judges.court())
  })

  max.component<-reactive({    
    max.comp(subgraph.court())
  })
  
subset.judges.clean<-reactive({
  subset(subset.judges.court(),!is.na(JudgeSex))
})

plot.net <- reactive({
  g<-subgraph.simplified.court()
  lay<-subgraph.layout()
  list<-subgraph.mark.list()[-length(subgraph.mark.list())]
  plog(g,lay,list)
})

plot.legend <- reactive({
  plog.legend(subgraph.mark.list())  
})

plot.k <- reactive({
  if(ecount(subgraph.simplified.court())==0) 
    NULL
  else {
    br<-if(length(unique(k.dist()$k))>1) seq(min(k.dist()$k,na.rm =T),max(k.dist()$k,na.rm =T),length.out=20) else seq(0,20,length.out=20)
    ggplot(k.dist(),aes(x=k))+geom_histogram(breaks=br)+labs(x="k - Number of direct connections to other judges",title="Histogram of k")
  }
  })
  
plot.w <- reactive({
  if(ecount(subgraph.simplified.court())==0) 
    NULL
  else {
    br<-if(length(unique(w.dist()$w))>1) seq(min(w.dist()$w,na.rm =T),max(w.dist()$w,na.rm =T),length.out=20) else seq(0,20,length.out=20)
    ggplot(w.dist(),aes(x=w))+geom_histogram(breaks=br)+labs(x="w - Number of times two judges was in the same judgment team",title="Histogram of w")
  }
  })

plot.comp <- reactive({
  if(is.null(max.component()))
    NULL
  else
    ggplot(max.component(),aes(x=year,y=size.max.component)) + geom_line()+labs(y="Maximum component size [%]",title="Graph of the maximum component relative size in terms of number of nodes")+ylim(0,1)
  })
  
plot.judges <- reactive({
    ggplot(judges.year(),aes(x=year,y=number.judges))+geom_line()+labs(y="Number of judges",title="Graph showing number of judges in court in following years")+ylim(0,max(judges.year()$number.judges))
  })

plot.coop<- reactive({
  if(is.null(judges.coop.year()))
    NULL
  else
    ggplot(judges.coop.year(),aes(x=year,y=coop))+geom_line()+labs(y="Diversity of judging teams [%]",title="Graph showing diversity of judging teams in following years")+ylim(0,1)
  })

plot.judgments <- reactive({
    ggplot(judgments.year(),aes(x=year,y=number.judgments))+geom_line()+labs(y="Number of judgments",title="Graph showing number of judgments in specified court in following years")+ylim(0,max(judgments.year()$number.judgments))
  })

plot.sex<-reactive({
  plot.sex.distribution(subset.judges.clean())
})

output$text1<-renderText({subgraph.summary()})
output$table1<-renderDataTable({max.component()})
output$table2<-renderDataTable({judges.coop.year()})
output$table3<-renderDataTable({court.divisions()})

output$plot.graph<-renderPlot({
  #par(mfrow=c(2,1))
  plot.net()
  #plot.legend()
},width=800,height=800)

output$plot.pie<-renderPlot({
  g<-subgraph.color.pie()
  lay<-subgraph.layout()
  plog.pie(g,lay)
},width=800,height=800)
  
output$plot.multi<-renderPlot({
    multiplot(plot.judgments(),plot.judges(),plot.sex(),plot.k(),plot.w(),plot.comp(),plot.coop(),cols=1)
  },width=1000,height=4000)

output$plot.top.chart<-renderPlot({
  top<-judges.top.court()
  ggplot(top,aes(x=N.of.judgments,y=JudgeName,size=N.of.judgments))+geom_point()+labs(x="Number of judgments",y="Judge Name",title="TopChart for judges is speciified court")+scale_size_continuous(range = c(3,15))+geom_segment(x =0, y =nrow(top):1 , aes(xend =(N.of.judgments-N.of.judgments/35)), yend = nrow(top):1,size=0.7)+theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=18))
},width=800,height=600)

output$times<-renderText({
  g<-subgraph.color.pie()
  lay<-subgraph.layout()
  t.pie<-system.time(plog.pie(g,lay))[1]
  t.multi<-system.time(multiplot(plot.judgments(),plot.judges(),plot.sex(),plot.k(),plot.w(),plot.comp(),plot.coop(),cols=2))[1]
  t.lay<-system.time(subgraph.layout())[1]
  t.g<-system.time(subgraph.court())[1]
  t.sim<-system.time(subgraph.simplified.court())[1]
  paste("Times",t.lay,t.g,t.sim,t.pie,t.multi,sep=";")
})
})