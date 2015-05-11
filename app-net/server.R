require(tidyr)
require(scales)
require(zoo)
require(shiny)
require(ggplot2)
require(igraph)
require(plyr)
require(dplyr)
require(data.table)
require(RColorBrewer)
require(sqldf)
source("funkcje.R")
source("helpers.R")
judgments<-readRDS("data/judgments.rds")
judges<-readRDS("data/judges.rds")
divisions<-readRDS("data/divisions.rds")
judges.net<-readRDS("data/judges.net.rds")

theme_set(theme_bw())
  
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
  output$select.court<-renderUI({
    courts.un<-divisions[!duplicated(divisions$CourtCode),] %>% dplyr::arrange(CourtName)
    list1<-as.list(courts.un$CourtCode)
    names(list1)<-courts.un$CourtName
    selectInput("select.court",label=h3("Wybierz sąd:"),choices=list1)
  })
  output$plottemp<-renderPlot({
    plog.legend2(g.color.div(subgraph.simplified.court(),subgraph.mark.matrix(),court.divisions()))
  },width=550,height=800)
  
  court.divisions<-reactive({
    subset(divisions,CourtCode==input$select.court)
  })
  
  subset.judgments.court<-reactive({
    subset(judges.net,CourtCode==input$select.court)
  })
  
  subset.judges.court<-reactive({
    judges.sub<-subset(judges,CourtCode==input$select.court)
    judges.sub<-subset(judges.sub,!is.na(judges.sub$JudgeName))
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
  
#   judges.coop.year<-reactive({
#     j.coop.year(subset.judges.court(),subset.judgments.court(),subgraph.simplified.court())
#   })
  
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
    {if(vcount(subgraph.simplified.court())==0) return(NULL)
     else w=as.vector(E(subgraph.simplified.court())$weight)
    }
    k<-as.vector(degree(subgraph.simplified.court()))
    as.data.frame(k)
  })
  
  w.dist<-reactive({
    
    {if(ecount(subgraph.simplified.court())==0) return(NULL)
     else w=as.vector(E(subgraph.simplified.court())$weight)
    }
    data.frame(w=w)
  })
  
  judgments.year<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    judgm.year(subset.judges.court())
  })
  
  judges.year<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    j.year(subset.judges.court())
  })

  team.size<-reactive({
    judgm.count<- subset(judges,CourtCode==input$select.court) %>% plyr::count(.,"judgmentID") %>% mutate(liczba.s=as.factor(freq))
  })

  team.types<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    judg.cnt<-plyr::count(subset.judges.court(),c("judgmentID","JudgeSex"))
    temp<-spread(judg.cnt,JudgeSex,freq,fill = 0) %>% mutate(typestring=paste(F,ifelse(F==1,"kobieta","kobiet"),"i\n",M,ifelse(M==1,"mężczyzna","mężczyzn"))) ##%>% mutate(typet=do.call(paste0, temp[c(2, 3)]))
  })
#   
#   max.component<-reactive({    
#     max.comp(subgraph.court())
#   })
  
  subset.judges.clean<-reactive({
    subset(subset.judges.court(),!is.na(JudgeSex))
  })
  
  output$plot.k <- renderPlot({
    if(is.null(k.dist())){return(NULL)}  
    br<-if(length(unique(k.dist()$k))>1) seq(min(k.dist()$k,na.rm =T),max(k.dist()$k,na.rm =T),length.out=20) else seq(0,20,length.out=20)
    ggplot(k.dist(),aes(x=k))+geom_histogram(breaks=br)+labs(x="k - liczba bezpośrednich połączeń z innymi sędziamy",title="Histogram zmiennej k")
  })
  
  output$plot.w <- renderPlot({
    if(is.null(w.dist())){return(NULL)}
      br<-if(length(unique(w.dist()$w))>1) seq(min(w.dist()$w,na.rm =T),max(w.dist()$w,na.rm =T),length.out=20) else seq(0,20,length.out=20)
      ggplot(w.dist(),aes(x=w))+geom_histogram(breaks=br)+labs(x="w - ile razy dwóch sędziów zasiadało w tym samym składzie sędziowskim",title="Histogram zmiennej w")
  })
#   
#   plot.comp <- reactive({
#       if(is.null(max.component())){return(NULL)}
#       ggplot(max.component(),aes(x=year,y=size.max.component)) + geom_line()+labs(y="Rozmiar największego komponentu [%]",title="Graph of the maximum component relative size in terms of number of nodes")+ylim(0,1)
#   })
  
  output$plot.judges <- renderPlot({
    if(nrow(judges.year())==0){return(NULL)}
    #ggplot(judges.year(),aes(x=year,y=number.judges))+geom_line()+labs(y="Number of judges",title="Graph showing number of judges in court in following years")+ylim(0,max(judges.year()$number.judges))
    if(length(judges.year()$Data)<20){br<-judges.year()$Data[seq(1,length(judges.year()$Data),by=4)]} else {br<-judges.year()$Data[round(seq(1,length(judges.year()$Data),length.out=20),0)]}
    ggplot(judges.year(), aes(x=Data, y=number.judges, group=1)) +
      geom_point(stat='summary', fun.y=sum) +
      stat_summary(fun.y=sum, geom="line")+scale_x_discrete(labels=judges.year()$Data,breaks=br)+
      labs(y="Liczba orzeczeń w czasie",title="Wykres pokazujący liczbę orzeczeń w wybranym sądzie w kolejnych miesiącach")+ylim(0,max(judges.year()$number.judges))
    
  })
#   
#   plot.coop<- reactive({
#      if(is.null(judges.coop.year())) {return(NULL)}
#       ggplot(judges.coop.year(),aes(x=year,y=coop))+geom_line()+labs(y="Diversity of judging teams [%]",title="Graph showing diversity of judging teams in following years")+ylim(0,1)
#   })
  
  output$plot.judgments <- renderPlot({
    
    if(nrow(judgments.year())==0){return(NULL)}
    if(length(judgments.year()$Data)<20){br<-judgments.year()$Data[seq(1,length(judgments.year()$Data),by=4)]} else {br<-judgments.year()$Data[round(seq(1,length(judgments.year()$Data),length.out=20),0)]}
    ggplot(judgments.year(), aes(x=Data, y=number.judgments, group=1)) +
      geom_point(stat='summary', fun.y=sum) +
      stat_summary(fun.y=sum, geom="line")+scale_x_discrete(labels=judgments.year()$Data,breaks=br)+
      labs(y="Liczba orzeczeń w czasie",title="Wykres pokazujący liczbę orzeczeń w wybranym sądzie w kolejnych miesiącach")+ylim(0,max(judgments.year()$number.judgments))
  })

  output$plot.team.size<-renderPlot({
    ggplot(team.size(),aes(x=liczba.s))+geom_histogram()+xlab("Liczba sędziów w składzie")+ylab("Liczba wystąpień")
  })
  
  output$plot.team.types<-renderPlot({
    validate(
      need(nrow(team.types())!=0,"Trwa ładowanie danych...")
    )
    qplot(typestring,data=team.types(),geom="bar")+xlab("Typ składu sędziowskiego")+ylab("Liczba wystąpień")
  })

  plot.sex<-reactive({
    if(nrow(subset.judges.clean())==0){return(NULL)}
    plot.sex.distribution(subset.judges.clean())
  })
  
#   output$text1<-renderText({subgraph.summary()})
   output$table1<-renderDataTable({team.types()})
#   output$table2<-renderDataTable({judges.coop.year()})
#   output$table3<-renderDataTable({court.divisions()})
# 

# stare rysowanie sieci bez svg
  output$plot.pie<-renderPlot({
    g<-subgraph.color.pie()
    lay<-subgraph.layout()
    layout(matrix(c(rep(1,12),2,2,2,3), 4, 4, byrow = FALSE))
    par(mar=c(0,0,0,0))
    plog.pie(g,lay)
    par(mar=c(0,0,0,0))
    plog.legend2(g.color.div(subgraph.simplified.court(),subgraph.mark.matrix(),court.divisions()))
    par(mar=c(0,0,3,0))
    plog.sex()
  },width=1000,height=800)

  output$plot.multi<-renderPlot({
    #multiplot(plot.judgments(),plot.judges(),plot.sex(),plot.k(),plot.w(),plot.comp(),plot.coop(),cols=1)
    #multiplot(plot.judgments(),plot.judges(),plot.k(),plot.w(),plot.team.size(),plot.team.types(),cols=1) 
    #multiplot(plot.judgments(),cols=1)
    
  },width=1000,height=2850)
  
  output$topImage<-renderImage({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    top<-judges.top.court()
    outfile <- tempfile(fileext='.svg')
    g1<-ggplot(top,aes(x=N.of.judgments,y=JudgeName,size=N.of.judgments))+geom_point()+labs(x="Number of judgments",y="Judge Name",title="TopChart for judges is speciified court")+geom_segment(x =0, y =nrow(top):1 , aes(xend =(N.of.judgments-0.50*sqrt(N.of.judgments/pi))), yend = nrow(top):1,size=0.7)+theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(face="bold",angle=0, vjust=0.5, size=10),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=18))+scale_shape()+scale_size_continuous(range = c(3,12))
    ggsave(filename=(outfile),g1,width = 1.5*160,height=1.5*120,units ="mm")
    filename <- normalizePath(file.path(outfile))
    list(src=filename,
         alt="alt text",
         width=1000,
         height=750
    )  
    }, deleteFile = TRUE)
  
#not used
#   output$times<-renderText({
#     g<-subgraph.color.pie()
#     lay<-subgraph.layout()
#     t.pie<-system.time(plog.pie(g,lay))[1]
#     t.multi<-system.time(multiplot(plot.judgments(),plot.judges(),plot.sex(),plot.k(),plot.w(),plot.comp(),plot.coop(),cols=2))[1]
#     t.lay<-system.time(subgraph.layout())[1]
#     t.g<-system.time(subgraph.court())[1]
#     t.sim<-system.time(subgraph.simplified.court())[1]
#     paste("Times",t.lay,t.g,t.sim,t.pie,t.multi,sep=";")
#   })
  
  output$pieImage <- renderImage({
    validate(
      need(vcount(subgraph.simplified.court())>0, "Trwa ładowanie danych...")
    )
    g<-subgraph.color.pie()
    lay<-subgraph.layout()
    outfile <- tempfile(fileext='.svg')
    svg(outfile)
    layout(matrix(c(rep(c(rep(1,3),2),2),rep(1,3),3,rep(4,4)), 4, 4, byrow = TRUE))
    par(mar=c(0,0,0,0))
    plog.pie.svg(g,lay)
    par(mar=c(0,0,0,0))
    plog.legend.svg(g.color.div(subgraph.simplified.court(),subgraph.mark.matrix(),court.divisions()))
    par(mar=c(0,0,0,0))
    plog.sex.svg()
    dev.off()
    filename <- normalizePath(file.path(outfile))
    list(src=filename,
         alt="alt text",
         width=1000,
         height=1000
         )
  }, deleteFile = TRUE)
})