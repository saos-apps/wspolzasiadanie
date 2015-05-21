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
Sys.setlocale("LC_ALL","pl_PL.UTF-8")
mon<-data.frame(abr=paste(months(as.Date(paste("01-",1:12,"-1995",sep="")),T)," ",sep=""),pe=paste(months(as.Date(paste("01-",1:12,"-1995",sep="")),F)," ",sep=""))

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
    if(vcount(subgraph.simplified.court())<2){ return(NULL)}
    k<-as.vector(degree(subgraph.simplified.court()))
    as.data.frame(k)
  })
  
  w.dist<-reactive({
    if(ecount(subgraph.simplified.court())==0){return(NULL)}
     w=as.vector(E(subgraph.simplified.court())$weight)
    data.frame(w=w)
  })
  
  judgments.year<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    judgm.year(subset.judges.court())
  })
  
  judgments.year2<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    judgm.year2(subset.judges.court())
  })

  judges.year<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    j.year(subset.judges.court())
  })

  team.size<-reactive({
    #judgm.count<- subset(judges,CourtCode==input$select.court) %>% plyr::count(.,"judgmentID") %>% mutate(liczba.s=as.factor(freq))
    judgm.count<- subset(judges,CourtCode==input$select.court) %>% plyr::count(.,"judgmentID") %>% mutate(liczba.s=as.factor(freq)) %>% select(-freq) %>% group_by(liczba.s) %>% summarise(count=n())
  })

  team.types<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    judg.cnt<-plyr::count(subset.judges.court(),c("judgmentID","JudgeSex"))
    temp<-spread(judg.cnt,JudgeSex,freq,fill = 0) %>% mutate(typestring=paste(F,ifelse(F==1,"kobieta","kobiet"),"i\n",M,ifelse(M==1,"mężczyzna","mężczyzn"))) %>% mutate(frac=F/(F+M)) 
  })

  team.types2<-reactive({
  validate(
    need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
  )
  judg.cnt<-plyr::count(subset.judges.court(),c("judgmentID","JudgeSex"))
  ttypes2<-spread(judg.cnt,JudgeSex,freq,fill = 0) %>% mutate(major=ifelse(F>M,"kobiety",ifelse(F==M,"brak przewagi","mężczyźni"))) %>% mutate(typer=paste(ifelse(F>M,F,M),ifelse(F>M,M,F),sep="/"))
  ggplot(ttypes2, aes(x=typer, fill=major))+geom_bar(position="fill")
  ctypes2<-plyr::count(ttypes2,c("major","typer")) %>% filter(typer!="0/0") #%>% mutate(freqnorm=ifelse(freq<sum(freq)/17,sum(freq)/17,freq))
  temp<-aggregate(freq ~ typer,ctypes2,sum) %>% mutate(freqnorm=ifelse(freq<sum(freq)/17,sum(freq)/17,freq)) %>% arrange(desc(freqnorm)) %>% mutate(xmax=cumsum(freqnorm),xmin=(xmax-freqnorm))
  ctypes2<-merge(ctypes2,temp,by="typer") %>% mutate(freq.x=freq.x/freq.y)
  names(ctypes2)[c(3,4)]<-c("freqmajor","typesum")
  ctypes2<-ddply(ctypes2, .(typer), transform, ymax = cumsum(freqmajor)) %>% mutate(ymin=ymax-freqmajor)
  ctypes2$xtext <- with(ctypes2, xmin + (xmax - xmin)/2)
  ctypes2$ytext <- with(ctypes2, ymin + (ymax - ymin)/2)
  ctypes2
})

  
  team.types2b<-reactive({
    validate(
      need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
    )
    judg.cnt<-plyr::count(subset.judges.court(),c("judgmentID","JudgeSex"))
    ttypes2<-spread(judg.cnt,JudgeSex,freq,fill = 0) %>% mutate(major=ifelse(F>M,"kobiety",ifelse(F==M,"brak przewagi","mężczyźni"))) %>% mutate(typer=paste(ifelse(F>M,F,M),ifelse(F>M,M,F),sep="/"))
    ctypes2<-plyr::count(ttypes2,c("major","typer")) %>% mutate(typer=ifelse(freq<10,"inne",typer)) %>% group_by(major,typer) %>% summarise(freq=sum(freq)) %>% filter(freq>=10)
    temp<-aggregate(freq ~ typer,ctypes2,sum) %>% mutate(freqnorm=ifelse(freq<sum(freq)/17,sum(freq)/17,freq)) %>% arrange(desc(freqnorm)) %>% mutate(xmax=cumsum(freqnorm),xmin=(xmax-freqnorm))
    ctypes2<-merge(ctypes2,temp,by="typer") %>% mutate(freq.x=freq.x/freq.y)
    names(ctypes2)[c(3,4)]<-c("freqmajor","typesum")
    ctypes2<-ddply(ctypes2, .(typer), transform, ymax = cumsum(freqmajor)) %>% mutate(ymin=ymax-freqmajor)
    ctypes2$xtext <- with(ctypes2, xmin + (xmax - xmin)/2)
    ctypes2$ytext <- with(ctypes2, ymin + (ymax - ymin)/2)
    ctypes2
  })


team.types3<-reactive({
  validate(
    need(nrow(subset.judges.court())!=0,"Trwa ładowanie danych...")
  )
  judg.cnt<-plyr::count(subset.judges.court(),c("judgmentID","JudgeSex"))
  ttypes3<-spread(judg.cnt,JudgeSex,freq,fill = 0) %>% plyr::count(.,c("F","M")) %>% filter(F!=0 | M!=0)
})


#   
#   max.component<-reactive({    
#     max.comp(subgraph.court())
#   })
  
  subset.judges.clean<-reactive({
    subset(subset.judges.court(),!is.na(JudgeSex))
  })
  
  output$plot.k <- renderPlot({
    #if(is.null(k.dist())){return(NULL)}  
    validate(
      need(!is.null(k.dist()),"Brak danych...")
    )
    #br<-if(length(unique(k.dist()$k))>1) seq(min(k.dist()$k,na.rm =T),max(k.dist()$k,na.rm =T),length.out=20) else seq(0,20,length.out=20)
#     ggplot(k.dist(),aes(x=k))+geom_histogram(breaks=br)+
#       #scale_x_discrete
#       labs(x="k - liczba bezpośrednich połączeń z innymi sędziami",y="Liczba wystąpień",title="Histogram zmiennej k")+
#     theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+scale_x_continuous(breaks=pretty_breaks(20))
#     
    bby<-ceiling(max(k.dist()$k)/20)
    br<-seq(1,max(k.dist()$k),by=bby)
    ggplot(k.dist(),aes(x=k))+geom_histogram(aes(fill=..count..),breaks=br)+
      #scale_x_discrete
      labs(x="k - liczba bezpośrednich połączeń z innymi sędziami",y="Liczba wystąpień",title="Histogram zmiennej k")+
      theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+
      scale_x_continuous(breaks=br[-1]-bby/2,labels=br[-1])
  })
  
  output$plot.w <- renderPlot({
    #if(is.null(w.dist())){return(NULL)}
    validate(
      need(!is.null(w.dist()),"Brak danych...")
    )
#       br<-if(length(unique(w.dist()$w))>1) seq(min(w.dist()$w,na.rm =T),max(w.dist()$w,na.rm =T),length.out=20) else seq(0,20,length.out=20)
#       ggplot(w.dist(),aes(x=w))+geom_histogram(breaks=br)+labs(x="w - ile razy dwóch sędziów zasiadało w tym samym składzie sędziowskim",y="Liczba wystąpień",title="Histogram zmiennej w")+
#       theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+scale_x_continuous(breaks=pretty_breaks(20))

    bby<-ceiling(max(w.dist()$w)/20)
    br<-seq(1,max(w.dist()$w),by=bby)
    ggplot(w.dist(),aes(x=w))+geom_histogram(aes(fill=..count..),breaks=br)+labs(x="w - ile razy dwóch sędziów zasiadało w tym samym składzie sędziowskim",y="Liczba wystąpień",title="Histogram zmiennej w")+
      theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+
      scale_x_continuous(breaks=br[-1]-bby/2,labels=br[-1])
  })
#   
#   plot.comp <- reactive({
#       if(is.null(max.component())){return(NULL)}
#       ggplot(max.component(),aes(x=year,y=size.max.component)) + geom_line()+labs(y="Rozmiar największego komponentu [%]",title="Graph of the maximum component relative size in terms of number of nodes")+ylim(0,1)
#   })
  
  output$plot.judges <- renderPlot({
    #if(nrow(judges.year())==0){return(NULL)}
    validate(
      need(sum(!is.na(judges.year()$number.judges))>1,"Brak danych...")
    )
    siz<-c(1,2,3,4,6,12,24)
    bylab<-siz[which(length(judges.year()$Data)/44 < siz)[1]]
    br1<-judges.year()$Data[seq(1,length(judges.year()$Data),by=bylab)]
    yearlabel<-seq(as.numeric(strsplit(as.character(judges.year()$Data[1])," ")[[1]][2]),as.numeric(strsplit(as.character(judges.year()$Data[length(judges.year()$Data)])," ")[[1]][2]))
    xlab<-seq(1,length(judges.year()$Data),12)
    br2<-rep(mon$abr[seq(1,12,by=bylab)],length(yearlabel))
      #br2<-as.vector(br)
    #  for(i in 1:12){br2<-gsub(pattern = mon$abr[i],paste0(mon$pe[i],"\n"),br2)}
    plabels<-data.frame(x=xlab,year=yearlabel,y=1.05*(max(judges.year()$number.judges,na.rm=T)))
    
    ggplot(judges.year(), aes(x=Data, y=number.judges, group=1)) +  
    geom_point(stat='summary', fun.y=sum) +
    stat_summary(fun.y=sum, geom="line")+
      scale_x_discrete(labels=br2,breaks=br1)+
      labs(y="Liczba orzekających sędziów",title="Liczba orzekających sędziów")+
      ylim(0,max(judges.year()$number.judges)*1.1)+
      theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+
      geom_vline(xintercept =xlab[-1],colour="grey45",alpha=0.7,linetype="longdash")+
      geom_text(data=plabels,aes(x=x, label=year,y=y), colour="blue", angle=0, text=element_text(size=10),hjust =-0.1)
  })
  
  output$plot.judgments<- renderPlot({
    #if(nrow(judgments.year())==0){return(NULL)}
    validate(
      need(sum(!is.na(judgments.year()$number.judgments))>1,"Brak danych...")
    )
    siz<-c(1,2,3,4,6,12,24)
    bylab<-siz[which(length(judgments.year()$Data)/44 < siz)[1]]
    br1<-judgments.year()$Data[seq(1,length(judgments.year()$Data),by=bylab)]
    yearlabel<-seq(as.numeric(strsplit(as.character(judgments.year()$Data[1])," ")[[1]][2]),as.numeric(strsplit(as.character(judgments.year()$Data[length(judgments.year()$Data)])," ")[[1]][2]))
    xlab<-seq(1,length(judgments.year()$Data),12)
    br2<-rep(mon$abr[seq(1,12,by=bylab)],length(yearlabel))
    
    plabels<-data.frame(x=xlab,year=yearlabel,y=1.05*(max(judgments.year()$number.judgments,na.rm=T)))
    ggplot(judgments.year(), aes(x=Data, y=number.judgments, group=1)) +
      geom_point(stat='summary', fun.y=sum) +
      stat_summary(fun.y=sum, geom="line")+
      scale_x_discrete(labels=br2,breaks=br1)+
      labs(y="Liczba orzeczeń",title="Wykres pokazujący liczbę orzeczeń w wybranym sądzie w danym miesiącu")+
      ylim(0,max(judgments.year()$number.judgments)*1.1)+
      theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+
      geom_vline(xintercept =xlab[-1],colour="grey45",alpha=0.7,linetype="longdash")+
      geom_text(data=plabels,aes(x=x, label=year,y=y), colour="blue", angle=0, text=element_text(size=10),hjust =-0.1)
  })

  output$plot.team.size<-renderPlot({
    validate(
      need(nrow(team.size())>1,"Brak danych...")
    )
#     ggplot(team.size(),aes(x=liczba.s))+geom_histogram()+
#     labs(x="Liczba sędziów w składzie",y="Liczba wystąpień",title="Wykres pokazujący wielkość składów sędziowskich")+
#       #ylim(0,max())
#     theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))
    ggplot(team.size(), aes(x=liczba.s, y=count, width=0.5)) + 
      geom_bar(aes(fill=count), stat="identity", position="identity")+
            geom_text(aes(x=liczba.s,y=count+max(count)/30,label=count),size=5)+
            scale_y_continuous(breaks=pretty_breaks(10))+
            theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"),legend.position="none")
  },width=1000,height=600)
  
  output$plot.team.types<-renderPlot({
    validate(
      need(nrow(team.types())!=0,"Trwa ładowanie danych...")
    )
    qplot(typestring,data=team.types(),geom="bar",fill=frac)+
    labs(x="Typ składu orzekającego",y="Liczba wystąpień",title="Wykres pokazujący wszystkie typy składów orzekających z podziałem na płeć")+
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(angle=0, vjust=0.5, size=12),axis.text.x  = element_text(face="bold",angle=0, vjust=0.5, size=12),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+
      scale_fill_continuous()
  })

  output$plot.team.types2<-renderPlot({
    validate(
      need(nrow(team.types2())>1,"Brak danych...")
    )
    labels<-data.frame(xmean=team.types2()$xmin+(team.types2()$xmax-team.types2()$xmin)/2,text=team.types2()$typesum)
    ggplot(team.types2(), aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = major))+geom_rect(colour = I("grey"))+
      geom_text(aes(x = xtext, y = ytext, label = ifelse(xmin==0,paste(major," - ",round(100*freqmajor,1), "%", sep = ""),paste(round(100*freqmajor,1), "%", sep = ""))), size = 4.5)+
      geom_text(aes(x = xtext, y = 1.03, label = typer), size = 5)+
      annotate("text",label="Typ składu: ",x=(min(labels$xmean*0.1)),y=1.03,size=5)+
      annotate("text",x=labels$xmean,y=-0.03,label=labels$text,size=5)+
      annotate("text",label="Liczba orzeczeń: ",x=(min(labels$xmean*0.1)),y=-0.03,size=5)+
      ggtitle("Wykres pokazujący wszystkie typy składów orzekających z podziałem na płeć")+
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="bottom",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  },width = 1100, height = 600, res = 72)
  
  output$plot.team.types2b<-renderPlot({
    validate(
      need(nrow(team.types2b())>1,"Brak danych...")
    )
    labels<-data.frame(xmean=team.types2b()$xmin+(team.types2b()$xmax-team.types2b()$xmin)/2,text=team.types2b()$typesum)
    ggplot(team.types2b(), aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = major))+geom_rect(colour = I("grey"))+
      geom_text(aes(x = xtext, y = ytext, label = ifelse(xmin==0,paste(major," - ",round(100*freqmajor,1), "%", sep = ""),paste(round(100*freqmajor,1), "%", sep = ""))), size = 4.5)+
      geom_text(aes(x = xtext, y = 1.03, label = typer), size = 5)+
      annotate("text",label="Typ składu: ",x=(min(labels$xmean*0.1)),y=1.03,size=5)+
      annotate("text",x=labels$xmean,y=-0.03,label=labels$text,size=5)+
      annotate("text",label="Liczba orzeczeń: ",x=(min(labels$xmean*0.1)),y=-0.03,size=5)+
      ggtitle("Wykres pokazujący wszystkie typy składów orzekających z podziałem na płeć")+
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="bottom",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  },width = 1100, height = 600, res = 72)

  output$plot.team.types3<-renderPlot({
    validate(
      need(nrow(team.types3())!=0,"Trwa ładowanie danych...")
    )
    ggplot(team.types3(),aes(x=M,y=F))+geom_point(aes(size=freq,colour=F/(F+M)))+scale_size_continuous(range = c(10,30))+scale_shape()+
      scale_color_continuous(low="lightblue4",high="blue4")+
      theme(legend.position="none")+
      geom_text(aes(x=M,y=F,label=freq),size=4,color="white") #fill=(M/(F+M))
  })

  plot.sex<-reactive({
    if(nrow(subset.judges.clean())==0){return(NULL)}
    plot.sex.distribution(subset.judges.clean())
  })
  
#   output$text1<-renderText({subgraph.summary()})
   output$table1<-renderDataTable({judges.year()})
   output$table2<-renderDataTable({judgments.year()})
   output$table3<-renderDataTable({team.types2()})
  output$table4<-renderDataTable({team.size()})
    
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
      need(nrow(subset.judges.court())>0,"Trwa ładowanie danych...")
    )
    
    validate(
      need(nrow(subset.judges.court())>1,"Brak danych...")
    )
    
    top<-judges.top.court()
    outfile <- tempfile(fileext='.svg')
    g1<-ggplot(top,aes(x=N.of.judgments,y=JudgeName,size=N.of.judgments))+geom_point()+labs(x="Łączna liczba orzeczeń",y="Sędzia",title="10 sędziów orzekających w największej liczbie spraw")+geom_segment(x =0, y =nrow(top):1 , aes(xend =(N.of.judgments-0.50*sqrt(N.of.judgments/pi))), yend = nrow(top):1,size=0.7)+theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),axis.title.y = element_text(face="bold", colour="#990000", size=14),axis.text.y  = element_text(face="bold",angle=0, vjust=0.5, size=10),legend.position="none",plot.title=element_text(face="bold",angle=0, vjust=0.5, size=14,colour="#990000"))+scale_shape()+scale_size_continuous(range = c(3,12))
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
    validate(
      need(vcount(subgraph.simplified.court())>1, "Brak danych...")
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