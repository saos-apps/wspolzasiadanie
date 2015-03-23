#require(gridSVG)
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
require(RColorBrewer)
# server.R
source("funkcje.R")
judgments<-read.table("data/judgments.csv")
judges<-read.table("data/judges.csv")
divisions<-read.table("data/divisions.csv")
judges.net<-read.table("data/judges.net.csv")
courts<-read.table("data/courts.csv")

theme_set(theme_bw())

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
    if(ecount(g)>0) E(g)$weight<-sapply(E(g)$CourtCode,length) else E(g)$weight=0
    E(g)$type="real"
    g       
    })
    
  subgraph.mark.matrix<-reactive({
    
    g<-subgraph.simplified.court()
    div.un<-unique(unlist(V(g)$DivisionCode))
    matrix<-sapply(div.un,function(x) sapply(V(g)$DivisionCode,function(y) x %in% y))
    matrix
  })

  subgraph.mark.list<-reactive({
    g<-subgraph.simplified.court()
    div.un<-unique(unlist(V(g)$DivisionCode))
    matrix<-subgraph.mark.matrix()
    list<-sapply(seq(length(div.un)),function(x) which(matrix[,x]))
    names(list)<-rep(brewer.pal(12,"Set3"),ceiling(length(div.un)/12))[seq(length(div.un))]
    names(list)<-addalpha(names(list),0.8)
    list$labels<-div.un
    #---- opcja 1
#     ul<-unlist(list[-length(list)])
#     names(ul)<-substr(names(ul),1,9)
#     ul<-as.list(ul)
#     ul$labels<-div.un
#     ul
    #--- opcja 2
    cl<-clusters(g)
    ord<-order(cl$csize,decreasing = T)
    list2<-sapply(list[-length(list)],function(x) {
      a<-cl$membership[x]  
      ll<-lapply(unique(cl$membership[x]),function(y){
      x[which(a==y)]
      })
    })
    list2<-unlist(list2,recursive = F)
    names(list2)<-substr(names(list2),1,9)
    list2$labels<-div.un
    list2
#--- opcja std.
#    list
  })
  
subgraph.color.pie<-reactive({
  g<-subgraph.simplified.court()
  div.un<-unique(unlist(V(g)$DivisionCode))
  matrix<-sapply(div.un,function(x) sapply(V(g)$DivisionCode,function(y) x %in% y))  
  names<-rep(brewer.pal(12,"Set3"),ceiling(length(div.un)/12))[seq(length(div.un))]
  names<-addalpha(names,0.75)
  values<-lapply(V(g)$DivisionCode,function(x) rep(1/length(x),length(x)))
  colors<-lapply(seq(nrow(matrix)),function(x) names[matrix[x,]])
  V(g)$colour<-colors
  V(g)$pie.values<-values
  g
})

subgraph.final<-reactive({
#   gb<-subgraph.simplified.court()
#   div.un<-subgraph.mark.list()$labels
#   matrix<-subgraph.mark.matrix()
#   sapply(seq(length(div.un)),function(x) {
#     vadd<-which(matrix[,x])
#     c1<-t(combn(vadd,2))
#     gb<<-add.edges(gb,c1)
#     E(gb)$type[which(is.na(E(gb)$type))]<<-"fake"
#     E(gb)$weight[which(is.na(E(gb)$weight))]<<-3
#   })
#   gb<-simplify(gb,remove.multiple = F,remove.loops = T)
  subgraph.color.pie()
})

subgraph.layout<-reactive({
  g<-subgraph.final()
  layout.fruchterman.reingold(g,weights=E(g)$weight,area=10000*vcount(g.sim)^2,repulserad=50000*vcount(g.sim)^3)
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
    {if(ecount(subgraph.simplified.court())==0)
      list1=NULL
    else{
    list1<-as.data.frame(t(sapply(years,function(x){
      sub.judges<-subset(subset.judges.court(),judgmentYear==x)
      n.judges<-length(unique(sub.judges$JudgeName))
      sub.judgments<-subset(subset.judgments.court(),judgmentYear==x)
      g.sub<-simplify(graph.data.frame(sub.judgments,directed = F,vertices=NULL),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
      coop.array<-count(sub.judges,"judgmentID")$freq
      coop<-ecount(g.sub)/max.unique.links(n.judges,coop.array)
      coop=ifelse(is.nan(coop),0,coop)
      coop=ifelse(coop>1,1,coop)
      c(x,coop)
    })))
    names(list1)<-c("year","coop")
    }
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
          "ecount simplified:",ecount(subgraph.final()),"\n coop:",paste(temp,sep=";"),sep="  ")
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
      c(as.numeric(x),max(cl$csize)/vcount(g.sub))
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
subset.judges.clean<-reactive({
  subset(subset.judges.court(),!is.na(JudgeSex))
})

sex.dist<-reactive({
  sexc<-count(subset.judges.clean(),c("judgmentID","JudgeSex"))
  ids<-data.frame(judgmentID=unique(sexc$judgmentID))
  sexa<-sqldf("select i.judgmentID, f.freq as freqf, m.freq as freqm from ids i
            left join sexc f on
            f.judgmentID=i.judgmentID
            and f.JudgeSex='F'
            left join sexc m on
            m.judgmentID=i.judgmentID
            and m.JudgeSex='M'
            ")
  sexa[is.na(sexa)]<-0
  sex.final<-transform(sexa,frac.female=freqf/(freqf+freqm))
})

  output$table1<-renderDataTable({max.component()})
  output$text1<-renderText({judges.coop.court()})
  
output$plot.net <- renderPlot({
    lay<-subgraph.layout()
    plog(subgraph.simplified(),lay)
  },width=400,height=400)

plot.net2 <- reactive({
  #g<-subgraph.final()
  g<-subgraph.simplified.court()
  lay<-subgraph.layout()
  list<-subgraph.mark.list()[-length(subgraph.mark.list())]
  plog(g,lay,list)
})#,width=400,height=400)

output$plot.pie<-renderPlot({
    g<-subgraph.color.pie()
    lay<-subgraph.layout()
    plog.pie(g,lay)
  },width=800,height=800)

plot.legend <- reactive({
  plog.legend(subgraph.mark.list())  
})#,width=400,height=400)

output$plot.graph<-renderPlot({
  par(mfrow=c(2,1))
  plot.net2()
  plot.legend()
},width=800,height=1600)

plot.k <- reactive({
  if(ecount(subgraph.simplified.court())==0) 
    NULL
  else {
    br<-if(length(unique(k.dist()$k))>1) seq(min(k.dist()$k,na.rm =T),max(k.dist()$k,na.rm =T),length.out=20) else seq(0,20,length.out=20)
    #br<-seq(min(k.dist()$k,na.rm =T),max(k.dist()$k,na.rm =T),length.out=20)
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

# plot.t <- reactive({
#     ggplot(trans.dist(),aes(x=clust))+geom_histogram()
#   })
# 
# plot.s<-reactive({
#     ggplot(s.dist(),aes(x=s)) + geom_histogram()
#   })

plot.comp <- reactive({
  if(is.null(max.component()))
    #ggplot(data.frame(x=0,y=5,t="No data to plot"),aes(x=x,y=y,label=t)) + geom_text(size=20)
    NULL
  else
    ggplot(max.component(),aes(x=year,y=size.max.component)) + geom_line()+labs(y="Maximum component size [%]",title="Graph of the maximum component relative size in terms of number of nodes")+ylim(0,1)
  })
  
plot.judges <- reactive({
    ggplot(judges.year(),aes(x=year,y=number.judges))+geom_line()+labs(y="Number of judges",title="Graph showing number of judges in court in following years")+ylim(0,max(judges.year()$number.judges))
  })

plot.coop<- reactive({
  if(is.null(judges.coop.year()))
    #ggplot(data.frame(x=0,y=5,t="No data to plot"),aes(x=x,y=y,label=t)) + geom_text(size=20)
    NULL
  else
    ggplot(judges.coop.year(),aes(x=year,y=coop))+geom_line()+labs(y="Diversity of judging teams [%]",title="Graph showing diversity of judging teams in following years")+ylim(0,1)
  })

plot.judgments <- reactive({
    ggplot(judgments.year(),aes(x=year,y=number.judgments))+geom_line()+labs(y="Number of judgments",title="Graph showing number of judgments in specified court in following years")+ylim(0,max(judgments.year()$number.judgments))
  })

plot.sex<-reactive({
  sex.final<-sex.dist()
  ctemp<-count(subset.judges.clean(),c("JudgeName","JudgeSex"))
  sexcourt<-count(ctemp[-3],"JudgeSex")
  frac<-sexcourt$freq[sexcourt$JudgeSex=="F"]/(sexcourt$freq[sexcourt$JudgeSex=="F"]+sexcourt$freq[sexcourt$JudgeSex=="M"])
  #vl<-round(mean(sex.final$frac.female),2)
  vl<-round(frac,2)
  h<-hist(sex.final$frac.female,breaks=seq(0,1,0.1))
  g<-ggplot(sex.final,aes(x=frac.female))+geom_histogram(breaks=seq(0,1,0.1))+labs(x="Fraction of female judges in judgment team",title="Histogram of female judges preferences")
  g<-g+geom_segment(x =vl, y =0 , xend =vl, yend =Inf ,size=0.7,col="red")+geom_text(data=NULL,x=vl+0.15,y=max(h$counts),label=paste("Mean: ",vl,sep=""))
  g
})
output$plot.top.chart<-renderPlot({
  top<-judges.top.court()
  ggplot(top,aes(x=N.of.judgments,y=JudgeName,size=N.of.judgments))+geom_point()+labs(x="Number of judgments",y="Judge Name",title="TopChart for judges is speciified court")+scale_size_continuous(range = c(3,15))+geom_segment(x =0, y =nrow(top):1 , aes(xend =(N.of.judgments-N.of.judgments/35)), yend = nrow(top):1,size=0.7)+theme(axis.title.x = element_text(face="bold", colour="#990000", size=10),axis.text.y  = element_text(angle=0, vjust=0.5, size=10),legend.position="none")
},width=800,height=600)

  output$plot.multi<-renderPlot({
    multiplot(plot.k(),plot.w(),plot.comp(),plot.judges(),plot.judgments(),plot.coop(),plot.sex(),cols=2)
  },width=800,height=1500)
})
