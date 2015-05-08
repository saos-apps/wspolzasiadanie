source("funkcje.R")

judges.top.c<-function(data.judges){
  temp<-plyr::count(data.judges,"JudgeName")
  temp<-subset(temp,!is.na(temp$JudgeName))
  top<-head(temp[order(temp$freq,decreasing = T),],min(10,nrow(temp)))
  top$JudgeName<-sapply(as.character(top$JudgeName),function(x) paste(unlist(strsplit(x," "))[1:2],collapse=" "))
  table1 <- table(top$JudgeName)
  c<-sapply(seq(min(10,nrow(temp))),function(x) which(names(table1)==top$JudgeName[x]))
  levels1 <- names(table1)[rev(c)]
  top$JudgeName <- factor(top$JudgeName, levels = levels1)
  names(top)[2]<-"N.of.judgments"
  top
}

g.court<-function(data.judges,data.judges.net){
  dt<-data.table(data.judges)
  vert <- dt[, list(JudgeSex=head(JudgeSex,1), DivisionCode2=paste(DivisionCode2,collapse=" ")), by=c("JudgeName")]
  vert$DivisionCode2<-sapply(vert$DivisionCode2,function(x) unique(unlist(strsplit(x," "))))
  g<-graph.data.frame(data.judges.net,directed = F,vertices=vert)
  V(g)$vertex.shape<-NA
  V(g)$vertex.shape<-"fcircle"
  g
}

g.simplify.c<-function(g.court){
  g<-simplify(g.court,remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
  if(ecount(g)>0) E(g)$weight<-sapply(E(g)$CourtCode,length) else E(g)$weight=0
  g    
}

g.mark.matrix<-function(g.simple.c){
  g<-g.simple.c
  div.un<-unique(unlist(V(g)$DivisionCode2))
  matrix<-sapply(div.un,function(x) sapply(V(g)$DivisionCode2,function(y) x %in% y))
  matrix
}

g.mark.list<-function(g.simple.c,g.mark.matrix){
  g<-g.simple.c
  div.un<-unique(unlist(V(g)$DivisionCode2))
  matrix<-g.mark.matrix
  list<-sapply(seq(length(div.un)),function(x) which(matrix[,x]))
  names(list)<-rep(brewer.pal(12,"Set3"),ceiling(length(div.un)/12))[seq(length(div.un))]
  names(list)<-addalpha(names(list),0.9)
  list$labels<-div.un
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
}

g.color.div<-function(g.simple.c,g.mark.matrix,divisions.sub){
  g<-g.simple.c
  div.un<-unique(unlist(V(g)$DivisionCode2))
  matrix<-g.mark.matrix
#   list<-sapply(seq(length(div.un)),function(x) which(matrix[,x]))
#   names(list)<-rep(brewer.pal(12,"Set3"),ceiling(length(div.un)/12))[seq(length(div.un))]
#   names(list)<-addalpha(names(list),0.8)
#   list$labels<-as.vector(sapply(div.un,function(x) unique(divisions.sub$DivisionName2[which(divisions.sub$DivisionCode2==x)])))  
  
{if(dim(matrix)[2]==1){
  list<-list(a=which(matrix[]))
  names(list)<-brewer.pal(9,"Set1")[1]
  list$labels<-divisions.sub$DivisionName
}
else{
  list<-sapply(seq(length(div.un)),function(x) which(matrix[,x]))
  names(list)<-rep(brewer.pal(9,"Set1"),ceiling(length(div.un)/12))[seq(length(div.un))]
  names(list)<-addalpha(names(list),0.8)
  list$labels<-as.vector(sapply(div.un,function(x) unique(divisions.sub$DivisionName2[which(divisions.sub$DivisionCode2==x)])))
}
} 
list
}

g.color.pie<-function(g.simple.c){
  g<-g.simple.c
  div.un<-unique(unlist(V(g)$DivisionCode2))
  matrix<-sapply(div.un,function(x) sapply(V(g)$DivisionCode2,function(y) x %in% y))  
  names<-rep(brewer.pal(9,"Set1"),ceiling(length(div.un)/9))[seq(length(div.un))]
  names<-addalpha(names,0.7)
  values<-lapply(V(g)$DivisionCode2,function(x) rep(1/length(x),length(x)))
  colors<-lapply(seq(nrow(matrix)),function(x) names[matrix[x,]])
  shapes<-sapply(seq(vcount(g)),function(x) ifelse(length(values[[x]])>1,"pie",V(g)$vertex.shape[x]))
  V(g)$colour<-colors
  V(g)$pie.values<-values
  V(g)$vertex.shape<-shapes
  g
}

j.coop.year<-function(subset.judges.c,subset.judgments.c,g.simple.c){
  years<-unique(subset.judges.c$judgmentYear)
{if(ecount(g.simple.c)==0)
  list1=NULL
 else{
   list1<-as.data.frame(t(sapply(years,function(y){
     sub.judges<-subset(subset.judges.c,judgmentYear==y)
     n.judges<-length(unique(sub.judges$JudgeName))
     sub.judgments<-subset(subset.judgments.c,judgmentYear==y)
     g.sub<-simplify(graph.data.frame(sub.judgments,directed = F,vertices=NULL),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
     coop.array<-plyr::count(sub.judges,"judgmentID")$freq
     coop<-ecount(g.sub)/max.unique.links(n.judges,coop.array)
     coop=ifelse(is.nan(coop),0,coop)
     coop=ifelse(coop>1,1,coop)
     c(y,coop)
   })))
   names(list1)<-c("year","coop")
 }
  }
list1
}

judgm.year<-function(subset.judges.c){
  sub<-subset.judges.c[!duplicated(subset.judges.c$judgmentID),]
  df<-plyr::count(sub,"judgmentYear")
  names(df)<-c("year","number.judgments")
  df
}

j.year<-function(subset.judges.c){
  temp<-plyr::count(subset.judges.c,c("JudgeName","judgmentYear"))
  names(temp)[3]<-"dd"
  df<-plyr::count(temp,"judgmentYear")
  names(df)<-c("year","number.judges")
  df
}

max.comp<-function(g.court){
  g<-g.court
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
}

plot.sex.distribution<-function(subset.j.clean){
  sexc<-plyr::count(subset.j.clean,c("judgmentID","JudgeSex"))
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
  ctemp<-plyr::count(subset.j.clean,c("JudgeName","JudgeSex"))
  sexcourt<-plyr::count(ctemp[-3],"JudgeSex")
  frac<-sexcourt$freq[sexcourt$JudgeSex=="F"]/(sexcourt$freq[sexcourt$JudgeSex=="F"]+sexcourt$freq[sexcourt$JudgeSex=="M"])
  vl<-round(frac,2)
  h<-hist(sex.final$frac.female,breaks=seq(0,1,0.1))
  g<-ggplot(sex.final,aes(x=frac.female))+geom_histogram(breaks=seq(0,1,0.1))+labs(x="Fraction of female judges in judgment team",title="Histogram of female judges preferences")
  g<-g+geom_segment(x =vl, y =0 , xend =vl, yend =Inf ,size=0.7,col="red")+geom_text(data=NULL,x=vl+0.15,y=max(h$counts),label=paste("Mean: ",vl,sep=""))
  g  
}