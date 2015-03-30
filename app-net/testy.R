require(data.table)
require(plyr)
require(saos)
require(devtools)
require(shiny)
require(datasets)
require(sqldf)
require(date)
require(stringi)
require(igraph)
require(XML)
require(httr)
require(ggmap)
require(RColorBrewer)
judgments<-read.table("data/judgments.csv")
judges<-read.table("data/judges.csv")
divisions<-read.table("data/divisions.csv")
judges.net<-read.table("data/judges.net.csv")
courts<-read.table("data/courts.csv")

## narysowanie sieci wszystkich sędziów
g1<-graph.data.frame(judges.net,directed = F,vertices = NULL)
g1s<-simplify(g1,remove.loops = T)
l1s<-layout.fruchterman.reingold(g1s)
pdf("net1.pdf",width = 1000,height=1000)
plog2(g1s,l1s)
dev.off()

## współpraca sędziów - test dla jednego sądu 15502000
s.judgments<-subset(judgments,CourtCode==15502000)
s.judges<-subset(judges,CourtCode==15502000,c("JudgeName","JudgeSex"))
s.judges<-s.judges[!duplicated(s.judges$JudgeName),]
s.judges.net<-subset(judges.net,CourtCode==15502000)
g1<-graph.data.frame(s.judges.net,directed = F,vertices=s.judges)
g1s<-simplify(g1,remove.multiple = T,remove.loops = T, edge.attr.comb ="concat")
V(g1s)$vertex.shape<-ifelse(V(g1s)$JudgeSex=="M","ftriangle",ifelse(V(g1s)$JudgeSex=="F","fcircle","fstar"))
V(g1s)$vertex.shape[which(is.na(V(g1s)$vertex.shape))]<-"fstar"

no.judges<-length(V(g))
judges.per.case<-count(s.judges,"judgmentID")
judges.per.case<-subset(judges.per.case,freq>1)
possible.links<-min(sum(choose(judges.per.case$freq,2)),choose(no.judges,2))
coop.per.case<-count(s.judges.net,"judgmentID")

g.comp<-t(sapply(unique(E(g1)$year),function(x) {
  g.sub<-subgraph.edges(g1,E(g1)[E(g1)$year==x],delete.vertices = T)
  g.sub<-simplify(g.sub)
  cl<-clusters(g.sub)
  c(x,max(cl$csize)/vcount(g.sub))
  }))

## mobilność sędziów  - moim zdaniem zbyt mała żeby można było fajnie pokazać
judgesunique<-sqldf("select distinct JudgeName, CourtCode from judges")
judges.sub<-sqldf("select j1.CourtCode CourtCode1, j2.CourtCode CourtCode2, j1.JudgeName from judgesunique j1
                  inner join judgesunique j2 
                  on j1.JudgeName=j2.JudgeName and
                  j1.CourtCode<>j2.CourtCode")
g.mob<-graph.data.frame(judges.sub,directed=F)
gname<-data.frame(Vname=V(g.mob)$name)
gcoord<-sqldf("select c.lon, c.lat from gname n
              left join courts c on
              c.CourtCode=n.Vname")
map<-get_map(location = "Poland",zoom=6,color="bw")
ggmap(map)
par(new=T)
plog(g.mob,as.matrix(gcoord)) #mapa dla całej PL

#dotchart
courts[which(regexpr("Wroc",courts$CourtName)>0),]
judges.sub<-subset(judges,CourtCode==15502500)
judgments.sub<-subset(judges.net,CourtCode==15502500)

temp<-count(judges.sub,"JudgeName")
temp<-subset(temp,!is.na(temp$JudgeName))
top<-head(temp[order(temp$freq,decreasing = T),],min(10,nrow(temp)))
top$JudgeName<-sapply(as.character(top$JudgeName),function(x) paste(unlist(strsplit(x," ")),collapse=" "))
table1 <- table(top$JudgeName)
c<-sapply(seq(min(10,nrow(temp))),function(x) which(names(table1)==top$JudgeName[x]))
levels1 <- names(table1)[rev(c)]
top$JudgeName <- factor(top$JudgeName, levels = levels1)
names(top)[2]<-"N.of.judgments"

ggplot(judges.top.court(),aes(x=N.of.judgments,y=JudgeName))+geom_point(aes(size=N.of.judgments))+labs(x="Number of judgments",y="Judge Name",title="TopChart for judges is speciified court")+geom_hline(aes(yintercept=JudgeName))
ggplot(top,aes(x=N.of.judgments,y=JudgeName,size=N.of.judgments))+geom_point()+scale_size_continuous(range = c(5,20))+geom_segment(aes(x =0, y =10:1 , xend =(N.of.judgments-N.of.judgments/35), yend = 10:1),size=0.7)+theme(axis.title.x = element_text(face="bold", colour="#990000", size=10),axis.text.y  = element_text(angle=0, vjust=0.5, size=10),legend.position="none")

#pie chart
courts[which(regexpr("Kędzierz",courts$CourtName)>0),]
judges.sub<-subset(judges,CourtCode==15501515)
judgments.sub<-subset(judges.net,CourtCode==15501515)

dt<-data.table(judges.sub)
vert <- dt[, list(JudgeSex=head(JudgeSex,1), DivisionCode=paste(DivisionCode,collapse=" ")), by=c("JudgeName")]
vert$DivisionCode<-sapply(vert$DivisionCode,function(x) unique(unlist(strsplit(x," "))))
g<-graph.data.frame(judgments.sub,directed = F,vertices=vert)
V(g)$vertex.shape<-NA
V(g)$vertex.shape<-ifelse(V(g)$JudgeSex=="M","ftriangle",ifelse(V(g)$JudgeSex=="F","fcircle","fstar"))
V(g)$vertex.shape[which(is.na(V(g)$vertex.shape))]<-"fstar"
g

g<-simplify(g,remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
if(ecount(g)>0) E(g)$weight<-sapply(E(g)$CourtCode,length)
E(g)$type="real"
g     

div.un<-unique(unlist(V(g)$DivisionCode))
matrix<-sapply(div.un,function(x) sapply(V(g)$DivisionCode,function(y) x %in% y))  
names<-rep(brewer.pal(12,"Set3"),ceiling(length(div.un)/12))[seq(length(div.un))]
names<-addalpha(names,0.75)
values<-lapply(V(g)$DivisionCode,function(x) rep(1/length(x),length(x)))
colors<-lapply(seq(nrow(matrix)),function(x) names[matrix[x,]])
V(g)$colour<-colors
V(g)$pie.values<-values
g

lay<-layout.fruchterman.reingold(g,weights=E(g)$weight)
plog.pie(g,layg)

#mark.group
#list<-sapply(seq(length(div.un)),function(x) setdiff(which(matrix[,x]),which(rowSums(matrix)>1)))
list<-sapply(seq(length(div.un)),function(x) which(matrix[,x]))
names(list)<-rep(brewer.pal(12,"Set3"),ceiling(length(div.un)/12))[seq(length(div.un))]
names(list)<-addalpha(names(list),0.8)
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
plog(g,lay,list2[-length(list2)])

#rozkład kobiet i mężczyzn
judges.fm<-subset(judges.sub,!is.na(JudgeSex))
sexc<-count(judges.fm,c("judgmentID","JudgeSex"))
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

ctemp<-count(judges.fm,c("JudgeName","JudgeSex"))
sexcourt<-count(ctemp[-3],"JudgeSex")
frac<-sexcourt$freq[sexcourt$JudgeSex=="F"]/(sexcourt$freq[sexcourt$JudgeSex=="F"]+sexcourt$freq[sexcourt$JudgeSex=="M"])


sex.final<-transform(sexa,frac.female=freqf/(freqf+freqm))
#vl<-round(mean(sex.final$frac.female),2)
vl=round(frac,2)
br<-seq(min(sex.final$frac.female),max(sex.final$frac.female),length.out=11)
h<-hist(sex.final$frac.female,breaks=br)
g<-ggplot(sex.final,aes(x=frac.female))+geom_histogram(breaks=br)+labs(x="Fraction of female judges in judgment team",title="Histogram of female judges preferences")
g<-g+geom_segment(x =vl, y =0 , xend =vl, yend =Inf ,size=0.7,col="red")+geom_text(data=NULL,x=vl+0.15,y=max(h$counts),label=paste("Mean: ",vl,sep=""))
g

#infinite recursion problem

courts[which(regexpr("Wroc",courts$CourtName)>0),]
judges.sub<-subset(judges,CourtCode==15500000 )
judgments.sub<-subset(judges.net,CourtCode==15500000 )

dt<-data.table(judges.sub)
vert <- dt[, list(JudgeSex=head(JudgeSex,1), DivisionCode=paste(DivisionCode,collapse=" ")), by=c("JudgeName")]
vert$DivisionCode<-sapply(vert$DivisionCode,function(x) unique(unlist(strsplit(x," "))))
#judgments.sub2<-judgments.sub[,c("name1"   ,     "name2" ,    "judgmentID"  , "judgmentDate" ,"judgmentYear"  ,"CourtCode" ,   "DivisionCode")]
g<-graph.data.frame(judgments.sub,directed = F,vertices=vert)
V(g)$vertex.shape<-NA
V(g)$vertex.shape<-ifelse(V(g)$JudgeSex=="M","ftriangle",ifelse(V(g)$JudgeSex=="F","fcircle","fstar"))
V(g)$vertex.shape[which(is.na(V(g)$vertex.shape))]<-"fstar"
g

g.sim<-simplify(g,remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
if(ecount(g.sim)>0) E(g.sim)$weight<-sapply(E(g.sim)$CourtCode,length)
E(g.sim)$type="real"
g.sim

div.un<-unique(unlist(V(g)$DivisionCode))
matrix<-sapply(div.un,function(x) sapply(V(g)$DivisionCode,function(y) x %in% y))  
names<-rep(brewer.pal(12,"Set3"),ceiling(length(div.un)/12))[seq(length(div.un))]
names<-addalpha(names,0.75)
values<-lapply(V(g)$DivisionCode,function(x) rep(1/length(x),length(x)))
colors<-lapply(seq(nrow(matrix)),function(x) names[matrix[x,]])
V(g)$colour<-colors
V(g)$pie.values<-values

lay<-layout.fruchterman.reingold(g.sim,weights=E(g.sim)$weight,area=10000*vcount(g.sim)^2,repulserad=100000*vcount(g.sim)^3)
plog(g.sim,lay)

  years<-unique(judges.sub$judgmentYear)
  {if(ecount(g.sim)==0)
    list1=NULL
  else{
    list1<-as.data.frame(t(sapply(years,function(x){
      print(x)
      x<-years[15]
      sub.judges<-subset(judges.sub,judgmentYear==x)
      n.judges<-length(unique(sub.judges$JudgeName))
      sub.judgments<-subset(judgments.sub,judgmentYear==x)
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



max.unique.links<-function(njudges,array){
  array<-sort(array,T)
  un.links<-choose(njudges,2)
  ordered<-1
  nnext<-2
{ if(length(array)<=1000) {
  #ordered<-fun1(array,njudges,ordered,nnext)
  ret<-fun1(array,njudges,ordered,nnext)
  nnext<-ret$l2
  ifelse(nnext>x*1000 | nnext>length(array),ordered<-ret$l1,ordered<-c(ret$l1,ret$l2))
}
else
{
  for(x in 1:ceiling(length(array)/1000))
  {
    ret<-fun1(array[1:min(x*1000,length(array))],njudges,ordered,nnext)
    #nnext<-ordered[length(ordered)]
    #ordered<-ordered[-length(ordered)]
    nnext<-ret$l2
    ifelse(nnext>x*1000 | nnext>length(array),ordered<-ret$l1,ordered<-c(ret$l1,ret$l2))
    if(sum(array[ordered])==njudges) break
  }
}
}

exist.links<-sum(choose(array[ordered],2),na.rm = T)
pos.links<-sum(choose(array[-ordered],2)-choose(ceiling(array[-ordered]/2),2)-choose(floor(array[-ordered]/2),2))

ifelse((exist.links+pos.links)<un.links,
       c<-exist.links+pos.links,
       c<-un.links
)
c
}

fun1<-function(array,njudges,order,nnext){
{if(length(array)<nnext)
  list(l1=order,l2=nnext)
 else {if(sum(array[c(order,nnext)])<njudges)
   return(fun1(array,njudges,c(order,nnext),nnext+1))
   else
   { if(sum(array[c(order,nnext)])>njudges)
     return(fun1(array,njudges,order,nnext+2))
     else
       list(l1=order,l2=nnext)
   }
 }
}
}

#error x not found

c.code1<-max(courts$CourtCode[which(regexpr("Wrocł",courts$CourtName)>0)])
judges.sub<-subset(judges,CourtCode==c.code1)
judgments.sub<-subset(judges.net,CourtCode==c.code1)
g.sub<- g.court(judges.sub,judgments.sub)
g.sim<-g.simplify.c(g.sub)
judges.coop1<-j.coop.year(judges.sub,judgments.sub,g.sim)
m.matrix<-g.mark.matrix(g.sim)
m.list<-g.mark.list(g.sim,m.matrix)
