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
ggplot(top,aes(x=N.of.judgments,y=JudgeName))+geom_point()

#error handle

judges.sub<-subset(judges,CourtCode==15251000)
judgments.sub<-subset(judges.net,CourtCode==15251000)

dt <- data.table(judges.sub)
dt.out <- dt[, list(JudgeSex=head(JudgeSex,1), DivisionCode=paste(DivisionCode,collapse=" ")), by=c("JudgeName")]
dt.out$DivisionCode<-sapply(dt.out$DivisionCode,function(x) unique(unlist(strsplit(x," "))))

  g<-graph.data.frame(judgments.sub,directed = F,vertices=dt.out)
  V(g)$vertex.shape<-NA
  V(g)$vertex.shape<-ifelse(V(g)$JudgeSex=="M","ftriangle",ifelse(V(g)$JudgeSex=="F","fcircle","fstar"))
  V(g)$vertex.shape[which(is.na(V(g)$vertex.shape))]<-"fstar"
  g

g<-simplify(g,remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
E(g)$weight<-sapply(E(g)$CourtCode,length)
E(g)$type="real"
E(g)$weight=0
g    

#mark list
div.un<-unique(unlist(V(g)$DivisionCode))
matrix<-sapply(div.un,function(x) sapply(V(g)$DivisionCode,function(y) x %in% y))
list<-sapply(seq(length(div.un)),function(x) which(matrix[,x]))
names(list)<-rainbow(length(div.un))
list$labels<-div.un

layg<-layout.fruchterman.reingold(g,weights=E(g)$weight)
plog(g,layg,list[-length(list)])

gb<-g
sapply(seq(length(div.un)),function(x) {
vadd<-which(matrix[,x])
c1<-t(combn(vadd,2))
gb<<-add.edges(gb,c1)
E(gb)$type[which(is.na(E(gb)$type))]<<-"fake"
E(gb)$weight[which(is.na(E(gb)$weight))]<<-3
#E(gb)$type[which(is.na(E(gb)$type))]<-"fake"
#E(gb)$weight[which(is.na(E(gb)$weight))]<-3
})

gb<-simplify(gb,remove.multiple = F,remove.loops = T)
laygb<-layout.fruchterman.reingold(gb,weights=E(gb)$weight)
ul<-unlist(list[-length(list)])
names(ul)<-substr(names(ul),1,9)
ul<-as.list(ul)
plog(gb,laygb,ul)

