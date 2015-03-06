require(ggmap)
require(plyr)
require(saos)
require(devtools)
require(shiny)
require(RgoogleMaps)
require(datasets)
require(googleVis)
require(sqldf)
require(date)
require(stringi)
require(igraph)
#install_github('ramnathv/rCharts')
#install_github('ramnathv/rMaps')

## 0.wczytanie danych
input<-readRDS("app-net/data/common_courts_data.RDS")
locations<-readRDS("../saos-apps/courts_coords.RDS")
judgments.list<-unlist(input,recursive = F)
class(judgments.list)<-c("saos_search","list")
j.judgmentType<-saos::extract(judgments.list,"judgmentType")
j.judgmentDate<-saos::extract(judgments.list,"judgmentDate")
j.division<-saos::extract(judgments.list,"division")
j.judges<-saos::extract(judgments.list,"judges")
j.divisions<-subset(j.division,select=c("id","court.code","court.name","code","name"))
names(j.divisions)<-c("judgmentID","CourtCode","CourtName","DivisionCode","DivisionName")
jdivisions<-j.divisions
divisions<-unique(j.divisions[,-1]) #tabela1: divisions
jjudgmentType<-j.judgmentType
j.judgmentDate<-transform(j.judgmentDate,judgmentYear=as.POSIXlt(j.judgmentDate$judgmentDate)$year+1900)
jjudgmentDate<-j.judgmentDate
j.judges<-j.judges[,-3]
jjudges<-j.judges

judges<-sqldf("select d.judgmentID, j.name as JudgeName, j.specialRoles, d.CourtCode,d.DivisionCode,dat.judgmentDate,dat.judgmentYear from jjudges j
              left join jdivisions d on
              j.id=d.judgmentID
              left join jjudgmentDate dat on j.id=dat.id") #tabela 2: judges

judgments<-sqldf("select div.judgmentID, t.judgmentType, dat.judgmentDate,dat.judgmentYear, div.CourtCode, div.DivisionCode from jjudgmentType t
                 left join jjudgmentDate dat on dat.id=t.id
                 left join jdivisions div on div.judgmentID=t.id") #tabela 2: judgments
courts<-sqldf("select distinct CourtCode, CourtName from divisions")
courts<-sqldf("select c.* , l.lon,l.lat from courts c
                 left join locations l
                 on c.CourtName=l.name")

## stworzenie sieci współzasiadania i 

judges.net<-sqldf("select j1.JudgeName as name1, j2.JudgeName as name2, j1.judgmentID, j1.judgmentDate,j1.judgmentYear, j1.specialRoles as specialRole1, j2.specialRoles as specialRole2,
j1.CourtCode, j1.DivisionCode from judges j1
inner join judges j2 on j1.judgmentID=j2.judgmentID and j1.JudgeName<>j2.JudgeName")
                  
## zapis do pliku jako dane wejściowe do aplikacji
write.table(judgments,"app-net/data/judgments.csv")
write.table(judges,"app-net/data/judges.csv")
write.table(divisions,"app-net/data/divisions.csv")
write.table(judges.net,"app-net/data/judges.net.csv")
write.table(courts,"app-net/data/courts.csv")

## narysowanie sieci wszystkich sędziów
g1<-graph.data.frame(judges.net,directed = F,vertices = NULL)
g1s<-simplify(g1,remove.loops = T)
l1s<-layout.fruchterman.reingold(g1s)
pdf("net1.pdf",width = 1000,height=1000)
plog2(g1s,l1s)
dev.off()


## współpraca sędziów - test dla jednego sądu 15502000

s.judgments<-subset(judgments,CourtCode==15502000)
s.judges<-subset(judges,CourtCode==15502000)
s.judges.net<-subset(judges.net,CourtCode==15502000)
g1<-graph.data.frame(s.judges.net,directed = F,vertices=NULL)
g1s<-simplify(g1,remove.multiple = T,remove.loops = T, edge.attr.comb ="concat")
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

## mobilność sędziów 

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




  
## typy spraw?
t.un<-unique(types$judgmentType)
types.list<-list(1,2,3,4,5)
names(types.list)<-c(t.un,"ALL")
