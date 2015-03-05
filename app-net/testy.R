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

## stworzenie sieci współzasiadania i 

judges.net<-sqldf("select j1.JudgeName as name1, j2.JudgeName as name2, j1.judgmentID, j1.judgmentDate,j1.judgmentYear, j1.specialRoles as specialRole1, j2.specialRoles as specialRole2,
j1.CourtCode, j1.DivisionCode from judges j1
inner join judges j2 on j1.judgmentID=j2.judgmentID and j1.JudgeName<>j2.JudgeName")
                  
## zapis do pliku jako dane wejściowe do aplikacji
write.table(judgments,"app-net/data/judgments.csv")
write.table(judges,"app-net/data/judges.csv")
write.table(divisions,"app-net/data/divisions.csv")
write.table(judges.net,"app-net/data/judges.net.csv")

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

## typy spraw?
t.un<-unique(types$judgmentType)
types.list<-list(1,2,3,4,5)
names(types.list)<-c(t.un,"ALL")

judgments.ldz<-search_judgments(ccCourtCode = "15251000",limit=200,force=T)
j.ldz<-get_judgments(judgments.ldz)
ldz.count<-count_judgments(ccCourtCode ="15251000")
ldz.judges<-saos::extract(j.ldz,"judges")
ldz.divisions<-saos::extract(j.ldz,"division")
ldz.divisions<-ldz.divisions[,1:3]
ldzdivisions<-ldz.divisions
names(ldzdivisions)[2]<-"divisionid"
divisions.un<-ldz.divisions[!duplicated(ldz.divisions$name),]
ldz.judges<-subset(ldz.judges,select=c("id","name"))
ldzjudges<-ldz.judges
ldz.judges.net<-sqldf("select j1.id,j1.name as name1, j2.name as name2,d.divisionid from ldzjudges j1 
                      inner join ldzjudges j2 on
                      j1.id=j2.id and
                      j1.name<>j2.name
                      left join ldzdivisions d
                      on d.id = j1.id")
ldzjudgesnet<-ldz.judges.net
ldz.dates<-saos::extract(j.ldz,"judgmentDate")
ldz.dates<-ldz.dates[order(ldz.dates$id),]
ldz.dates<-transform(ldz.dates,year=as.numeric(substr(judgmentDate,1,4)))
ldzdates<-ldz.dates
ldz.judgment.sum<-count(ldz.dates,"year")
ldz.judge<-count(ldz.judges.year,"year")
merge<-sqldf("select d.*, j.* from ldzjudgesnet j
             left join ldzdates d 
             on d.id=j.id")
ldz.judges.year<-sqldf("select d.*, j.* from ldzjudges j
                       left join ldzdates d
                       on j.id = d.id")
ldz.judges.year<-ldz.judges.year[,c("name","id","judgmentDate","year")]

write.table(merge,"app-map/data/net.csv")
write.table(ldz.judges.year,"app-map/data/judges.csv")
write.table(ldz.dates,"app-map/data/dates.csv")
write.table(merge,"app-net/data/net.csv")
write.table(ldz.judges.year,"app-net/data/judges.csv")
write.table(ldz.divisions,"app-net/data/divisions.csv")

# 3. sample viz

df.graph<-subset(merge,divisionid==1018,select=c("name1","name2","year"))
g1<-graph.data.frame(df.graph,directed = F,vertices = NULL)
g1<-simplify(g1,remove.multiple = T,remove.loops = F)
l1<-layout.fruchterman.reingold(g1)
plog2(g1,l1)
g1<-gvisGeoMap(c.sample,"lat.lon",hovervar = "courts$name",numvar="no.judg",options=list(region="155", dataMode="markers",width=600, height=400))
g2<-gvisGeoChart(c.sample,"lat.lon",hovervar = "courts$name",sizevar = "no.judg",options=list(region="155", dataMode="markers",width=600, height=400))
g3<-PlotOnStaticMap(lat = c.sample$lat, lon = c.sample$lon, zoom = 5,size=30, cex = 1.4, pch = 19, col = "red")
