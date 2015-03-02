require(plyr)
require(saos)
require(devtools)
require(shiny)
require(RgoogleMaps)
#require(dismo)
#library(sp)  # classes for spatial data
#library(raster)  # grids, rasters
#library(rasterVis)  # raster visualisation
#library(maptools)
#library(rgeos)
#library(rgdal)
require(datasets)
require(googleVis)
require(sqldf)
require(date)
require(stringi)
require(igraph)
#install_github('ramnathv/rCharts')
#install_github('ramnathv/rMaps')

# 0.wczytanie danych
input<-readRDS("data//common_courts_data.RDS")

# 0. dump orzeczeń próbka ok 10777 s
dmp.judg<-get_dump_judgments(judgmentStartDate = Sys.Date() - 180,judgmentEndDate = Sys.Date() - 90)
judges<-saos::extract(dmp.judg,"judges")
unlist(dmp.judg[[1]]$judges)

# 1. próba lokalizacji sądów (194 z 291)
options(stringsAsFactors = F)
courts<-get_dump_courts(T)
courts.location<-t(sapply(seq(nrow(courts)),function(x) getGeoCode(courts$name[x])))
no.judg<-sapply(seq(nrow(courts)),function(x) count_judgments(ccCourtCode =courts$code[x]))
c.loc<-transform(courts.location,lat.lon=as.character(paste(lat,lon,sep=":")))
c.sample<-cbind(courts$name,c.loc,no.judg)
c.sample<-c.sample[which(!is.na(c.sample$lon)),]
names(c.sample)[1]<-"name"
write.table(c.sample,"app-map/data/locations.csv")

# 2.app-map data sample (LDZ)
t.un<-unique(types$judgmentType)
types.list<-list(1,2,3,4,5)
names(types.list)<-c(t.un,"ALL")

judgments.ldz<-search_judgments(ccCourtCode = "15251000",limit=NULL,force=T)
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
