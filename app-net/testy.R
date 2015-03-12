require(data.table)
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
require(XML)
require(httr)
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

judges.net<-sqldf("select j1.JudgeName as name1, j2.JudgeName as name2,j1.JudgeSex as Sex1,j2.JudgeSex as Sex2, j1.judgmentID, j1.judgmentDate,j1.judgmentYear, j1.specialRoles as specialRole1, j2.specialRoles as specialRole2,
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

#przyporządkowanie płci - 96.2% udało się! czekam jeszcze na bazę nazwisk w odmianie
#potem spróbuję jeszcze z końcówkami
#max 1.2% dla kobiet z przyporządkowanych może być źle (przypisanie po końcówce "a")

subLast <- function(x){
  tolower(substr(x, nchar(x), nchar(x)))
}
options(stringsAsFactors = F)
j.names<-data.frame(name=as.character(unique(judges$JudgeName)))
llist1<-lapply(j.names$name,function(x) which(judges$JudgeName==x))
temp<-as.data.frame(t(sapply(seq(nrow(j.names)),function(x) {t<-strsplit(j.names$name[x]," ");c(unlist(t)[1],unlist(t)[2])})))
names(temp)<-c("name","surname")
temp$sex<-NA


url<-"http://www.behindthename.com/names/gender/masculine/usage/polish"
html2=GET(url)
cont<-content(html2,as="text")
parsed.html=htmlParse(cont,asText=T)
names.male<-xpathSApply(parsed.html,"//div/b/a",xmlValue)
names.male<-data.frame(name=unlist(lapply(names.male,function(x) as.character(unlist(strsplit(x," "))[1]))))
names.male<-tolower(names.male[,1])
names.male<-names.male[-which(names.male=="maria")]
url<-"http://www.behindthename.com/names/gender/feminine/usage/polish"
html2=GET(url)
cont<-content(html2,as="text")
parsed.html=htmlParse(cont,asText=T)
names.female<-xpathSApply(parsed.html,"//div/b/a",xmlValue)
names.female<-data.frame(name=unlist(lapply(names.female,function(x) as.character(unlist(strsplit(x," "))[1]))))
names.female<-tolower(names.female[,1])
#wiki
url<-"http://pl.wiktionary.org/wiki/Indeks:Polski_-_Imiona"
html2=GET(url)
cont<-content(html2,as="text")
parsed.html=htmlParse(cont,asText=T)
names<-xpathSApply(parsed.html,"//ul/li/a",xmlValue)
names.female2<-tolower(names[(which(names=="Abigail")):(which(names=="Żywisława"))])
names.male2<-tolower(names[(which(names=="Abdon")):(which(names=="Żytek"))])
names.male<-unique(c(names.male,names.male2))
names.female<-unique(c(names.female,names.female2))
                   
sapply(names.male,function(x) temp$sex[which(tolower(temp$name)==x)]<<-"M")
sapply(names.female,function(x) temp$sex[which(tolower(temp$name)==x)]<<-"F")
sapply(names.male,function(x) temp$sex[which(tolower(temp$surname)==x & is.na(temp$sex))]<<-"M")
sapply(names.female,function(x) temp$sex[which(tolower(temp$surname)==x & is.na(temp$sex))]<<-"F")
left<-which(is.na(temp$sex))
tlist<-sapply(names.male,function(x) {
  #w<-which(sapply(temp$name[left],function(y) regexec(x,y,ignore.case = T)[[1]][1]>0)==TRUE)
  w<-which((regexpr(x,temp$name[left],ignore.case = T)>0)==TRUE)
  temp$sex[left[w]]<<-"M"
  })
left<-which(is.na(temp$sex))
tlist<-sapply(names.male,function(x) {
  w<-which((regexpr(x,temp$surname[left],ignore.case = T)>0)==TRUE)
  temp$sex[left[w]]<<-"M"
})
left<-which(is.na(temp$sex))
tlist<-sapply(names.female,function(x) {
  w<-which((regexpr(x,temp$name[left],ignore.case = T)>0)==TRUE)
  temp$sex[left[w]]<<-"F"
})
left<-which(is.na(temp$sex))
tlist<-sapply(names.female,function(x) {
  w<-which((regexpr(x,temp$surname[left],ignore.case = T)>0)==TRUE)
  temp$sex[left[w]]<<-"F"
})
left<-which(is.na(temp$sex))
temp$name<-sub("/*[.]/*","",temp$name,ignore.case = T)
temp$surname<-sub("/*[.]/*","",temp$surname,ignore.case = T)
# temp$name<-sub("/*-/*","",temp$name,ignore.case = T)
# temp$name<-sub("/*sso/*","",temp$name,ignore.case = T)
# temp$name<-sub("/*ssa/*","",temp$name,ignore.case = T)
# temp$name<-sub("/*del/*","",temp$name,ignore.case = T)
# temp$surname<-sub("/*-/*","",temp$surname,ignore.case = T)
# temp$surname<-sub("/*sso/*","",temp$surname,ignore.case = T)
# temp$surname<-sub("/*ssa/*","",temp$surname,ignore.case = T)
# temp$surname<-sub("/*del/*","",temp$surname,ignore.case = T)
# temp$name<-sub("/*so/*","",temp$name,ignore.case = T)
# temp$name<-sub("/*sa/*","",temp$name,ignore.case = T)
# temp$surname<-sub("/*so/*","",temp$surname,ignore.case = T)
# temp$surname<-sub("/*sa/*","",temp$surname,ignore.case = T)
temp$sex[which(subLast(temp$name)=="a" & is.na(temp$sex) & nchar(temp$name)>1)]<-"F" #niedokładne
temp$sex[which(subLast(temp$surname)=="a" & is.na(temp$sex)  & nchar(temp$surname)>1)]<-"F" #niedokładne
left<-which(is.na(temp$sex))
judges$JudgeSex<-NA
lapply(seq(nrow(temp)),function(x) judges$JudgeSex[llist1[[x]]]<<-temp$sex[x])

write.table(judges,"app-net/data/judges.csv")

## ile sędziów w więcej niż 1 wydziale/izbie?

head(judges)
judges2<-sqldf("select distinct JudgeName, CourtCode, DivisionCode from judges")
c1<-count(judges2,c("JudgeName","CourtCode"))

#mark group po wydziałach

judges.net.court<-subset(judges.net,CourtCode== 15500000)
g.court<-graph.data.frame(judges.net.court,directed = F,vertices=NULL)
g.s<-simplify(g.court,remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )

div.un<-unique(unlist(div.vect))
ymax<-length(div.un)
div.vect<-as.vector(E(g.s)$DivisionCode)
list<-lapply(seq(ymax),function(y) {
  which.div<-sapply(seq(ecount(g.s)),function(x) div.un[y] %in% unique(div.vect[[x]]))
  v<-get.edges(g.s,E(g.s)[which.div])
  unique(c(v[,1],v[,2]))  
})

names(list)<-rainbow(length(div.un))
list$labels<-div.un
list2<-list[-length(list)]
plot.igraph(g.s,mark.groups =list2,mark.col=names(list2))

g.leg<-graph.empty(3,F)
V(g.leg)$label=div.un
V(g.leg)$color=names(list)
lay.leg<-matrix(c(rep(0,3),seq(1,3)),byrow = F,nrow = 3)
plot.igraph(g.leg,vertex.label.dist=1,layout=lay.leg)

judges.sub<-subset(judges,CourtCode==15500000)
judgments.sub<-subset(judges.net,CourtCode==15500000)
# coop dla całego sądu

g<-g.s
div.vect<-as.vector(E(g)$DivisionCode)
div.un<-unique(unlist(div.vect))
years<-unique(judges.sub$judgmentYear)
list1<-as.data.frame(t(sapply(years,function(x){
  x<-2012
  sub.judges<-subset(judges.sub,judgmentYear==x)
  n.judges<-length(unique(judges.sub$JudgeName))
  sub.judgments<-subset(judgments.sub,judgmentYear==x)
  g.sub<-simplify(graph.data.frame(sub.judgments,directed = F,vertices=NULL),remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
  coop.array<-count(judges.sub,"judgmentID")$freq
  coop<-ecount(g.sub)/max.unique.links(n.judges,coop.array)
  c(x,coop)
})))
names(list1)<-c("year","coop")
list1

#dotchart
courts[which(regexpr("Złot",courts$CourtName)>0),]
judges.sub<-subset(judges,CourtCode==15501025)
judgments.sub<-subset(judges.net,CourtCode==15501025)

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

judges.sub<-subset(judges,CourtCode==15501025)
judgments.sub<-subset(judges.net,CourtCode==15501025)

#subgraph.court<-reactive({


dt <- data.table(judges.sub)
dt.out <- dt[, list(JudgeSex=head(JudgeSex,1), DivisionCode=paste(DivisionCode,collapse=" ")), by=c("JudgeName")]
dt.out$DivisionCode<-sapply(dt.out$DivisionCode,function(x) unique(unlist(strsplit(x," "))))

  vert<-judges.sub[!duplicated(judges.sub$JudgeName),c("JudgeName","JudgeSex","DivisionCode")]
  g<-graph.data.frame(judgments.sub,directed = F,vertices=dt.out)
  V(g)$vertex.shape<-NA
  V(g)$vertex.shape<-ifelse(V(g)$JudgeSex=="M","ftriangle",ifelse(V(g)$JudgeSex=="F","fcircle","fstar"))
  V(g)$vertex.shape[which(is.na(V(g)$vertex.shape))]<-"fstar"
  g

g<-simplify(g,remove.multiple = T,remove.loops = T,edge.attr.comb ="concat" )
E(g)$weight<-sapply(E(g)$CourtCode,length)
g    

#mark list
div.un<-unique(unlist(V(g)$DivisionCode))
div.vect<-as.vector(E(g)$DivisionCode)
div.un<-unique(unlist(div.vect))
ymax<-length(div.un)
list<-lapply(seq(ymax),function(y) {
  which.div<-sapply(seq(ecount(g)),function(x) div.un[y] %in% unique(div.vect[[x]]))
  v<-get.edges(g,E(g)[which.div])
  unique(c(v[,1],v[,2]))
})

div.un<-unique(unlist(V(g)$DivisionCode))
list<-sapply(div.un,function(x) which(V(g)$DivisionCode==x))
names(list)<-rainbow(length(div.un))
list$labels<-div.un
list
}

