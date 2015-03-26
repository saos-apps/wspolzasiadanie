require(data.table)
require(plyr)
require(sqldf)
require(date)
require(stringi)
require(XML)
require(httr)
require(saos)

## 0.wczytanie danych
# COMMON
input<-readRDS("app-net/data/common_courts_data.RDS")
locations<-readRDS("../saos-apps/courts_coords.RDS")
#Supreme, Constitutional Tribunal
judg.supreme<-search_judgments(courtType = "SUPREME",limit=NULL,force=TRUE)
judg.const<-search_judgments(courtType ="CONSTITUTIONAL_TRIBUNAL",limit=NULL,force=TRUE)

judgments.list<-unlist(input,recursive = F)
class(judgments.list)<-c("saos_search","list")
j.judgmentType<-saos::extract(judgments.list,"judgmentType")
j.judgmentDate<-saos::extract(judgments.list,"judgmentDate")
j.division<-saos::extract(judgments.list,"division")
j.judges<-saos::extract(judgments.list,"judges")
j.divisions<-subset(j.division,select=c("id","court.code","court.name","code","name"))
names(j.divisions)<-c("judgmentID","CourtCode","CourtName","DivisionCode","DivisionName")

c.judgmentType<-saos::extract(judg.const,"judgmentType")
c.judgmentDate<-saos::extract(judg.const,"judgmentDate")
c.judges<-saos::extract(judg.const,"judges")
c.divisions<-data.frame(judgmentID=c.judgmentType$id)
c.divisions$CourtCode="00000001"
c.divisions$CourtName="Trybunał Konstytucyjny"
c.divisions$DivisionCode="0000001"
c.divisions$DivisionName="Trybunał Konstytucyjny"

s.judgmentType<-saos::extract(judg.supreme,"judgmentType")
s.judgmentDate<-saos::extract(judg.supreme,"judgmentDate")
s.judges<-saos::extract(judg.supreme,"judges")
s.division<-saos::extract(judg.supreme,"division")
s.division$CourtCode="00000002"
s.division$CourtName="Sąd Najwyższy"
s.divisions<-subset(s.division,select=c("id","CourtCode","CourtName","division.id","name"))
names(s.divisions)<-c("judgmentID","CourtCode","CourtName","DivisionCode","DivisionName")

jdivisions<-rbind(j.divisions,c.divisions,s.divisions)

divisions<-unique(jdivisions[,-1]) #tabela1: divisions


jjudgmentType<-rbind(j.judgmentType,c.judgmentType,s.judgmentType)
j.judgmentDate<-transform(j.judgmentDate,judgmentYear=as.POSIXlt(j.judgmentDate$judgmentDate)$year+1900)
c.judgmentDate<-transform(c.judgmentDate,judgmentYear=as.POSIXlt(c.judgmentDate$judgmentDate)$year+1900)
s.judgmentDate<-transform(s.judgmentDate,judgmentYear=as.POSIXlt(s.judgmentDate$judgmentDate)$year+1900)
jjudgmentDate<-rbind(j.judgmentDate,c.judgmentDate,s.judgmentDate)
j.judges<-j.judges[,-3]
c.judges<-c.judges[,-3]
s.judges<-s.judges[,-3]
jjudges<-rbind(j.judges,c.judges,s.judges)

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
temp$sex[which(subLast(temp$name)=="a" & is.na(temp$sex) & nchar(temp$name)>1)]<-"F" #niedokładne
temp$sex[which(subLast(temp$surname)=="a" & is.na(temp$sex)  & nchar(temp$surname)>1)]<-"F" #niedokładne
left<-which(is.na(temp$sex))
judges$JudgeSex<-NA
lapply(seq(nrow(temp)),function(x) judges$JudgeSex[llist1[[x]]]<<-temp$sex[x])

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

saveRDS(judgments,"app-net/data/judgments.rds")
saveRDS(judges,"app-net/data/judges.rds")
saveRDS(divisions,"app-net/data/divisions.rds")
saveRDS(judges.net,"app-net/data/judges.net.rds")
saveRDS(courts,"app-net/data/courts.rds")

