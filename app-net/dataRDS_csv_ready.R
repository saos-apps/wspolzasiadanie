library(lubridate)
require(data.table)
require(plyr)
require(dplyr)
require(sqldf)
require(date)
require(stringi)
require(XML)
require(httr)
require(saos)

## 0.wczytanie danych
# COMMON
#input<-readRDS("app-net/data/common_courts_data.RDS")
locations<-readRDS("../../saos-apps/courts_coords.RDS")
#Supreme, Constitutional Tribunal
judg.supreme<-search_judgments(courtType = "SUPREME",limit=NULL,force=TRUE)
judg.const<-search_judgments(courtType ="CONSTITUTIONAL_TRIBUNAL",limit=NULL,force=TRUE)
#judg.common<-search_judgments(courtType = "COMMON",limit=NULL,force=TRUE)
#judgments.list<-unlist(input,recursive = F)
#class(judgments.list)<-c("saos_search","list")

#ściąganie nowych danych, te od Bartka nie mają info o sądach bo są z dumpa
path<-"data/judgments_09042015/"
for (y in seq(1994,2015)){
  for(m in seq(1,12)){
    startd=ymd(paste(y,m,"01",sep="/"))
    obj=paste("common",y,".",m,sep="")
    assign(obj,
           search_judgments(judgmentDateFrom =startd,judgmentDateTo = startd + months(1)-days(1) ,force=TRUE,limit=NULL,courtType="COMMON")
    )
    saveRDS(get(obj),paste0(path,obj,".rds"))
  }
}
for (y in seq(1994,2015)){
  for(m in seq(1,12)){
    startd=ymd(paste(y,m,"01",sep="/"))
    obj=paste("appeal",y,".",m,sep="")
    assign(obj,
           search_judgments(judgmentDateFrom =startd,judgmentDateTo = startd + months(1)-days(1) ,force=TRUE,limit=NULL,courtType="NATIONAL_APPEAL_CHAMBER")
    )
    saveRDS(get(obj),paste0(path,obj,".rds"))
  }
}

common.list<-lapply(seq(1994,2015),function(y) lapply(seq(1,12),function(m) readRDS(paste0(path,paste("common",y,".",m,sep=""),".rds"))))
common.list<-unlist(common.list,recursive = F)
common.list<-unlist(common.list,recursive = F)
class(common.list)<-c("saos_search","list")
comm.judgmentType<-saos::extract(common.list,"judgmentType")
comm.judgmentDate<-saos::extract(common.list,"judgmentDate")
comm.division<-saos::extract(common.list,"division")
comm.judges<-saos::extract(common.list,"judges")
comm.divisions<-subset(comm.division,select=c("id","court.code","court.name","code","name"))
names(comm.divisions)<-c("judgmentID","CourtCode","CourtName","DivisionCode","DivisionName")

# APPEAL
appeal.list<-lapply(seq(1994,2015),function(y) lapply(seq(1,12),function(m) readRDS(paste0(path,paste("appeal",y,".",m,sep=""),".rds"))))
appeal.list<-unlist(appeal.list,recursive = F)
appeal.list<-unlist(appeal.list,recursive = F)
class(appeal.list)<-c("saos_search","list")
app.judgmentType<-saos::extract(appeal.list,"judgmentType")
app.judgmentDate<-saos::extract(appeal.list,"judgmentDate")
app.judges<-saos::extract(appeal.list,"judges")
app.divisions<-data.frame(judgmentID=app.judgmentType$id)
app.divisions$CourtCode="00000003"
app.divisions$CourtName="Krajowa Izba Odwoławcza"
app.divisions$DivisionCode="0000003"
app.divisions$DivisionName="Krajowa Izba Odwoławcza"

# TK
cons.judgmentType<-saos::extract(judg.const,"judgmentType")
cons.judgmentDate<-saos::extract(judg.const,"judgmentDate")
cons.judges<-saos::extract(judg.const,"judges")
cons.divisions<-data.frame(judgmentID=cons.judgmentType$id)
cons.divisions$CourtCode="00000001"
cons.divisions$CourtName="Trybunał Konstytucyjny"
cons.divisions$DivisionCode="0000001"
cons.divisions$DivisionName="Trybunał Konstytucyjny"

# SN
s.judgmentType<-saos::extract(judg.supreme,"judgmentType")
s.judgmentDate<-saos::extract(judg.supreme,"judgmentDate")
s.judges<-saos::extract(judg.supreme,"judges")
s.division<-saos::extract(judg.supreme,"division")
s.division$CourtCode="00000002"
s.division$CourtName="Sąd Najwyższy"
s.divisions<-subset(s.division,select=c("id","CourtCode","CourtName","division.id","name"))
names(s.divisions)<-c("judgmentID","CourtCode","CourtName","DivisionCode","DivisionName")

jdivisions<-rbind(comm.divisions,app.divisions,cons.divisions,s.divisions)

divisions<-unique(jdivisions[,-1]) #tabela1: divisions

cCodes<-unique(divisions$CourtCode)
DivisionName2<-gsub("Odwoławczy",replacement = "",x =divisions$DivisionName,fixed = F)
DivisionName3<-as.vector(sapply(DivisionName2,function(x) paste(strsplit(x," ")[[1]][-c(1,2)],collapse = " ")))
divisions$DivisionName2<-DivisionName3
divisions$DivisionCode2<-NA
for(i in 1:length(cCodes)){
  divs.un<-unique(subset(divisions,CourtCode==cCodes[i]))$DivisionName2
  for(j in 1:length(divs.un)){
    divisions$DivisionCode2[which(divisions$CourtCode==cCodes[i] & divisions$DivisionName2==divs.un[j])]<-j
  }
}

jdivisions<-sqldf("select jd.*, div.DivisionCode2 from jdivisions jd left join divisions div on jd.CourtCode=div.CourtCode and jd.DivisionCode=div.DivisionCode")

jjudgmentType<-rbind(comm.judgmentType,app.judgmentType,cons.judgmentType,s.judgmentType)
comm.judgmentDate<-transform(comm.judgmentDate,judgmentYear=as.POSIXlt(comm.judgmentDate$judgmentDate)$year+1900)
app.judgmentDate<-transform(app.judgmentDate,judgmentYear=as.POSIXlt(app.judgmentDate$judgmentDate)$year+1900)
cons.judgmentDate<-transform(cons.judgmentDate,judgmentYear=as.POSIXlt(cons.judgmentDate$judgmentDate)$year+1900)
s.judgmentDate<-transform(s.judgmentDate,judgmentYear=as.POSIXlt(s.judgmentDate$judgmentDate)$year+1900)
jjudgmentDate<-rbind(comm.judgmentDate,app.judgmentDate,cons.judgmentDate,s.judgmentDate)
comm.judges<-comm.judges[,-3]
app.judges<-app.judges[,-3]
cons.judges<-cons.judges[,-3]
s.judges<-s.judges[,-3]
jjudges<-rbind(comm.judges,app.judges,cons.judges,s.judges)

judges<-sqldf("select d.judgmentID, j.name as JudgeName, j.specialRoles, d.CourtCode,d.DivisionCode,d.DivisionCode2,dat.judgmentDate,dat.judgmentYear from jjudges j
              left join jdivisions d on
              j.id=d.judgmentID
              left join jjudgmentDate dat on j.id=dat.id") #tabela 2: judges

judgments<-sqldf("select div.judgmentID, t.judgmentType, dat.judgmentDate,dat.judgmentYear, div.CourtCode, div.DivisionCode,div.DivisionCode2 from jjudgmentType t
                 left join jjudgmentDate dat on dat.id=t.id
                 left join jdivisions div on div.judgmentID=t.id") #tabela 3: judgments
courts<-sqldf("select distinct CourtCode, CourtName from divisions")
courts<-sqldf("select c.* , l.lon,l.lat from courts c
              left join locations l
              on c.CourtName=l.name") #tabela 4: courts

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
                  j1.CourtCode, j1.DivisionCode,j1.DivisionCode2 from judges j1
                  inner join judges j2 on j1.judgmentID=j2.judgmentID and j1.JudgeName<>j2.JudgeName")

## zapis do pliku jako dane wejściowe do aplikacji
write.table(judgments[1:10,],"app-net/data/judgments.csv")
write.table(judges[1:10,],"app-net/data/judges.csv")
write.table(divisions[1:10,],"app-net/data/divisions.csv")
write.table(judges.net[1:10,],"app-net/data/judges.net.csv")
write.table(courts[1:10,],"app-net/data/courts.csv")

saveRDS(judgments,"app-net/data/judgments.rds")
saveRDS(judges,"app-net/data/judges.rds")
saveRDS(divisions,"app-net/data/divisions.rds")
saveRDS(courts,"app-net/data/courts.rds")
saveRDS(judges.net,"app-net/data/judges.net.rds")

judgments<-readRDS("app-net/data/judgments.rds")
judges<-readRDS("app-net/data/judges.rds")
divisions<-readRDS("app-net/data/divisions.rds")
courts<-readRDS("app-net/data/courts.rds")
