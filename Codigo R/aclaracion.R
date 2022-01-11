#titulo. Aclaracion#
#Autor: Israel Garcia Solares#
#idioma: Espanol#
#License: CC4#
install.packages("ineq")
install.packages("gender")
install.packages("genderdata", repos = "http://packages.ropensci.org")
install.packages("Rtools")
devtools::install_github("ropensci/gender")
devtools::install_github("lmullen/gender-data-pkg")
devtools::install_github("ropensci/genderdata")
install.packages("gender")
install.packages("genderdata", repos = "http://packages.ropensci.org")libs<-c('tm','stringi')
library(ineq)
library(tidyverse)
library(plyr)
lapply(libs,require, character.only= TRUE)
library(devtools)
library(usethis)
library(Rtools)
library(gender)
library(genderdata)
setwd('~/Codigo R')

####UDG####
base<-read.csv("udgprimer.csv", na.strings=c("", "NA", " "))
primas<-read.csv("primasudg.csv", na.strings=c("", "NA", " "))
base$dias<-as.numeric(as.character(str_extract(base$fechatermino, "^(\\d+)(?=/)")))-as.numeric(as.character(str_extract(base$fechainicio, "^(\\d+)(?=/)")))
base$period<-ifelse(base$dias>15, "mensual", "quincenal")
base$dias<-NULL
base$menbruto<-ifelse(base$period=="mensual", base$bruto, base$bruto*2)
profs<-paste(c("TECN", 'PROF', 'ACADEM', 'RECTOR', "INV"), collapse="|")
base<-base[which(!is.na(base$puesto)),]
base$name<-paste(base$apellido1, " ", base$apellido2, ", ", base$nombre, sep="")
for (i in 1:10){
  base$name<-str_replace_all(base$name, "(\\s)(?=[,])|(\\b)NA(\\b)|[0]", "")  
}
base<-base[order(base$id),]
base$prof<-NA
for(i in 1:nrow(base)){
  if(str_detect(base$puesto[i], profs)){
    base$prof[i]<-1
  }else if((base$id[i]==base$id[i-1])&!is.na(base$prof[i-1])){
    base$prof[i]<-1
  }else{
    base$prof[i]<-0
  }
}
base<-base[order(base$name),]
for(i in 2:nrow(base)){
  if((base$prof[i]==0)&(!is.na(base$name[i]))&(!is.na(base$name[i-1]))&(base$name[i]==base$name[i-1])&(base$prof[i-1]==1)){
    base$prof[i]<-1
  }
}
base$academ<-ifelse(str_detect(base$puesto, profs), base$puesto, NA)
primna<-primas[which(is.na(primas$adbruto)),]
denominacion<-data.frame(den=unique(primna$denominacion))
denominacion$per<-as.numeric(str_extract(denominacion$den, "(\\d+)[.](\\d+)(?=[%])|(\\d+)(?=[%])"))
denominacion$dias<-as.numeric(str_extract(denominacion$den, "(\\d+)(?=(\\s)dmas)"))
primna<-left_join(primna, denominacion, by=c("denominacion"="den"))
prima<-rbind.fill( primas[which(!is.na(primas$adbruto)), ], primna)
prima$periodicidad<-tolower(ifelse(!is.na(prima$periodicidad), prima$periodicidad, 
                                   ifelse(str_detect(prima$denominacion, "anual"), "anual", "mensual")))
primna<-prima[which(is.na(prima$adbruto)),]
primna<-left_join( primna, subset(base, select=c("id", "menbruto")))
primna$pro<-as.numeric(primna$dias)/30
primna$adbruto<-ifelse(!is.na(primna$per), primna$per*primna$menbruto/100, primna$pro*primna$menbruto)
prima2<-rbind(subset(prima,!is.na(prima$adbruto)), 
              subset(primna, select=c("id", "denominacion", "adbruto", "adneto", "periodicidad", "per", "dias")))
prima3<-prima2[which(!is.na(prima2$adbruto)),]
prima3$adbruto<-ifelse(prima3$periodicidad=="anual", prima3$adbruto/12, 
                       ifelse(prima3$periodicidad=="mensual", prima3$adbruto, 
                              ifelse(prima3$periodicidad=="quincenal", prima3$adbruto*2, 
                                     prima3$adbruto/4)))
primat<-ddply( prima3, "id", summarize, estimulos=sum(adbruto))
completo<-left_join(base, primat, by="id")
completo<-completo[which(completo$prof==1),]
completo$inv<-ifelse(str_detect(completo$academ, "(\\s)INV"), 1, 2)
completo<-completo[order(completo$name, completo$inv),]
completo$primary<-ifelse(completo$name!=lag(completo$name), 1, 0)
completona<-subset(completo, completo$name==", ")
completo2<-subset(completo, completo$name!=", ")
tcompleto<-ddply(completo2, "name", summarize, menbruto=sum(menbruto), estimulos=sum(estimulos))
completo3<-subset(completo2, completo2$primary==1)
completo3$menbruto<-NULL
completo3$estimulos<-NULL
completo3<-left_join(completo3, tcompleto)
completo4<-rbind(completo3, completona)
completo4$total<-completo4$menbruto+completo4$estimulos
completo4$name<-ifelse(completo4$name==", ", "Anon", completo4$name)
write.csv(completo4, "udg.csv")
