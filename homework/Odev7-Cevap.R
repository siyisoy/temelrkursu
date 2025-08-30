#Soru1-2 
data1<-read.table(file="exer.csv",header = TRUE,sep = ",")
data<-reshape(data1,v.names = "PULSE",timevar = "TIME",idvar = "SUBJECT",varying =list(2:4),direction = "long")
data$TIME<-as.factor(data$TIME)  

#Soru3
attach(data)
repanova1<-with(data,aov(PULSE~TIME+Error(SUBJECT/TIME)))
repanova1
library(nlme)
repanova2<- lme(PULSE ~ TIME, random = ~1|SUBJECT/TIME, data=data)  
summary(repanova2)  
library(multcomp)
summary(glht(repanova2, linfct=mcp(TIME = "Tukey")))       
summary(glht(repanova2, linfct=mcp(TIME = "Dunnett")))   

#Soru4
with(data,friedman.test(PULSE~TIME|SUBJECT)) 
library(pgirmess)
with(data,friedmanmc(PULSE,TIME,SUBJECT))  
detach(data)

#Soru5
data(survey,package="MASS")
attach(survey)
tablo<-table(W.Hnd,Clap)         
chisq.test(tablo)                
detach(survey)

#Soru6
data(longley)
attach(longley)
cor.test(Employed,GNP.deflator)
library(psych)
partial.r(longley,x=c(7,1),y=c(5)) 

