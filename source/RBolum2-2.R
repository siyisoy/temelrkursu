#######  R ile Temel Istatistik Bolum 2-Kisim 2  ###########

###  Ucretsiz Istatistik Kitabi
#####  https://www.openintro.org/stat/textbook.php

### R contrib/docs
### https://cran.r-project.org/doc/


####### Reshape (Wide-Long Data Formats)    ################


wide<-reshape(Indometh, v.names="conc", timevar="time", idvar="Subject",direction="wide")      # long formattan wide formata gecis
long<-reshape(wide,v.names="conc",timevar="time",idvar="Subject",varying=list(2:12),direction = "long")     # wide formattan long formata gecis

####### Repeated Measures ANOVA    ################

Indometh$time<-as.factor(Indometh$time)
repanova<-with(Indometh,aov(conc~time+Error(Subject/time)))    # aov komutu ile repeated measures ANOVA
summary(repanova)

library(ez)
repanova2<-ezANOVA(data = Indometh,dv=conc,wid=Subject,within=time,type=1)  # ezANOVA komutu ile repeated measures ANOVA
print(repanova2)
boxplot(conc~time,data=Indometh) 

library(nlme)
repanova3<- lme(conc ~ time, random = ~1|Subject/time, data=Indometh)    # lme komutu ile repeated measures ANOVA
summary(repanova3)

########## Post hoc testler    ############

library(multcomp)
summary(glht(repanova3, linfct=mcp(time = "Tukey")))                  # Post hoc Tukey testi
summary(glht(repanova3, linfct=mcp(time = "Dunnett")))                # Post hoc Dunnett testi
library(gplots)
plotmeans(Indometh$conc~Indometh$time,xlab="Time",
          ylab="Concentration", main="Means Plot\nwith 95% CI",n.label=FALSE)   # CI grafigi

####### Friedman Test    ################

with(Indometh,friedman.test(conc~time|Subject))            # Friedman testi 

library(pgirmess)
with(Indometh,friedmanmc(conc,time,Subject))             # Post hoc comparisons
with(Indometh,pairwise.wilcox.test(conc,time,p.adjust.method = "bonferroni",paired = TRUE))  # Bonferroni adjusted post hoc comparisons
library(PMCMR)
with(Indometh,posthoc.friedman.nemenyi.test(conc,time,Subject))   # Nemenyi testi

####### Chi Square Test    ################

library(MASS)
data(survey)
attach(survey)
tbl<-table(Sex,Smoke)              # Ki kare yapmak icin tablo olusturuluyor
chisq.test(tbl)                    # Tabloya Ki kare testi uygulaniyor
table(Sex,M.I)
chisq.test(Sex,M.I)                # Sex ve Fold degiskenleri icin Yates continuity corrected Pearson Ki kare testi
chisq.test(Sex,M.I,correct = FALSE)   # Sex ve M.I degiskenleri icin Pearson Ki kare testi
             

freqs<-c(2,2,3,8,7,2)              
data.matrix<-matrix(freqs, nrow=2)          # Frekanslari bilinen bir tablo olusturuluyor
rownames(data.matrix)<-c("Celexa","placebo")       # Satir isimleri veriliyor (Ki kare icin gerekli degil)
colnames(data.matrix)<-c("worse","same","better")  # Sutun isimleri veriliyor (Ki kare icin gerekli degil)
chisq.test(data.matrix)
fisher.test(data.matrix)                      #  Beklenen degerler dusuk ciktigi icin Fisher exact testi yapiliyor

####### Correlation   ################

data(iris)
attach(iris)
plot(Sepal.Length,Petal.Length)
plot(iris)
cor(Sepal.Length,Petal.Length)                # Sepal.Length ve Petal.Length icin Pearson korelasyon katsayisi hesaplaniyor
cor(Sepal.Length,Petal.Length,method = "spearman")   # Ayni degiskenler icin spearman katsayisi
cor(iris[,1:4],method = "spearman")             # Tum degiskenler icin spearman katsayisi
library(psych)
corr.test(iris[,1:4])
partial.r(iris[,1:4],x=c(1,2),y=c(3))            #Sepal.Length ile Sepal.Width arasinda Pedal.Width etkisi alindiktan sonraki Kismi korelasyon katsayisi
partial.r(iris[,1:4],x=c(1,2),y=c(3,4))          #Sepal.Length ile Sepal.Width arasinda Pedal.Length ve Pedal.Width etkisi alindiktan sonraki Kismi korelasyon katsayisi
partial.r(iris[,1:4],x=c(1,2,3),y=c(4))          #Sepal.Length, Sepal.Width ve Pedal.Length arasinda Pedal.Width etkisi alindiktan sonraki Kismi korelasyon katsayilari
