#######  R ile Temel Istatistik Bolum 1-Kisim 2  ###########
#######  Import   ################
getwd()                                             # Calisma klasorunu (Working Directory) gosterir
setwd("C:/Documents and Settings/pc/Belgelerim/")   # Calisma klasorunu belirler / kullanilmali
setwd("Documents/RKursu/")
dir()                                               # Calisma klasorundeki dosyalari gosterir
###tab completion
veri<-read.table(file="leukemia.csv",header = TRUE,sep = ";")  # ";" ile ayrilmis dosyayi headerla birlikte import eder
read.table("http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/caffeine.csv")   # Uzaktaki dosyayi import eder
write.table(data, file="veri2.csv", row.names=FALSE,sep=",")   # Bir veriyi dosyaya yazar (export eder)


#######  Temel Islemler   ################
(12+18)*2-14^3*15%%2*2/12  # Islem onceligine gore matematiksel islemleri yapar
?Syntax
exp(2)                   # e nin kuvvetlerini bulur
log(10)                  # e tabanina gore logaritma alir
sqrt(49)                 # Karekok alir
factorial(8)             # Faktoriyel bulur
choose(12,8)             # Kombinasyon hesaplar
round(log(9),3)          # Virgulden sonra 3 basamaga yuvarlar
abs(2-30)                # Mutlak degeri bulur
a<-c(1,2,3) 
b<-c(3,4,5)
a^2+2*b-1
a*b
rnorm(100,10,2)         # Ortalamasi 10, standart sapmasi 2 olan normal dagilimdan 100 sayi uretir
sort(c(18,9,23,10))     # Siralar
order(c(18,9,23,10))    # Sirasini verir
height<-c(98,75,25,48,25)
height>=50              # 50 den buyuk esit olanlar icin mantiksal vektor olusturur
height==48              # 48 e esit olanlari bulur
height!=48              # 48'den farkli olanlari bulur
which(height==25)       # 25'e esit olanlarin indexini verir
which(height>=50)       
all(height>=10)         # Hepsi 10'dan buyuk mu diye karar verir. TRUE ya da FALSE dondurur
any(height<30)          # 10'dan kucuk olan var mi bakar
height[height>=50]      # 50'den buyuk olanlari gosterir
height[height>20 & height<40]  # 20'den buyuk 40'tan kucuk olanlar
sum(height>=50)         # 50'den buyuk esit olanlarin sayisini bulur
sum(which(height>=50))  # 25'e esit olanlarin indekslerini toplar
seq(1,20,5)             # 1'den baslayip 20'ye kadar 5'er sayar 
seq(11,5,-1)            # 11'den baslayip 5'e kadar geriye 1'er sayar
dizi<-1:5
rep(dizi,3)             # diziyi 3 kere tekrarlar
rep(dizi,each=3)        # dizinin jer elemanini 3'er kere tekrarlar
rep(dizi,c(3,2,2,1,1))  # dizinin her elemanini c(3,2,2,1,1) vektorundeki sayilara gore tekrarlar


######  Dataframe   ###############

data("chickwts")
data("mtcars")          # R'da hazir gelen verileri yukler
head(chickwts)          # chickwts in ilk 6 satiri
tail(chickwts)          # chickwts in son 6 satiri
names(chickwts)         # chickwts in sutun basliklari
dim(chickwts)           # chickwts in boyutlari
attach(mtcars)
mpg
cyl
detach(mtcars)
mpg

#######  Histogram   ################
data<-rnorm(250,70,15)
hist(data)             # data verisinin histogramini cizer
histinfo<-hist(data)
hist(data,breaks=20)              # histogramda 20 kesim noktasi olur
hist(data,breaks=20,labels=TRUE)  # veri etiketleri olur
hist(data,breaks=20,labels=TRUE,density=20,ylim=c(0,40),xlim=c(0,140))
hist(data,freq=FALSE,breaks=20,labels=T)   # Yuzdelik olarak histogram cizer
hist(data,freq=FALSE,ylim=c(0,0.05))
hist(data,xlab="Boy",ylab="Yuzde",main="Histogram",probability = TRUE)
curve(dnorm(x, mean=mean(data), sd=sd(data)),col="darkblue", lwd=5, add=TRUE)
lines(density(data),col="red")

h<-hist(data,breaks=20,labels=TRUE,density=15,ylim=c(0,40),xlim=c(0,140))
xfit<-seq(min(data),max(data),length=40) 
yfit<-dnorm(xfit,mean=mean(data),sd=sd(data)) 
yfit<-yfit*diff(h$mids[1:2])*length(data) 
lines(xfit, yfit, col="darkblue", lwd=2)


#######  Boxplot   ################
a<-rnorm(240,100,10)
boxplot(a)                    # a verisine ait boxplot
plotdata<-data.frame(a=a)
ggplot(plotdata,aes(x=factor(0),y=a))+geom_boxplot()+scale_x_discrete(breaks = NULL) +
  xlab(NULL)
qplot(factor(0),a,geom = 'boxplot')
b<-rep(c(1,2,3),each=80)      # bir faktor olusturmak icin 80'er adet 1,2,3 uretiyoruz (Grup1, Grup2 ve Grup3 e karsilik)
f<-factor(b,labels=c("Grup1","Grup2","Grup3"))     # yukaridaki b verisini kullanarak bir f faktoru olusturuyoruz
boxplot(a~f)          # a verisini f faktorune gore boxplotlar cizer
qplot(f,a,geom = 'boxplot')
ggplot(plotdata,aes(x=f,y=a,fill=f))+geom_boxplot()
data(chickwts)       
str(chickwts)
attach(chickwts)
boxplot(weight~feed,data=chickwts)    # feed faktorune gore weight boxplotlari
title(main="Body Weight of Chicks by Type of Diet")  # Grafige ana baslik ekler
detach(chickwts)

chickwts$gender<-factor(sample(c(rep("Male",35),rep("Female",36))))
with(chickwts,table(feed,gender))
boxplot(weight~feed,subset = gender=="Male",col="yellow",data=chickwts,at= 1:6-0.2,boxwex=.4,las=2)
boxplot(weight~feed,subset = gender=="Female",col="red",data=chickwts,add=TRUE,at= 1:6+0.3,boxwex=.4,las=2)

boxplot(weight~gender+feed,data=chickwts,las=2)


#######  Scatterplot   ################
library(MASS)
data(mammals)
plot(mammals$brain,mammals$body,xlab = "Brain",ylab="Body")  # Scatterplot cizer
plot(log(mammals$brain),log(mammals$body)) # yerine
plot(mammals$brain,mammals$body,log="xy") # kullanilabilir
fit<-lm(log(mammals$body)~log(mammals$brain))     # Regresyon modeli olusturur
abline(fit)           # Regresyon dogrusunu cizer

library(car)
scatterplot(mammals$brain,mammals$body,xlab = "Brain",ylab="Body",
            main="Mammals' Brain and Body",labels = row.names(mammals))
scatterplot(log(mammals$brain),log(mammals$body),xlab = "Brain",ylab="Body",
            main="Mammals' Brain and Body",labels = row.names(mammals))

#######  Barplot   ################
data(mtcars)
attach(mtcars)
a<-table(carb)
barplot(table(carb),space=0.1,xlab="Carburator",width=.6,xlim=c(0,5),names.arg=c("Carb1","Carb2","Carb3","Carb4","Carb6","Carb8"))
t<-table(vs,carb)
colnames(t)<-c("1Carb","2Carb","3Carb","4Carb","6Carb","8Carb")
rownames(t)<-c("V","S")
barplot(t,legend.text=TRUE)
barplot(t,legend.text=TRUE,beside = TRUE)
barplot(t,legend.text=TRUE,horiz=TRUE,las=1,beside = TRUE)
prop.table(t,2)
barplot(prop.table(t,2),legend.text = TRUE)
