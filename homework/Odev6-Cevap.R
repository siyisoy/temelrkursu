data(Pima.tr,package = "MASS")
attach(Pima.tr)
set.seed(1234)
yes<-which(type=="Yes")
white<-sample(yes,34)
black<-setdiff(yes,white)
hispanik<-sample(which(type!="Yes"),50)
t<-rep(0,200)
t[white]<-1
t[black]<-2
t[hispanik]<-3
Pima.tr$race<-factor(t,labels = c("Other","White","Black","Hispanic"))
attach(Pima.tr)


#Varyanslarin homojenligi homoscedasticity
fligner.test(glu~race)        # varyanslar homojen
fligner.test(bmi~race)        # varyanslar homojen degil

#Oneway ANOVA
anova1<-aov(glu~race)
summary(anova1)              # Irka gore glu ortalamalari degisiyor
oneway.test(bmi~race,var.equal = FALSE)    # Varyanslar farkli oldugu icin oneway.test komutunu kullandik ve Irklara gore bmi degerlerinin degistigini bulduk
anova2<-aov(bmi~race)        # Post hoc test icin bu ANOVA yi da yapmaliyiz
summary(anova2)

#Post hoc testler Tukey
library(multcomp)
summary(glht(anova1,linfct=mcp(race="Tukey")),test = adjusted("Shaffer"))   # Hispanic- Other ve Black- White glu karsilastirmalari anlamli degil.
summary(glht(anova2,linfct=mcp(race="Tukey")),test = adjusted("Shaffer"))   # Hispanic- Other ve Black- White bmi karsilastirmalari anlamli degil.

#Post hoc test Scheffe
library(DescTools)
ScheffeTest(anova1)                   # Hispanic- Other ve Black- White glu karsilastirmalari anlamli degil.
ScheffeTest(anova2)                   # Sadece White- Other ve Hispanic- White bmi karsilastirmalari anlamli.

#Post hoc test Dunnett

#multcomp paketindeki glht komutu ile
summary(glht(anova1,linfct=mcp(race="Dunnett")))    # Other grubu ile Hispanik glu ortalamasi farkli degil. Digerleri farkli.

# ya da DescTools paketindeki DunnetTest komutu ile
DunnettTest(glu~race)                               # Ayni sonuc

# bmi icin Dunnett testleri
summary(glht(anova2,linfct=mcp(race="Dunnett")))   #Sadece White- Other bmi karsilastirmasi anlamli. Digerleri anlamsiz
DunnettTest(bmi~race)                              # Ayni sonuc

# glu icin normallik testleri
shapiro.test(glu)        #Shapiro Wilk Testi   Sonuc: Normal degil
library(nortest)
ad.test(glu)             #Anderson Darling testi  Sonuc:Normal degil
cvm.test(glu)            #Cramer Von Mises testi  Sonuc:Normal degil
lillie.test(glu)         #Lillifors duzeltmeli Kolmogorov Smirnov testi  Sonuc:Normal degil
qqnorm(glu)              #QQ Grafigi
qqline(glu)    
hist(glu)

# race faktorunun seviyelerinde glu nun normalliginin test edilmesi
tapply(glu,race,shapiro.test)
tapply(glu,race,ad.test)
tapply(glu,race,qqnorm)
tapply(glu,race,hist)
