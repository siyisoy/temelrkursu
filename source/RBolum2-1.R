#######  R ile Temel Istatistik Bolum 2-Kisim 1  ###########

##### Ucretsiz Site ve Kitap #############

####  https://learningstatisticswithr.com/  

####### Tanimlayici Istatistikler   ################

data(mtcars)
attach(mtcars)
tapply(mpg,vs,mean)    # vs faktorune gore mpg ortalamalarini bulur
by(mpg,vs,mean)       # vs faktorune gore mpg ortalamalarini bulur
aggregate(mpg,list(vs),mean)    # vs faktorune gore mpg ortalamalarini bulur
aggregate(mpg,list(vs,gear),mean)   # vs ve gear faktorlerine gore mpg ortalamalarini bulur
detach(mtcars)
with(mtcars,tapply(mpg,vs,mean))   # with komutu ile icerideki komut (tapply) mtcars verisi uzerinde calistirilir
library(psych)                     # Daha once install ettigimiz psych paketini kullanmak icin yukler
describe(mtcars)                   # psych paketi icerisindeki describe komutu ile mtcars verisinin tanimlayici istatistikleri bulunuyor

with(mtcars,by(mtcars[,3:4],gear,colMeans))
describeBy(mtcars,mtcars$cyl)     # psych paketi icerisindeki describeby komutu ile cyl faktorune gore tanimlayici istatistikler elde ediliyor
summary(mtcars$mpg)               # another way of getting simple statistics :)
library(DescTools)                  # daha once install edilmis DescTools paketi kullanim icin yukleniyor
Desc(mtcars)                      # DescTools paketi icerisindeki Desc komutu ile tanimlayici istatistikler elde ediliyor
attach(mtcars)
table(gear,carb,vs)               # gear,carb ve vs faktorlerinin capraz tablosu olusturuluyor
xtabs(~gear+carb)                 # yet another way of cross tabulating gear & carb
gearf<-factor(gear,labels=c("3vites","4vites","5vites"))     # seviyeleri anlamli ifadeler olan faktorler elde ediliyor
carbf<-factor(carb,labels=c("1carb","2carb","3carb","4carb","6carb","8carb"))
t<-table(gearf,carbf)
prop.table(t)                     # capraz tablodaki yuzdelikler hesaplaniyor
prop.table(t,1)
round(prop.table(t,2),2)          # yuzdelik degerleri yuvarlaniyor
library(gmodels)
CrossTable(gearf,carbf,prop.t=F,prop.r=F,prop.chisq=F)   # another way of cross tabulating gear & carb
detach(mtcars)

####### T test   ################

data(mtcars)
attach(mtcars)
str(mtcars) 

t.test(mpg~vs)                # vs faktorune gore mpg degerleri icin t testi yapiyor. Varyanslarin homojen olmadigi varsayimi altinda (Welch testi) 
t.test(mpg~vs,var.equal=TRUE) # Varyanslarin homojen oldugu varsayimi altinda t testi 

mpg1<-mpg[vs==1]
mpg2<-mpg[vs==0]
t.test(mpg1,mpg2)             # yet another way of doing t test for mpg values

##### Homogenity of Variances    ###################

library(car)
leveneTest(mpg~vs)            # Varyanslarin homojenligini bulmak icin Levene Testi yapiliyor, ama vs faktor olmadigi icin hata veriyor
leveneTest(mpg,as.factor(vs))
t.test(mpg~vs,var.equal=TRUE)
t.test(mpg~vs,var.equal=TRUE,alternative="less")    # alternatif hipotez "<" olunca yapilacak t testi
options(scipen=999)  # ya da 
format(3.416e-05,scientific = FALSE)          # 3.416e-05 gibi ifadelerden kurtulmak(!) icin kullanilacak bir komut
sonuc<-t.test(mpg~vs,var.equal=TRUE)
sonuc$conf.int                               #  t testi sonucu ortaya cikan confidence interval

###### Paired T test #################

data(anorexia, package="MASS")              # MASS paketi icerisindeki anorexia verisi yukleniyor
with(anorexia,t.test(Postwt, Prewt, paired=TRUE) )  # Postwt ve prewt degerlerine paired t testi yapiliyor
colMeans(anorexia[,2:3])

with(anorexia[anorexia$Treat=="Cont",],t.test(Postwt, Prewt, paired=TRUE))   # Sadece Treat="Cont" yani kontrol grubundakilerin Postwt ve Prewt degerlerine paired t testi yapiliyor


####### Mann Whitney U test,  Wilcoxon Rank-Sum Test  ################

data(mtcars)
with(mtcars,wilcox.test(mpg~vs))              # Mann Whitney U testi
wilcox.test(mpg~vs,alternative="greater")

####### Wilcoxon Test, Wilcoxon Signed Rank Test #################

data(anorexia, package="MASS")
with(anorexia, wilcox.test(Postwt, Prewt, paired=TRUE))    # Wilcoxon testi

####### ANOVA    ################

data(iris)
attach(iris)
library(psych)
describeBy(Sepal.Length,Species)
library(car)
str(iris)  

###### Homogenity of Variances ############

leveneTest(Sepal.Length~Species)
bartlett.test(Sepal.Length~Species)    # another way of assessing homogenity of variances 
fligner.test(Sepal.Length~Species)     # another more robust way of assessing homogenity of variances 

#### OneWay ANOVA  ####################

oneway.test(Sepal.Length~Species)                       # ANOVA varyanslarin homojen olmadigi varsayilarak (ie, Welch Testi)
oneway.test(Sepal.Length~Species,var.equal = TRUE)      # ANOVA varyanslarin homojen oldugu varsayilarak
myanova<-aov(Sepal.Length~Species)                      # Another way of doing ANOVA with some differences 
summary(myanova)

library(gplots)
plotmeans(Sepal.Length~Species,xlab="Species",              # gplots paketindeki plotmeans komutu
          ylab="Sepal Length", main="Means Plot\nwith 95% CI",n.label = FALSE,ylim=c(4,7),barwidth = 3) 
myanova
str(myanova)
myanova$contrasts                                       # Anovada kullandigimiz kontrast
coefficients(myanova)
tapply(Sepal.Length,Species,mean)
plot(myanova)                                           # Anova icin diagnostik grafikler

contrasts(Species)<-contr.helmert                       # Species faktorune Helmert kontrasti tanimliyoruz ve ANOVA yi tekrarliyoruz
myanova2<-aov(Sepal.Length~Species)
contrastlabels1<-c("helmert1"=1,"helmert2"=2)
summary(myanova2,split=list(Species=contrastlabels1))
summary.lm(myanova2)

contrasts(Species)<-cbind("repeated1"=c(1,-1,0),"repeated2"=c(0,1,-1))          # Species faktorune user defined bir kontrast tanimliyoruz
myanova3<-aov(Sepal.Length~Species)
summary.lm(myanova3)

contrasts(Species)<-contr.poly
myanova4<-aov(Sepal.Length~Species)
summary.lm(myanova4)

#use contr.sum 
#For further reference http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm

####### Post Hoc Tests    ############################
TukeyHSD(myanova)                                       # Post Hoc Tukey HSD testi
plot(TukeyHSD(myanova, conf.level=.95))
with(iris, pairwise.t.test(Sepal.Length, Species,p.adj="bonferroni", paired=FALSE))   # Bonferroni
library(multcomp)
summary(glht(myanova,linfct=mcp(Species="Dunnett")))    # Post hoc Dunnett testi (genelde kontrol grubu oldugunda kullanilir)
summary(glht(myanova,linfct=mcp(Species="Tukey")),test = adjusted("Shaffer"))
# test = adjusted("Westfall")

library(DescTools)
ScheffeTest(myanova)

####### Normallik Testleri    ################

shapiro.test(Sepal.Length)        #Shapiro Wilk Testi
library(nortest)
ad.test(Sepal.Length)             #Anderson Darling testi
cvm.test(Sepal.Length)            #Cramer Von Mises testi
lillie.test(Sepal.Length)         #Lillifors duzeltmeli Kolmogorov Smirnov testi
qqnorm(Sepal.Length)              #QQ Grafigi
qqline(Sepal.Length)
hist(Sepal.Length)

####### Kruskal Wallis Test    ################

data(iris)
attach(iris)
kruskal.test(Sepal.Length~Species)      #Sepal.Length icin Kruskal Wallis testi
library(agricolae)
kr<-kruskal(Sepal.Length,Species)
library(PMCMR)
posthoc.kruskal.nemenyi.test(Sepal.Length,Species) 
library(pgirmess)
kruskalmc(Sepal.Length~Species, iris, probs = 0.05)   #Kruskal Wallkis post hoc testleri 
library(dunn.test)
dunn.test(Sepal.Length,Species,method = "Sidak")
