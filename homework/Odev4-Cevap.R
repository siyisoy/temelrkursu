#Soru 1
data(chickwts)  
hist(chickwts$weight,breaks=17,labels=TRUE,xlim=c(0,500),ylim=c(0,12),xlab="Agirlik",ylab="Sayi",main="Tavuk Agirliklari")

#Soru2
data(iris) 
attach(iris)
boxplot(Sepal.Length~Species,main= "Sepal Length")
plot(Petal.Width, Sepal.Length,xlab="Petal.Width", ylab="Sepal.Length")
detach(iris)

#Soru3
data(birthwt,package = "MASS")  
attach(birthwt)
t=table(race, smoke)
colnames(t)<-c("Sigara icmeyen", "Sigara icen")
rownames(t)<-c("Beyaz","Siyah","Diger")
barplot(t,legend.text = TRUE,beside = TRUE, ylim=c(0,60),main="Sigara icme ve irk iliskisi")
detach(birthwt)

#Soru4
data(Pima.tr,package = "MASS")      
attach(Pima.tr)  
length(which(skin>=40))  #skin>40 sayısı
any(skin==60)  #skin=60 var mı
mean(glu[which(bmi<30)])  # bmi<30 olanların glu ortalaması 
mean(glu[bmi<30])         # bmi<30 olanların glu ortalaması which kullanmadan
max(npreg[age>25])           # age>25 olanlarda npreg değerlerinin en büyüğü
length(which(bp>40 & bp<60))   # bp değeri 40 ile 60 arasında olanların sayısı
length(bp[bp>40 & bp<60])      # bp değeri 40 ile 60 arasında olanların sayısı
sum(skin[bp>40 & bp<60])      # bp değeri 40 ile 60 arasında olanların skin değerleri toplamı
hist(skin[bmi>30],label=T)    # bmi>30 olanlarda skin değerlerinin histogramı
boxplot(glu~type)            # type faktörüne göre (type=Yes ve type=No) glu değerlerine ait boxplotlar

#Soru5
tapply(skin,type,mean)  

#Soru6
data(mammals,package = "MASS")
yeniveri<-mammals[!(rownames(mammals) %in% c("Asian elephant","African elephant")),]   # Asya ve Afrika fili cikarilarak elde edilen veriseti 
# "Asian elephant" ve "African elephant" birer satir ismi (rowname) olduguna dikkat. %in% komutu ile satir ismi Asian elephant ve African elephant olanlar 
#  seciliyor. Sonra ! ile bu ifadenin tersini saglayanlar aliniyor. Daha iyi anlamak icin ilk once
mammals[rownames(mammals) %in% c("Asian elephant","African elephant"),]  # komutunu calistirmayi deneyebilirsiniz (! yok bu komutta)
plot(yeniveri)
#######
mammals$sira<-1:62      ## verisetinden Asya ve Afrika fillerini cikarmak icin sira (indeks) degiskeni ekleniyor
mammals[order(mammals$body),]    ##  Asya ve afrika fillerinin vucut agirliklari en buyuk oldugu icin veriseti agirliga gore siralaniyor
yeniveri<-mammals[mammals$sira!=19 & mammals$sira!=33,]   ## Bu siralamada en sondaki iki satir icin sira degerlerinin 19 ve 33 oldugu goruldugu icin bu degerlere sahip satirlar cikariliyor  
plot(yeniveri[,1:2])     ## ,1:2 ile verisetindeki sadece 1. ve 2. sutunlar seciliyor. 3.sutundaki bizim ekledigimiz sira degiskeni alinmiyor.