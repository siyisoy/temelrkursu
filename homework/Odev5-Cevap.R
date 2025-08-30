#Soru1

library(psych)
data(Pima.tr,package = "MASS")
describe(Pima.tr)
#type kategorik oldugu icin ona ait frekans ve yuzdeleri verelim
attach(Pima.tr)
table(type)
prop.table(table(type))
#Gruplarda (seker+ ve seker-) varyanslarin homojenligini test edelim
fligner.test(glu~type)
fligner.test(bp~type)
fligner.test(skin~type)
#Varyanslar hepsinde homojen cikti
t.test(glu~type,var.equal=TRUE)
t.test(bp~type,var.equal=TRUE)
t.test(skin~type,var.equal=TRUE)
#nonparametrik mann whitney u yapalim
wilcox.test(glu~type)
wilcox.test(bp~type)
wilcox.test(skin~type)
#p degerleri hep anlamli her iki test icin
#seker hastasi olanlarda bmi daha yuksek oldugu hipotezini test edelim
t.test(bmi~type,alternative="less")
#type degiskeni No,Yes seklinde. No=0 Yes=1 olarak aliniyor. 
#Dolayisiyla ilk olarak No ortalamasi, sonra Yes ortalamasi dusunuluyor.
#Yani No-Yes ortalamasi<0 alternatif hipotezimiz oldugu icin less yazdik.
#No-Yes<0 => No<Yes demektir. Yani seker hastalarindaki ortalama hasta olmayanlardan yuksek oldugu hipotezi
# Sonuc:Seker hastasi olanlarda bmi degeri olmayanlardan fazladir p=5.941e-06 Welch testi sonucu
#Normal t testi de yapilabilirdi, varyanslarin homojenligini test etmedigim icin bunu yazmakla yetindim.


#Soru2
data(iris)
#once setosa ve virginica yi secelim
attach(iris)
yeniveri<-iris[Species!="versicolor",]
with(yeniveri,t.test(Sepal.Length~Species,alternative="less"))
#the reasoning is the same as above
#Setosa-virginica farki 0'dan kucuktur ve bu istatistiksel olarak anlamlidir
#p=2.2e-16

#Soru3
data(anorexia,package = "MASS")
attach(anorexia)
yeniveri<-anorexia[Treat!="Cont",]
attach(yeniveri)
tapply(Postwt, Treat, mean)
tapply(Prewt, Treat, mean)
t.test(Postwt,Prewt,paired = TRUE,alternative = "greater")
#Once Postwt i formule yazdigimiz icin Postwt-Prewt degerine bakiyor.
#Bu degerin 0'dan buyuk olmasini test etmek icin greater yazmaliyiz.
#Bu test 0'dan buyukse Postwt Prewt den buyuk demektir.
# Postwt prewt den buyuktur ve istatistiksel olarak anlamlidir p=7.063e-05

#Yukaridaki cevap aslinda benim sormak istedigim sorunun cevabiydi. O soru sudur:
#CBT ve FT gruplarindaki kisiler goz onune alindiginda Post degerler pre degerlerden buyuk mudur?
#Yani bu kli tedavi ise yaramis midir?

#Benim yanlis ifade ederek sordugum soru ise: Hangi grupta kilo alimi daha fazla olmustur? seklinde oldugu icin 
#normalde Twoway ANOVA yapmak gereklidir. Biz burada t testi ile bu soruya cozum bulacagiz.
#T testi yapabilmek icin Post degerlerden pre degerleri cikartalim, ve bu yeni degerlere
#t testi yapalim. 
yeniveri$Fark<-Postwt-Prewt     # Post-Pre farklari bulunuyor
fligner.test(yeniveri$Fark~Treat)   # Varyanslarin homojen oldugu bulunuyor
t.test(yeniveri$Fark~Treat,var.equal=TRUE,alternative="less")    # Bagimsiz gruplarda T testi yapiliyor
#CBT-FT farkinin <0 oldugu bulunuyor p=0.03. Bu demektir ki FT deki artislar CBT den daha fazla.
#Dolayisyla FT daha iyi bir tedavi yontemidir diyebiliriz.
#Zaten ortalamalara da bakildiginda FT deki artislarin CBT den fazla oldugu goruluyor.
