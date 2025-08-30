#######  R ile Temel Istatistik Bolum 1-Kisim 1  ###########
#######  Atama ve Veri Tipleri ################

a<-10              # <- isareti ile bir degiskene deger atanir
b=9                # =  isareti ile bir degiskene deger atanir
c<-a^3+sqrt(b)     # ^ islemi us alma islemidir
d<-c(2,3,4,5)      # c() vektor ya da liste olusturmada kullanilir
d[1]               # vektor veya matrislerin elemanlarini gostermek icin [] kullanilir
e<-"Merhaba R!"    # "" ile string degerler belirtilir
typeof(d)          # typeof() bir objenin tipini gosterir
str(d)             #  str() bir objenin yapisini gosterir

f<-TRUE            #  TRUE, FALSE mantiksal dogru ve yanlisi gosterir
g<-1:10            # a:b a ile b arasinda duzenli sayi dizileri vektorunu verir
h<-NULL            # NULL objesini gosterir, is.null() ile bir degerin null olup olmadigi anlasilir
i<-a<b

#######  Vektorler   ################

a<-c(1,2,7,11,15)   # Vektorler farkli sayida obje iceren sirali dizilerdir
b<-1:5
d<-c(4,3,2,1)
a[1]                 # Vektorun 1. elemani
a[-1]                # Vektorun 1. eleman disindaki elemanlari
a[c(2,4)]            # Vektorun 2. ve 4. elemanlari
a[1:3]               # Vektorun 1. ile 3. elemani arasindaki elemanlari
e<-2*a               # a vektorunun tum elemanlarinin 2 katlarindan olusan yeni vektor
g<-a^2+3*a+1
h<-a*b               # a ile b vektorunun karsilikli elemanlarinin carpimindan olusan vektor
i<-a+d               # a ile b vektorunun karsilikli elemanlarinin toplamindan olusan vektor
mean(a)              # mean() vektordeki degerlerin ortalamasini verir. Benzer komutlar: sd,median,min,max,sum,sort
j<-a>2               # a vektorunun 2 den buyuk elemanlarini gosteren *mantiksal* vektor
k<-a^2==4            


#######  Matrisler   ################

a<-1:12
b<-matrix(a)              # a vektorundeki elemanlardan olusan matris
c<-matrix(a,nrow=3)       # a vektorundeki elemanlarin 3 satir olarak dizilmesinden olusan matris
d<-matrix(a,nrow=3,byrow=TRUE)   
e<-2*d+3
f<-solve(d)               # solve() bir matrisin ters matrisini bulur
d[1,2]                    # d matrisinin 1.satir ve 2.sutunundaki hucre degeri
d[1:2,2]                  # d matrisinin 1 ve 2.satiri ile 2.sutunundaki degerler
d[1,2:3]                  # d matrisinin 1.satiri ile 2. ve 3.sutunundaki degerler
d[,1]                     # d matrisinin 1.sutunundaki degerler
d[,1:2]
d[1:2,]
d[-1,]                    # d matrisinden 1.satir cikarildiginda elde edilen alt matris
d[-c(1,2),]
a<-matrix(1:12,nrow=3)
b<-matrix(1:12,nrow=4)
c<-a%*%b                  # %*% islemi ile iki matris carpilir
d<-t(c)                   # t() islemi bir matrisin transpozunu alir
eigen(c)                  # eigen() islemi bir matrisin ozdeger ve ozvektorlerini bulur
e<-c(1,2,3,4)
f<-c(2,4,6,8)
g<-rbind(e,f)             # rbind() ile iki vektor satir olarak birlestirilerek bir matris elde edilir
h<-cbind(e,f)             # cbind() ile iki vektor sutun olarak birlestirilerek bir matris elde edilir
is.matrix(g)

#######  Listeler   ################

a<-c(1,2,3,4)
b<-matrix(a,nrow=2)
c<-c(TRUE,TRUE,FALSE,FALSE)
d<-list(a,b,c,12,"Ilk Listem")     # list() ile liste uretilir
d[[1]]                             # bir liste icerisindeki elemanlara ulasmak icin [[]] kullanilir
d[[2]][2,]
e<-list(vektor=a,matris=b,mantiksal=c)
e$vektor
e[["vektor"]]                      # liste icerisinde vektor isimli bir vektore ulasiyoruz 
e[["vektor"]][1]
is.matrix(e$matris)

#######  Faktorler   ################

gender<-c(rep("male",20), rep("female", 30)) # rep() komutu ile bir deger cogaltilir
str(gender)
gender<-factor(gender)                       # factor() ile bir vektor faktor tipine getirilir, factorial design, bolunmus parseller
str(gender)
ses <- c("low", "middle", "low", "low", "low", "low", "middle", "low", "middle","middle", "middle", "middle", "middle", "high", "high", "low", "middle","middle", "low", "high")
is.factor(ses)
is.character(ses)
ses.f<-factor(ses, levels = c("low", "middle", "high"))
is.factor(ses.f)
levels(ses.f)                                # levels() komutu ile bir faktorun seviyeleri gosterilir
ses.f2<-factor(ses)
levels(ses.f2)
table(ses.f2)

#######  Dataframe   ################

gender<-c("Male","Female")
height<-c(182,165)
weight<-c(75,65)
age<-c(27,25)
a<-data.frame(gender,height,weight,age)       # data.frame() komutu ile dataframe uretilir
str(a)
rownames(a)<-c("Ali","Ayse")                  # rownames() komutu ile dataframedeki satirlara isim verilir
a$gender
a$bloodtype<-c("ARh+","ARh-")                 # dataframe e yeni bir sutun eklemek icin sutun ismi ile atama yapilmalidir
dim(a)                                        # dim() bir objenin boyutlarini verir
a$height<-NULL                                # dataframeden bir sutunu silmek icin kullanilir
a["Ali","weight"]<-90                         # dataframedeki verileri guncellemenin bir yolu



