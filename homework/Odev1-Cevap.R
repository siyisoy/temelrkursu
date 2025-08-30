#Soru1
a<-c(1,2,3,4,5,6,7,8)
b<-c(8,7,6,5,4,3,2,1)
c<-a[1:3]
d<-b[6:8]
skalercarpim<-sum(c*d)
#Soru2
euclid<-sqrt(sum((a-b)^2))
#Soru3
yas<-c(27,28,30,32,25,33,29,19,26,27,34,38,41,29,28,32,30,24,23,22,21,29,32,42,35,36,32,43,44,46,25,27,32,53,56,62,22,21,24,22)
sd1<-sqrt(sum((yas-mean(yas))^2)/(length(yas)-1))
sd2<-sd(yas)
#sd1 ve sd2 ayni
#Soru4
medyan<-median(yas)
kimbuyuk1<-yas>medyan
ortalama<-mean(yas)
kimbuyuk2<-yas>ortalama
which(kimbuyuk1==TRUE)
table(kimbuyuk1)
table(kimbuyuk2)

#Soru5
m<-c(3,4,2,2,2,4,6,8,6,-2,2,4,2,6,4,4,8,2,4,2)
A<-matrix(m,nrow=5,byrow = TRUE)
