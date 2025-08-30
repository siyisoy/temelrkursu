##Soru1
data(mtcars)
str(mtcars)
typeof(mtcars)
##Soru2
scale(mtcars[,3:7])           ## [,3:7] 3.ve 7. sutunlar arasini secer
##Soru3
silindir<-factor(mtcars$cyl,labels=c("4 silindirli","6  silindirli","8 silindirli"))
##Soru4
ceo<-read.table(file="ceo.csv",header=TRUE,sep=",") 
volcano<-read.table(file="volcano.csv",header = TRUE,sep = "_")  ## volcano _ ile ayrilmis
colMeans(ceo[,-8])
sd(ceo$salary)
##Soru5
a<-rnorm(250,50,7)     ## 50 ortalamali 7 std sapmali normal dagilimdan 250 adet sayi ceker
b1<-a-mean(a)  # ya da
b2<-scale(a,center = TRUE, scale = FALSE)  ## Sadece centering istedigimiz icin center=TRUE scale=FALSE
hist(a)
hist(b1)
##Soru7
m<-mean(a)
std<-sd(a)
s1<-length(a[a<m+1*std & a>m-1*std])    ## ort+1*sd dan kucuk ve ort+1*sd dan buyuk olan a lari secer
s2<-length(a[a<m+2*std & a>m-2*std])
s3<-length(a[a<m+3*std & a>m-3*std])
(s1/250)*100   ## 65'e yakin olmali
(s2/250)*100   ## 95'e yakin olmali
(s3/250)*100   ## 99,7'ye yakin olmali

