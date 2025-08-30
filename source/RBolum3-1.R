#######  R ile Temel Istatistik Bolum 3  ###########

### 2way ANOVA   ####################
warpbreaks$id<-1:54
eza<-ezANOVA(warpbreaks,dv=breaks,between = .(wool,tension),wid=id,type = "3",detailed = TRUE,return_aov = TRUE)
print(eza)

####### Linear Regression   ################

library(car)
data(Prestige)
attach(Prestige)
pairs(Prestige)                                  # Scatter plot matrisi ciziliyor
scatterplot.matrix(Prestige)
fit<-lm(prestige~education+log(income)+women)    # Regresyon modeli
summary(fit)                                     # Parameter estimates
confint(fit,level = .95)                         # Confidence intervals


######  Formula   ###########################

Y~A           Y=B0+B1*A                    # Sadece A ana etkisi modelde
Y~-1+A        Y=B1*A                       # A etkisinin oldugu sabitin olmadigi (no intercept) modeli
Y~A+B         Y=B0+B1*A+B2*B               # A ve B ana etkileri modelde
Y~A:B         Y=B0+B1*A*B                  # A ve B etkilesim (Interaction) etkisi modelde
Y~A*B         Y=B0+B1*A+B2*B+B3*A*B        # A ve B nin hem ana etkileri hem de etkilesim etkileri modelde
Y~A+I(A^2)    Y=B0+B1*A+B2*A^2             # A ve A^2 etkileri modelde


######  Prediction   ###########################

education1<-c(14,15,16)               # Yeni veri olusturuluyor
income1<-c(10000,11000,12000)
women1<-c(15,20,25)
newdata <-data.frame(education=education1,income=income1,women=women1) 
predict(fit,newdata,interval = "prediction")    # Bu yeni veri eldeki model kullanilarak tahmin ediliyor


##### Categorical Regressors   ####################

contrasts(type)
contrasts(type)<-contr.treatment(3,base=2)              # treatment contrast inda refereans kategori 2. kategori yapiliyor
fit2<-lm(prestige~education+log(income)+type)
Anova(fit2)
summary(fit2)
fit3<-lm(prestige~education+log(income)+C(type,sum))     # type icin sum contrast i kullaniliyor
fit4<-lm(prestige~education+log(income)+C(type,helmert))
summary(fit3)
summary(fit4)

#####  Interactions    ############################

fit6<-lm(prestige~education+type*income)                 # type ve log(income) icin hem ana etkiler hem de etkilesim olan model
fit66<-lm(prestige~education+type+log(income)+type:log(income))  # uzun yazim sekli
scatterplot(log(income),prestige,groups = type,smoother = F)
by(Prestige,type,function(x){cor(log(x$income),x$prestige)})

######   Diagnostics  ###############################

scatterplot(prestige,income)
plot(fit)                                  # Regression diagnostic plots, linearity & homoscedasticity (duz cizgi etrafina yayilmis), artiklarin normalligi, homoscedasticity (duz cizgi etrafina yayilmis), residualxleverage etkili gozlemler 
vif(fit)                                   # VIF degerleri
influence.measures(fit)                    # Regresyon etki olculeri   
influencePlot(fit)                         # Regresyon etki grafikleri
influenceIndexPlot(fit,id.n = 2)
qqPlot(fit)                                # Artiklarin normalligi icin QQ grafigi
ncvTest(fit)                               # Homoscedasticity testi
outlierTest(fit)                           # Student artiklarinda outlier olup olmadigi
crPlots(fit)                               # Component plus residual plots, partial residual plots
avPlots(fit)                               # Added variable plots, partial regression plots
durbinWatsonTest(fit)                      # Durbin Watson testi
residualPlots(fit)                         # Artik grafikleri
leveragePlots(fit)                         # Leverage grafikleri


##### Model Comparison  ##################################

fit7<-lm(prestige~education+log(income))  # women fit modelinden cikariliyor
anova(fit7,fit)
fit8<-update(fit,.~.-women)               # women fit modelinden cikariliyor
anova(fit8,fit)                           # Nested modeller karsilastiriliyor
AIC(fit8,fit6)                            # Iki model karsilastiriliyor

##### Stepwise Regression (Not recommended!)  ##################################

backward<-lm(prestige~.-census,data=Prestige)                # backward regression
step(backward,direction="backward")
forward<-lm(prestige~1)                                      # forward regression
step(forward,direction="forward",scope=~education+log(income)+women)
step(forward,direction="both",scope=~education+income+women)
library(MASS)
addterm(forward,test="F",scope = ~education+log(income)+women)
dropterm(backward,test="F",scope = ~education+income+women)

