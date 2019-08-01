library(ISLR)
library(MASS)
Auto=ISLR::Auto
mpg<-Auto$mpg
horsepower<-Auto$horsepower
fit.lm<-lm(mpg~horsepower,data=Auto)
fit.lm2<-lm(mpg~poly(horsepower,2),data=Auto)
residu1<-(summary(fit.lm))$residuals
residu2<-(summary(fit.lm2))$residuals
fitted1<-fit.lm$fitted.values
fitted2<-fit.lm2$fitted.values
fit.loess1<-loess(residu1~fitted1,span=0.9)
d1<-seq(0,35,length.out=length(fitted1))
fit.loess2<-loess(residu2~fitted2,span=1.8)
d2<-seq(10,40,length.out=length(fitted2))

par(mfrow=c(1,2))
plot(fitted1,residu1,col="grey",main='Residual plot for linear fit.\n Auto dataset ISLR',ylab="Residuals",xlab="Fitted values")
lines(d1,predict(fit.loess1,data.frame(fitted1=d1)),col="orange",lwd=2)
abline(h=0,col="purple")

plot(fitted2,residu2,col="grey",main='Residual plot for quadratic fit.\n Auto dataset ISLR',ylab="Residuals",xlab="Fitted values")
lines(d2,predict(fit.loess2,data.frame(fitted2=d2)),col="orange",lwd=2)
abline(h=0,col="purple")
