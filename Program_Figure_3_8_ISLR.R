library(ISLR)
library(MASS)
library(ggplot2)
#Auto<-read.csv(file="K:/INTRODUCCIÓN A LA ANALÍTICA/02-2019/Credit.csv",
#                 header=T,sep=',',dec='.')
Auto=ISLR::Auto
mpg<-Auto$mpg
horsepower<-Auto$horsepower
fit.lm<-lm(mpg~horsepower,data=Auto)
fit.lm2<-lm(mpg~poly(horsepower,2),data=Auto)
fit.lm5<-lm(mpg~poly(horsepower,5),data=Auto)
d<-seq(40,300,length.out=length(horsepower))
plot(horsepower,mpg,ylim=c(10,50),xlim=c(45,230),col="grey",main='Non linear trend between mpg and horsepower. Auto dataset ISLR')
lines(d,predict(fit.lm,data.frame(horsepower=d)),col="orange",lwd=2)
lines(d,predict(fit.lm2,data.frame(horsepower=d)),col="blue",lwd=2)
lines(d,predict(fit.lm5,data.frame(horsepower=d)),col="green",lwd=2)
legend("topright",c("Linear","Degree 2","Degree 5"),col=c("orange","blue","green"),lwd=2,cex=0.7)
