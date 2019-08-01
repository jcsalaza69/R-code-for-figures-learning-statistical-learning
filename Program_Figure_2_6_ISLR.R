#Obtaining Figure 2.6. Fitting a wiggly loess. It requires the csv file that can be also found in this repository
#https://github.com/jcsalaza69/R-code-for-figures-learning-statistical-learning

library(ISLR)
library(ggplot2)
library(MASS)

Income3D=read.csv(file="Income2.csv",header=T,sep=',',dec='.')
head(Income3D)
x=Income3D$Education
y=Income3D$Seniority
z=Income3D$Income
fit.loess<-loess(z~x+y+x*y,span=0.3,degree=2)
xnew <- seq(min(x), max(x), len=50)
ynew <- seq(min(y), max(y), len=50)
df <- expand.grid(x = xnew, y = ynew)
dim(df)
f1<-function(x,y){predict(fit.loess,newdata=df)}
z1<-outer(xnew,ynew,f1)
persp(xnew,ynew,z1,phi=25,theta=40,shade=0.2,scale=TRUE,col="yellow", expand=0.4,
      box = TRUE, xlab="Years of Education",ylab="Seniority",zlab="Income")
mypoints <- trans3d(x, y, z, pmat=res); points(mypoints, pch=19, col="red")
