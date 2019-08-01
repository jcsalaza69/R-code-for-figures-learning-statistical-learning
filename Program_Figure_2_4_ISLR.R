#Figure 2.4. Fitting a plane. It requires the csv file that can be also found in this repository
#https://github.com/jcsalaza69/R-code-for-figures-learning-statistical-learning

library(ISLR)
library(ggplot2)
library(MASS)
require(plotly)
library(plotly)

Income3D=read.csv(file="Income2.csv",header=T,sep=',',dec='.')
head(Income3D)
x=Income3D$Education
y=Income3D$Seniority
z=Income3D$Income

#Fitting the plane

fit.lm<-lm(z~x+y)
summary(fit.lm)
xnew <- seq(min(x), max(x), len=30)
ynew <- seq(min(y), max(y), len=30)
df <- expand.grid(x = xnew, y = ynew)
dim(df)
f1<-function(x,y){predict(fit.lm,newdata=df)}
z2<-outer(xnew,ynew,f1)
persp(xnew,ynew,z2,phi=25,theta=40,shade=0.2,scale=TRUE,col="yellow", expand=0.4,
      box = TRUE, xlab="Years of Education",ylab="Seniority",zlab="Income")
mypoints <- trans3d(x, y, z, pmat=res); points(mypoints, pch=19, col="red")

# Yet another way using plotly

aX <- list(title = "Years of Education")
aY <- list(title = "Seniority")
aZ <- list(title = "Income")
df3=data.frame(x,y,z)
head(df3)
df3=t(df3)
z2=t(z2)
plot_ly() %>% add_surface(x=xnew,y=ynew,z=z2)%>%
  layout(scene = list(xaxis = aX, yaxis = aY, zaxis=aZ,dragmode="turntable"),autosize=1.9)%>% 
  add_trace(data = df3, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))

plot_ly(x=xnew,y=ynew,z=z2,type = "surface",colors=colors) %>%
  layout(scene = list(xaxis = aX, yaxis = aY,  zaxis=aZ, dragmode="turntable"))%>% 
  add_trace(data = df3, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))  

