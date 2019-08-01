#Obtaining figure 2.3. It requires the csv file that can be also found in this repository
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
dev.off()

xyz.fit <- loess(z~x+y+x*y,span=0.5,degree=2); d1<-0.5; d2<-10
f1<-function(x,y){predict(xyz.fit,cbind(x,y))};
x1<-sort(seq(min(x),max(x),d1)); y1<-sort(seq(min(y),max(y),d2)); z1<-outer(x1,y1,f1)
res<-persp(x=x1,y=y1, z=z1,phi=25,theta=40,shade=0.2,scale=TRUE,col="lightblue", expand=0.4,
           box = TRUE, xlab="years of Education",ylab="Seniority",zlab="Income")
mypoints <- trans3d(x, y, z, pmat=res); points(mypoints, pch=19, col="red")

dev.off()
#Another way
fit.loess<-loess(z~x+y+x*y,span=0.5,degree=2)
xnew <- seq(min(x), max(x), len=30)
ynew <- seq(min(y), max(y), len=30)
df <- expand.grid(x = xnew, y = ynew)
dim(df)
f1<-function(x,y){predict(fit.loess,newdata=df)}
z1<-outer(xnew,ynew,f1)
persp(xnew,ynew,z1,phi=25,theta=40,shade=0.2,scale=TRUE,col="lightblue", expand=0.4,
      box = TRUE, xlab="years of Education",ylab="Seniority",zlab="Income")
mypoints <- trans3d(x, y, z, pmat=res); points(mypoints, pch=19, col="red")

# Yet another way using plotly. Interactive plot

aX <- list(title = "years of Education")
aY <- list(title = "Seniority")
aZ <- list(title = "Income")
df2=data.frame(x,y,z)
df2=t(df2)
z1=t(z1)
plot_ly() %>% add_surface(x=xnew,y=ynew,z=z1)%>%
  layout(scene = list(xaxis = aX, yaxis = aY, zaxis=aZ,dragmode="turntable"),autosize=0.9)%>% 
  add_trace(data = df2, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))

plot_ly(x=xnew,y=ynew,z=z1,type = "surface",colors=colors) %>%
  layout(scene = list(xaxis = aX, yaxis = aY,  zaxis=aZ, dragmode="turntable"))%>% 
  add_trace(data = df2, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))  

