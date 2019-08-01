#Obtaining figure 3.4.  It requires the csv file that can be also found in this repository
#https://github.com/jcsalaza69/R-code-for-figures-learning-statistical-learning

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")

#Income3D=read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/02-2019/Income2.csv",header=T,sep=',',dec='.')
Income3D=read.csv(file="Income2.csv",header=T,sep=',',dec='.')
head(Income3D)
x=Income3D$Education
y=Income3D$Seniority
z=Income3D$Income

# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
#scatter plot with regression plane with many colors
graf1<-scatter3D(x, y, z, pch = 19, cex = 1,
theta = 30, phi = 30, ticktype = "detailed",
xlab = "Education", ylab = "Seniority", zlab = "Income",
surf = list(x = x.pred, y = y.pred, z = z.pred,
            facets = NA, fit = fitpoints), main = "Income Dataset ISLR")

#scatter plot with regression plane with few colors
graf2<-scatter3D(x, y, z, pch = 19, cex = 1,col='red',
          theta = 20, phi = 17, scale=TRUE, expand=0.3,
          xlab = "Education", ylab = "Seniority", zlab = "Income",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col='#006633',
                      facets = NA, fit = fitpoints), main = "Income Dataset ISLR")

par(mfrow=c(1,2))
graf1
graf2
