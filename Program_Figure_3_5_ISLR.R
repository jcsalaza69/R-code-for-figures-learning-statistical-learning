#Obtaining figure 3.5. It requires the csv file that can be also found in this repository
#https://github.com/jcsalaza69/R-code-for-figures-learning-statistical-learning

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
#Advertising<-read.csv(file="K:/INTRODUCCIÓN A LA ANALÍTICA/02-2019/Advertising.csv",header=T,sep=',',dec='.')
Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
z=Advertising$sales
y=Advertising$TV
x=Advertising$radio

#Fitting the linear regression model (z = ax + by + d)
fit <- lm(z ~ x + y)
# Predict values on regular xy grid
grid.lines = 50
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
#scatter plot with regression plane with few colors
scatter3D(x, y, z, pch = 19, cex = 1,col=c('red'),
          theta = 25, phi = 0, scale=TRUE, expand=0.6,
          xlab = "Radio", ylab = "Tv", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col='#006633',
                      facets = NA, fit = fitpoints), main = "Advertising Dataset ISLR. Fig 3.5")
