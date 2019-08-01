#Obtaining figure 2.2. It requires the csv file that can be also found in this repository
#https://github.com/jcsalaza69/R-code-for-figures-learning-statistical-learning

library(ISLR)
library(ggplot2)
library(MASS)
Income=read.csv(file="Income1.csv",header=T,sep=',',dec='.')
YofEdu=Income$Education
Income_=Income$Income
plot(YofEdu,Income_,pch=19,col='red',xlab='years of Education', ylab='Income')
ggplot(Income, aes(YofEdu,Income_,color='red')) + 
  geom_point(color='red') +
  labs(x='years of Education', y='Income')

ggplot(Income, aes(YofEdu,Income_,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "loess", se = FALSE, colour="purple") +
  labs(x='years of Education', y='Income')

#With joining line segments

mod <- loess(Income ~ YofEdu, data = Income)
Income <- transform(Income, Fitted = fitted(mod))

ggplot(Income, aes(YofEdu,Income_,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "loess", se = FALSE, colour="purple") +
  labs(x='years of Education', y='Income')+
  geom_segment(aes(x = YofEdu, y = Income,
                   xend = YofEdu, yend = Fitted))

#Joining line segments using base plot
plot(Income ~ YofEdu, data = Income, type = "p", col = "red",
     cex = 1.25,xlab='Years of Education')
points(Fitted ~ YofEdu, data = Income)
lines(Fitted ~ YofEdu, data = Income, col = "blue")
with(Income, segments(YofEdu, Income, YofEdu, Fitted))

plot(Income ~ YofEdu, data = Income, type = "p", col = "red",pch=19,
     cex = 1,xlab='Years of Education')
points(Fitted ~ YofEdu, data = Income,pch=19,
       cex = 0.5)
lines(Fitted ~ YofEdu, data = Income, col = "blue")
with(Income, segments(YofEdu, Income, YofEdu, Fitted))
