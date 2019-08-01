#Obtaining figure 2.1. It requires the csv file that can be also found in this repository
#https://github.com/jcsalaza69/R-code-for-figures-learning-statistical-learning

library(ISLR)
library(ggplot2)
library(MASS)
Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
head(Advertising)
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
par(mfrow=c(1,3))
plot(Tv,Sales,pch=19,col='red')
plot(Radio,Sales,pch=19,col='red')
plot(Radio,Sales,pch=19,col='red')
dev.off()
plot(Newspaper,Sales,pch=19,col='red')
ggplot(Advertising, aes(Tv,Sales,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, colour="purple")
ggplot(Advertising, aes(Radio,Sales,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, colour="purple")
ggplot(Advertising, aes(Newspaper,Sales,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, colour="purple")

