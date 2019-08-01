#Obtaining Figure 1.2
library(ISLR)
library(ggplot2)
library(MASS)
#Loading Smarket dataset
Smarket=ISLR::Smarket
dim(Smarket)
names(Smarket)
Pchange_Yesterday=Smarket$Lag1
Pchange_Twodays=Smarket$Lag2
Pchange_Threedays=Smarket$Lag3
Direction=Smarket$Direction
Year=Smarket$Year
boxplot(Pchange_Yesterday~Direction,data=Smarket, main="Yesterday. S&P500",
        xlab="Today?s direction", ylim=c(-4,6),ylab="Percentage change in S&P",col=c(4,2))
boxplot(Pchange_Twodays~Direction,data=Smarket, main="Two days ago. S&P500",
        xlab="Today?s direction", ylim=c(-4,6),ylab="Percentage change in S&P",col=c(4,2))
boxplot(Pchange_Threedays~Direction,data=Smarket, main="Three days ago. S&P500",
        xlab="Today?s direction", ylim=c(-4,6),ylab="Percentage change in S&P",col=c(4,2))
