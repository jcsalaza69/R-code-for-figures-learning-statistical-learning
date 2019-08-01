#Obtaining Figure 1.1 in several different ways

library(ISLR)
library(ggplot2)
library(MASS)

#Loading Wage dataset
Wage=ISLR::Wage
dim(Wage)
names(Wage)
Age=Wage$age
Wage_=Wage$wage

par(mfrow=c(1,2))
plot(Age,Wage_,col='gray',ylim=c(50,300),ylab='Wage')
smooth<-loess(Wage_ ~ Age, data=Wage) 
j <- order(Age)
lines(Age[j],smooth$fitted[j],col="red",lwd=3)
scatter.smooth(Wage_ ~ Age, span = 2/4, degree = 2,col='gray',lwd=3) #Another way

ggplot(Wage, aes(Age,Wage_,color=cyl)) + 
  geom_point(color='green') +
  geom_smooth(method = "loess", se = FALSE, colour="purple")#Yet another way
Year=Wage$year
plot(Year,Wage_,col='gray',ylim=c(50,300),ylab='Wage')
abline(lm(Wage_ ~ Year,data=Wage),col='blue',lwd=3)
Education=Wage$education
boxplot(Wage_~Education,data=Wage, main="Boxplot of Education vs Wage",
        xlab="Education level", ylim=c(50,300),ylab="Wage",col=c(2,3,4,5,6))
