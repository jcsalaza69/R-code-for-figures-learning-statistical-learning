#Obtaining Figure 1.3
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
#Discriminant analysis
train=(Year<2005)
length(train)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
length(Direction.2005)
#Linear discriminant
lda.fit=lda(Direction~Pchange_Yesterday+Pchange_Twodays,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Smarket.2005)$class
length(lda.pred)
table(lda.pred[999:1250],Direction.2005)
#Quadratic discriminant. Obtaining figure 1.3
qda.fit=qda(Direction~Pchange_Yesterday+Pchange_Twodays,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)
names(qda.class)
qda.class=predict(qda.fit,Smarket.2005)$class
length(qda.class)
table(qda.class[999:1250],Direction.2005)
summary(qda.class)
qda.classif=qda.class[999:1250]
qda.prob=predict(qda.fit,Smarket.2005)$posterior
qda.posterior=qda.prob[999:1250]
par(mfrow=c(1,1))
boxplot(qda.posterior~Direction.2005,col=c(5,2),ylab='Posterior probability',xlab='Todays direction')
