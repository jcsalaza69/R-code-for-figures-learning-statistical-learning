#PCA NCI60 data. Obtaining figure 1.4. It requires a simulated csv file that can be also found
#in this repository https://github.com/jcsalaza69/R-code-for-figures-learning-statistical-learning

library(ISLR)
library(ggplot2)
library(MASS)

#Function for colors of classes
par(mfrow=c(1,2))
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

figure1_4_left2<-read.csv(file="COPY1_OF_figure1_4_islr.csv",header=T,sep=',',dec='.')

#Obtaining figure 1.4 left panel
plot(figure1_4_left2[,1:2],col=Cols(figure1_4_left2[,3]), pch=19, xlab="Z1", ylab="Z2",main='NCI60 in R2')

#Obtaining figure 1.4 right panel
nci.data=NCI60$data
nci.labs=NCI60$labs
dim(nci.data)
pr.out=prcomp(nci.data, scale=TRUE)
names(pr.out)
dim(pr.out$x)
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2")
