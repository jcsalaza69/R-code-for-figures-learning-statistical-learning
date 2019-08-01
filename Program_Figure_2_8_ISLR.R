#Obtaining plots like Figure 2.8. Simulated data.

#Left Panel
m1<-mvrnorm(n = 50, mu=c(2.5,3), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
head(m1)
X1=m1[,1]
X2=m1[,2]
m1=cbind(X1,X2)

m2<-mvrnorm(n = 50, mu=c(2.5,9), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
head(m2)
X1=m2[,1]
X2=m2[,2]
m2=cbind(X1,X2)

m3<-mvrnorm(n = 50, mu=c(9,6), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
head(m3)
X1=m3[,1]
X2=m3[,2]
m3=cbind(X1,X2)

#First way
plot(m1[,1], m1[,2], , col="darkorange", pch=1, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12),main='Left Panel Figure 2.8') 
points(m2[,1], m2[,2], , col="dodgerblue", pch=2, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12)) 
points(m3[,1], m3[,2], , col="darkgreen", pch=3, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12)) 

#Second way using former function from Chapter 1 ISLR

group=c(rep(1,50),rep(2,50),rep(3,50))
length(group)
data<-rbind(m1,m2,m3)
data1<-cbind(data,group)

Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

figure2_8_left=data1
plot(figure2_8_left[,1:2],col=Cols(figure2_8_left[,3]), pch=19, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12),main='Left Panel Figure 2.8')


#Rigth Panel
m1<-mvrnorm(n = 50, mu=c(2.5,3), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m1[,1]
X2=m1[,2]
m1=cbind(X1,X2)

m2<-mvrnorm(n = 50, mu=c(2.5,6), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m2[,1]
X2=m2[,2]
m2=cbind(X1,X2)

m3<-mvrnorm(n = 50, mu=c(6,4), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m3[,1]
X2=m3[,2]
m3=cbind(X1,X2)

#First way
plot(m1[,1], m1[,2], , col="darkorange", pch=1, xlab="X1", ylab="X2", ylim=c(0,10),xlim=c(0,8),main='Righ Panel Figure 2.8') 
points(m2[,1], m2[,2], , col="dodgerblue", pch=2, xlab="X1", ylab="X2", ylim=c(0,10),xlim=c(0,8)) 
points(m3[,1], m3[,2], , col="darkgreen", pch=3, xlab="X1", ylab="X2", ylim=c(0,10),xlim=c(0,8)) 

#Second way using former function from Chapter 1 ISLR

group=c(rep(1,50),rep(2,50),rep(3,50))
data<-rbind(m1,m2,m3)
data1<-cbind(data,group)

Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

figure2_8_left=data1
plot(figure2_8_left[,1:2],col=Cols(figure2_8_left[,3]), pch=19, xlab="X1", ylab="X2",ylim=c(0,10),xlim=c(0,8),main='Righ Panel Figure 2.8')

