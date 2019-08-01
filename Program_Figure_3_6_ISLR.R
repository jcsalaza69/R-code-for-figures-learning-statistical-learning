library(ISLR)
library(MASS)
Credit=ISLR::Credit
head(Credit)
Pred_Cuanti<-Credit[,c(12,6,5,7,2,3,4)]
pairs(Pred_Cuanti,pch=1,cex=0.1,col="blue")
