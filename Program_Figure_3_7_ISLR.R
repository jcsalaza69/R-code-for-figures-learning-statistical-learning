library(ISLR)
library(MASS)
library(ggplot2)
# library(dummies)
Credit<-read.csv(file="K:/INTRODUCCIÓN A LA ANALÍTICA/02-2019/Credit.csv",
                 header=T,sep=',',dec='.')
Balance=Credit[,13]
Income=Credit[,3]
Student=Credit[,10]
fitted1=lm(Balance~Income+Student)$fitted.values
fitted2=lm(Balance~Income+Student+Income*Student)
newdata1=data.frame(fitted1,Income,Student)
ggplot(newdata1, aes(x=Income, y=fitted1))+
  labs(title="Prediction of balance from Income and student. Model1",
       x="Income", y = "Balance")+
  geom_line(aes(group = Student, color = Student), size = 1) 

fitted2=lm(Balance~Income+Student+Income*Student)$fitted
newdata2=data.frame(fitted2,Income,Student)
ggplot(newdata2, aes(x=Income, y=fitted2))+
  labs(title="Prediction of balance from Income and student with interaction. Model2",
       x="Income", y = "Balance")+
  geom_line(aes(group = Student, color = Student), size = 1) 

# To put them side-by-side

library(cowplot)

Model1<-ggplot(newdata1, aes(x=Income, y=fitted1))+
  labs(title="Prediction of balance from Income and student.\n Model1",
       x="Income", y = "Balance")+
  geom_line(aes(group = Student, color = Student), size = 1) 

Model2<-ggplot(newdata2, aes(x=Income, y=fitted2))+
  labs(title="Prediction of balance from Income and student \n with interaction. Model2",
       x="Income", y = "Balance")+
  geom_line(aes(group = Student, color = Student), size = 1) 

plot_grid(Model1, Model2, labels = "AUTO")
