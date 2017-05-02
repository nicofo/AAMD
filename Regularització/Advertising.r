#require(ISLR)

require(pracma)

Advertising<-read.csv("Advertising.csv",row.names=1)
plot(Advertising,pch=20,col=c("blue"))


Advertising.L2<-lm(Sales~.,data=Advertising)


A<-model.matrix(Advertising.L2)
b<-as.matrix(Advertising[,4])


Advertising.L1<-L1linreg(A,b,maxiter=800)

Advertising.L1$x
# (Intercept)           TV        Radio    Newspaper 
# 3.411906669  0.043455708  0.197292785 -0.003212031 

Advertising.L2$coeff
# (Intercept)           TV        Radio    Newspaper 
# 2.938889369  0.045764645  0.188530017 -0.001037493 

