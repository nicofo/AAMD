source("D:\\AAMD\\Practica.02\\Code\\UScensus.r")
plot(t,p)
#
#	Regressió lineal
#
UScensus.1<-lm(p~t)
UScensus.1$coefficients[1]->a
UScensus.1$coefficients[2]->b
abline(a,b)
#
#	Regressió polinòmica de grau 2
#
t2<-t^2
UScensus.2<-lm(p~t+t2)
T<-seq(1900,2000,length=300)
T2<-T^2
UScensus.2$coefficients[1]->a
UScensus.2$coefficients[2]->b1
UScensus.2$coefficients[3]->b2
lines(T,a+b1*T+b2*T2,col="blue")