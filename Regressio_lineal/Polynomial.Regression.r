Polynomial.Regression<-function(x,y,k){
	n<-length(x)
	K<-1:k
	X<-outer(x,K,"^")
	ones<-rep(1,n)
	X1<-cbind(ones,X)
	return(list(X=X,X1=X1))
}