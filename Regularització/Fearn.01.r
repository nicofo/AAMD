xy.a <- read.table("Fearn.data.a.txt", header=TRUE)
n.a<-nrow(xy.a)
x.a<-as.matrix(xy.a[,-1])
y.a<-as.matrix(xy.a[,1])
X.a<-cbind(rep(1,n.a),x.a)
xy.b <- read.table("Fearn.data.b.txt", header=TRUE)
n.b<-nrow(xy.b)
x.b<-as.matrix(xy.b[,-1])
y.b<-as.matrix(xy.b[,1])
X.b<-cbind(rep(1,n.b),x.b)
xy<-rbind(xy.a,xy.b)
x<-rbind(x.a,x.b)
y<-rbind(y.a,y.b)
X<-rbind(X.a,X.b)
p<-ncol(X)    	
n<-n.a+n.b					# ncol(X) = 6 + 1
# --------------------------------------------------------------------------------------------------
#
#	Check correlations of predictors
#
# --------------------------------------------------------------------------------------------------
C<-cor(x)
# --------------------------------------------------------------------------------------------------
#
#	Fit a linear model
#
# --------------------------------------------------------------------------------------------------
xy.l1<-lm(y~.,data=xy)
summary(xy.l1)
# --------------------------------------------------------------------------------------------------
#
#	LS estimator of beta, from the formula 
#
# --------------------------------------------------------------------------------------------------
beta.hat.LS<- solve( t(X) %*% X) %*% t(X) %*% y 
y.hat<- X %*% solve( t(X) %*% X) %*% t(X) %*% y
y.tilde<-y-y.hat
#
#	Residual sum of squares
#
RSS<-sum(y.tilde^2)
#
#	Residual mean squares
#
RMS<-RSS/(n-p)
#
#	Residual standard error
#
RSE<-sqrt(RMS)
#
#	Estimated variance of each y
#
sigma2.hat<-RMS
#
#	Estimated standard deviation of each y
#
sigma.hat<-RSE
#
#	Estimated variances and covariances matrix of the beta estimations
#
Var.beta.hat.LS<-solve( t(X) %*% X)*sigma2.hat
#
#	Estimated variances of the beta estimations
#
var.beta.hat.LS<-diag(Var.beta.hat.LS)
#
#	The second column in the summary
#
beta.sd<-sqrt(var.beta.hat.LS)
#
#	The third column in the summary
#
t<-beta.hat.LS/beta.sd
#
#	The fourth column in the summary
#
p.val<-2*pt(-abs(t),df=n-p)
#
#	Centered y values
#
y.0<-y-mean(y)
#
#	Centered y.hat values
#
y.hat.0<-y.hat-mean(y)
#
#	Total sum of squares (centered)
#
Total.SS.0<-sum(y.0^2)
#
#	Regression sum of squares (centered)
#
Regression.SS.0<-sum(y.hat.0^2)
#
#	Regression mean of squares (centered)
#
Regression.MS.0<-Regression.SS.0/(p-1)
#
#	The F statistic: it is que quotient of two estimations of the variance, from the
#	regression and from the residuals
#
F<-Regression.MS.0/RMS
#
#	Coefficient of determination
#
R2<-Regression.SS.0/Total.SS.0
#
#
#
Adj.R2<-1-(1-R2)*(n-1)/(n-p-1)
# --------------------------------------------------------------------------------------------------
#
#	Leave-One-Out cross-validation to decide the best lambda
#
# --------------------------------------------------------------------------------------------------
I<-diag(rep(1,p))
cv <- 1.0e50
N<-100
for (i in 1:N){     
	lambdai <- i/N
	cvi <- 0
	for (j in 1:n){
		bR <- solve( t(X[-j,]) %*% X[-j,] + lambdai*I) %*% t(X[-j,]) %*% y[-j]; 
		yje <- X[j,] %*% bR 
		cvi <- cvi +(yje-y[j])^2
		}
	if(cvi < cv){
		cv <- cvi
		lambda <- lambdai
		}
	}
# --------------------------------------------------------------------------------------------------
#
#	Ridge estimator of beta
#
# --------------------------------------------------------------------------------------------------
	
beta.hat.Ridge <- solve(t(X) %*% X + lambda*I) %*% t(X) %*% y

beta.hat.LS
beta.hat.Ridge
lambda

# --------------------------------------------------------------------------------------------------
#
#	Model validation for a new data set
#
# --------------------------------------------------------------------------------------------------
xy.new<- read.table("Fearn.data.b.txt",header=TRUE)
n.new<-nrow(xy.new)
x.new<-as.matrix(xy.new[,-1])
y.new<-as.matrix(xy.new[,1])
X.new<-cbind(rep(1,n.new),x.new)


error.LS <- mean((y.new-X.new %*% beta.hat.LS)^2)
error.Ridge <- mean((y.new-X.new %*% beta.hat.Ridge)^2)

error.LS
error.Ridge


