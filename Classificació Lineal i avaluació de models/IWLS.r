# --------------------------------------------------------------------------------------------------
#
#	La funcion logística
#
# --------------------------------------------------------------------------------------------------
F<-function(z){1/(1+exp(-z))}
# --------------------------------------------------------------------------------------------------
#
#	La función log-verosimilitud 
#
#	y	:	vector [n,1] de respuestas (binarias, 0,1)
#	p	:	vector [n,1] de probabilidades. Atención! no puede haber 0's ni 1's, no se verifica
#			(habría que arreglar este código para que fuera más seguro)
#
# --------------------------------------------------------------------------------------------------
logLik<-function(p,y){sum(y*log(p)+(1-y)*log(1-p))}
# --------------------------------------------------------------------------------------------------
#
#	WLS
#
#	Entra: 
#
#	X		:	matriz [n,k] de predictores (incluyendo columna de 1's para intercept)
#	y		:	vector [n,1] de respuestas (binarias, 0,1)
#	p		: 	vector [n,1] de probabilidades
#
#	Retorna:
#
#	delta	:	vector [k,1]  Incrementos del vector beta
#
#	Cantidades internas:
#
#	w		: 	vector [n,1] de pesos (varianzas de las observaciones individuales)
#	G1		:   matriz [k,k] inversa de la matriz de información de Fisher (segunda derivada)
#
# --------------------------------------------------------------------------------------------------
WLS<-function(X,y,p){
		w<-as.numeric(p*(1-p))
		G1<-solve(t(X)%*%diag(w)%*%X)
		delta<-G1%*%t(X)%*%(y-p)
		return(delta)
		}
# --------------------------------------------------------------------------------------------------
#
#	Leer datos y preparar matrices X, y
#
#
#	X		:	matriz [n,p] de predictores (incluyendo columna de 1's para intercept)
#	y		:	vector [n,1] de respuestas (binarias, 0,1)
#	
# --------------------------------------------------------------------------------------------------
chdage<-read.table("chdage.txt",header=TRUE)
X<-as.matrix(chdage$age)
y<-as.matrix(chdage$CHD)
# --------------------------------------------------------------------------------------------------
#
#	Añadir la columna de unos a X
#
# --------------------------------------------------------------------------------------------------
n<-nrow(X)
X<-cbind(rep(1,n),X)
# --------------------------------------------------------------------------------------------------
#
#	Estimación inicial de p = Prob(Y=1) : suponemos que no hay x, de modo que son observaciones
#	de Bernoulli igualmente distribuidas
#
# --------------------------------------------------------------------------------------------------
n1<-sum(y)
p0<-n1/n
# --------------------------------------------------------------------------------------------------
#
#	IWLS
#
#	1. Inicializar cantidades para la iteración
#
# --------------------------------------------------------------------------------------------------
n<-nrow(X)
k<-ncol(X)
p<-rep(p0,n)
logL0<-logLik(p,y)		# Log-verosimilitud con la p inicial
logL0
#[1] -68.33149
# --------------------------------------------------------------------------------------------------
beta<-matrix(rep(0,k),ncol=1)
epsilon<-1.0e-5
Max.iter<-25
# --------------------------------------------------------------------------------------------------
#
#	2. Bucle de iteración
#
# --------------------------------------------------------------------------------------------------
for(t in 1:Max.iter){
	delta<-WLS(X,y,p)
	beta<-beta+delta
	p1<-F(X%*%beta)
	inc<-max(abs(p1-p))
	if(inc<epsilon) break 
		else p<-p1
	}
# --------------------------------------------------------------------------------------------------
#
#	3. Resultados
#
# --------------------------------------------------------------------------------------------------
t
#	[1] 5				# se han hecho 5 iteraciones
#
logL1<-logLik(p,y) 		# Log-verosimilitud con la p final
logL1
#[1] -53.67655
beta
#           [,1]
#[1,] -5.3094534
#[2,]  0.1109211
#
# --------------------------------------------------------------------------------------------------
#
#	Comparación con los resultados de la función glm
#
# --------------------------------------------------------------------------------------------------
l1<-glm(CHD~age,family=binomial,data=chdage)
summary(l1)
#Call:
#glm(formula = CHD ~ age, family = binomial, data = chdage)
#
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-1.9718  -0.8456  -0.4576   0.8253   2.2859  
#
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -5.30945    1.13365  -4.683 2.82e-06 ***
#age          0.11092    0.02406   4.610 4.02e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 136.66  on 99  degrees of freedom
#Residual deviance: 107.35  on 98  degrees of freedom
#AIC: 111.35
#
#Number of Fisher Scoring iterations: 4
#
# --------------------------------------------------------------------------------------------------
#
#	Las desvianzas se obtienen de las Log-verosimilitudes
#
# --------------------------------------------------------------------------------------------------
-2*logL0
#[1] 136.663
-2*logL1
#[1] 107.3531
#
# --------------------------------------------------------------------------------------------------


