# --------------------------------------------------------------------------------------------------
#
#	Funci�n auxiliar para calcular la estimaci�n de la probabilidad de error a partir
#	de la matriz de confusi�n.
#
# --------------------------------------------------------------------------------------------------   
prob.err<-function(T){
	n<-sum(T)
	n1<-sum(diag(T))
	return(round(100*(n-n1)/n,2))
	}
