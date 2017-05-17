# --------------------------------------------------------------------------------------------------
#
#	Función auxiliar para calcular la estimación de la probabilidad de error a partir
#	de la matriz de confusión.
#
# --------------------------------------------------------------------------------------------------   
prob.err<-function(T){
	n<-sum(T)
	n1<-sum(diag(T))
	return(round(100*(n-n1)/n,2))
	}
