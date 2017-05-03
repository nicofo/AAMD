require(rpart)
data(stagec)
#
#	Los datos stagec son, en realidad, datos para el análisis de supervivencia. La primera variable,
#	pgtime es el tiempo de supervivencia para los pacientes que progresan. La segunda variable, pgstat,
#	es el indicador de progreso (1="progresa", 0="censurado"). 
#	Quitamos estas dos variables y cambiamos el nombre de la variable pgstat y de sus niveles.
#
#
stagec1<-stagec[,-(1:2)]
stagec1$progstat <- factor(stagec$pgstat, levels=0:1, labels=c("No", "Prog"))
#  complete.cases(stagec1)
#
#	Hay casos con variables missing. En total hay:
#
sum(!complete.cases(stagec1))
#[1] 12
#
#	El data.frame stagec2 contiene 134 = 146 - 12 casos sin ningún valor missing.
#
stagec2<-stagec1[complete.cases(stagec1),]
# --------------------------------------------------------------------------------------------------
#
#	Ajuste de un modelo rpart
#
# --------------------------------------------------------------------------------------------------
stagec1.rpart.1<-rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,data=stagec1, method="class")
stagec2.rpart.1<-rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,data=stagec2, method="class")
plot(stagec1.rpart.1)
text(stagec1.rpart.1)
# --------------------------------------------------------------------------------------------------
#
#	Ver el resultado de print, la información que da de cada nodo y la forma de presentación.
#
# --------------------------------------------------------------------------------------------------
print(stagec2.rpart.1)
plot(stagec2.rpart.1)
text(stagec2.rpart.1)
# --------------------------------------------------------------------------------------------------
#
#	Ajustar manualmente los parámetros del árbol 
#
# --------------------------------------------------------------------------------------------------
# control.parms<-rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
#			maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10, 
#			surrogatestyle = 0, maxdepth = 30, ...)
control.parms<-rpart.control(minsplit = 10, cp=0.005)
stagec2.rpart.2<-rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,data=stagec2, 
			method="class", control=control.parms)
print(stagec2.rpart.2)
plot(stagec2.rpart.2)
text(stagec2.rpart.2,cex=0.8)
# --------------------------------------------------------------------------------------------------
#
#	Mostrar el error de validación cruzada en función de cp
#
# --------------------------------------------------------------------------------------------------
printcp(stagec2.rpart.2)
plotcp(stagec2.rpart.2)
# --------------------------------------------------------------------------------------------------
#
#	Podar el árbol según un valor de cp
#
# --------------------------------------------------------------------------------------------------
stagec2.rpart.3<-prune(stagec2.rpart.2,cp=0.052)
print(stagec2.rpart.3)
plot(stagec2.rpart.3)
text(stagec2.rpart.3,cex=0.8)


