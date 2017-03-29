#
#	Exemple clàssic de mal condicionament de la regressió polinòmica.
#	Dades del cens dels Estats Units en els anys de 1900 a 1990.
#
#	t és l'any
#	p és la població en milions
#
#	Regressió polinòmica de p sobre t amb el propòsit de fer 
#	predicció de la població al 2000. 
#	Graus k de 3 a 8 (a partir de grau 7 molta inestabilitat)
#
# --------------------------------------------------------------------
t <-seq(1900,1990,by=10)
p <-c(75.995,91.972,105.711,123.203,131.669,150.697,179.323,203.212,226.505,249.633)