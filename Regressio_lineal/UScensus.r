#
#	Exemple cl�ssic de mal condicionament de la regressi� polin�mica.
#	Dades del cens dels Estats Units en els anys de 1900 a 1990.
#
#	t �s l'any
#	p �s la poblaci� en milions
#
#	Regressi� polin�mica de p sobre t amb el prop�sit de fer 
#	predicci� de la poblaci� al 2000. 
#	Graus k de 3 a 8 (a partir de grau 7 molta inestabilitat)
#
# --------------------------------------------------------------------
t <-seq(1900,1990,by=10)
p <-c(75.995,91.972,105.711,123.203,131.669,150.697,179.323,203.212,226.505,249.633)