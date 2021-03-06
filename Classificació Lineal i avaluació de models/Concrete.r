#
#	Preparació de les dades 'concrete'
#
require(regsel)
data(concrete)
#
#	Estandarditzem els predictors
#
conc.s<-scale(concrete)
str(conc.s)
# num [1:1030, 1:9] 2.477 2.477 0.491 0.491 -0.79 ...
# - attr(*, "dimnames")=List of 2
#  ..$ : NULL
#  ..$ : chr [1:9] "Cement" "BlastFurnaceSlag" "FlyAsh" "Water" ...
# - attr(*, "scaled:center")= Named num [1:9] 281.2 73.9 54.2 181.6 6.2 ...
#  ..- attr(*, "names")= chr [1:9] "Cement" "BlastFurnaceSlag" "FlyAsh" "Water" ...
# - attr(*, "scaled:scale")= Named num [1:9] 104.51 86.28 64 21.35 5.97 ...
#  ..- attr(*, "names")= chr [1:9] "Cement" "BlastFurnaceSlag" "FlyAsh" "Water" ...
conc.s.df<-as.data.frame(conc.s)
str(conc.s.df)
#'data.frame':   1030 obs. of  9 variables:
# $ Cement             : num  2.477 2.477 0.491 0.491 -0.79 ...
# $ BlastFurnaceSlag   : num  -0.856 -0.856 0.795 0.795 0.678 ...
# $ FlyAsh             : num  -0.847 -0.847 -0.847 -0.847 -0.847 ...
# $ Water              : num  -0.916 -0.916 2.174 2.174 0.489 ...
# $ Superplasticizer   : num  -0.62 -0.62 -1.04 -1.04 -1.04 ...
# $ CoarseAggregate    : num  0.8627 1.0557 -0.5263 -0.5263 0.0705 ...
# $ FineAggregate      : num  -1.217 -1.217 -2.24 -2.24 0.648 ...
# $ Age                : num  -0.28 -0.28 3.55 5.06 4.98 ...
# $ CompressiveStrength: num  2.644 1.561 0.266 0.313 0.508 ...

