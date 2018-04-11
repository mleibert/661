rm(list = ls())
setwd("G:\\math\\661")
options(scipen=999)
library("VGAM")


library(MASS)
cheese<-read.table("cheese.txt",header=T)
polr.fit = polr(as.factor(response) ~ type, weights=count, data=cheese)

cheese.ungrp = as.data.frame(
	lapply(cheese, function(x,p) rep(x,p), cheese$count))

 

tail(cheese);tail(cheese.ungrp )
cheese.ungrp <- cheese.ungrp[,-3] 

 

vglm.fit = vglm(response ~ type, 
	family=cumulative(parallel=T), data=cheese.ungrp)
summary(vglm.fit)
summary( polr.fit  )