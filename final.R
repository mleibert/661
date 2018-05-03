rm(list = ls())
setwd("G:\\math\\661")
options(scipen=999)
require(ggplot2)

tempp<-data.frame( rep( "low" ,6 ) );names(tempp)<-"pollution"
tempp$exposure<- c( rep( "no" ,3 ),  rep( "yes" ,3 ))
tempp$smoker<-  rep( c("non","ex","current") ,2 ) 
mat<-matrix(NA,6,4)
mat[1,]<-c(158,9,5,0 )
mat[2,]<-c(167 ,19 , 5, 3)
mat[3,]<-c(307,102,83 ,68)
mat[4,]<-c(26, 5 ,5, 1)
mat[5,]<-c(38, 12, 4, 4 )
mat[6,]<-c(94, 48, 46, 60 )
cough<-cbind(tempp,mat)
names(cough)[ 4:ncol(cough)]<-paste0("Y",1:4)

tempp<-data.frame( rep( "high" ,6 ) );names(tempp)<-"pollution"
tempp$exposure<- c( rep( "no" ,3 ),  rep( "yes" ,3 ))
tempp$smoker<-  rep( c("non","ex","current") ,2 ) 
mat<-matrix(NA,6,4)
mat[1,]<-c(94, 7, 5 ,1 )
mat[2,]<-c(67, 8, 4 ,3)
mat[3,]<-c(184, 65, 33, 36)
mat[4,]<-c(32, 3, 6, 1)
mat[5,]<-c(39, 11, 4 ,2)
mat[6,]<-c(77, 48, 39, 51)
tempp<-cbind(tempp,mat)
names(tempp)[ 4:ncol(tempp)]<-paste0("Y",1:4)

cough<-rbind(cough,tempp)
 
cough.ungrp = as.data.frame(lapply(dat, 
	function(x,p) rep(x,p), dat$count))


str(cough)
cough$smoker<-relevel(as.factor(cough$smoker), ref="non")
cough$exposure<-relevel(as.factor(cough$exposure), ref="no")
cough$pollution<-relevel(as.factor(cough$pollution), ref="low")



#Fit a proportional odds cumulative logit model with pairwise interaction 
#efects for all covariates and assess its goodness of fit. 
#Use low air pollution, no job exposure and non-smoker as reference group.

cough



require(VGAM)
cough.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution+ exposure + smoker,
	family=cumulative(parallel=T),data=cough)
summary(cough.fit)

tail( cough.ungrp  )
vglm.cough = vglm(level ~ pollution+ exposure + smoker,
	family=cumulative(parallel=T),data=cough.ungrp  )
summary(cough.fit)
summary(vglm.cough )



#goodness of fit
1-pchisq(29.9969, 29)
1-pchisq(4178.511 , 6260 )


#We can test the proportional odds assumption
H0 : same slope for all cumulative logits vs. H1 : different slopes

#example
source("cheese.R")
vglm.fit = vglm(response ~ type, family=cumulative(parallel=TRUE),
	data=cheese.ungrp)
noprop.fit = vglm(response ~ type, family=cumulative, data=cheese.ungrp)

1-pchisq(-2*(logLik(vglm.fit)-logLik(noprop.fit)),
	df=df.residual(vglm.fit)-df.residual((noprop.fit)))

nocough.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution+ exposure + smoker,
	family=cumulative ,data=cough)



1-pchisq(-2*(logLik(cough.fit)-logLik(nocough.fit)),
	df=df.residual(cough.fit)-df.residual((nocough.fit)))








