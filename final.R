rm(list = ls())
setwd("G:\\math\\661")
options(scipen=999)
require(ggplot2)
library(MASS)
library(VGAM)

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
dat


require(VGAM)
library(MASS)

polr.fit = polr(as.factor(level) ~ smoker+ exposure+ pollution +
	smoker*exposure + smoker*pollution+pollution*exposure , 
	weights=count, data=dat)
df.residual(polr.fit )


cough.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution+ as.factor(exposure) +
	as.factor( smoker) ,family=cumulative(parallel=T),data=cough)

## use this guy

cough.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution+  (exposure) +( smoker)+
	smoker*exposure + smoker*pollution+pollution*exposure ,
	  family=cumulative(parallel=T),data=cough)

1-pchisq(27.29641,24)


main.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution+  (exposure) +( smoker),
	family=cumulative(parallel=T),data=cough)





vglm.cough<-vglm(as.factor(level) ~exposure+ pollution+ smoker +
	smoker*exposure + smoker*pollution+pollution*exposure ,
  	family=cumulative(parallel=T),data=cough.ungrp ) 

noprop.cough<-vglm(as.factor(level) ~exposure+ pollution+ smoker +
	smoker*exposure + smoker*pollution+pollution*exposure ,
  	family=cumulative(parallel=F),data=cough.ungrp ) 


1-pchisq(deviance(polr.fit ),df.residual(polr.fit ))

1-pchisq(29.99692,29)
logLik(polr.fit )
logLik(cough.fit)

 res<-hoslem.test(polr.fit$level,fitted(polr.fit))

library(ResourceSelection)
cough.fit


summary(cough.fit)
str(cough)
tail( cough.ungrp  )
vglm.cough = vglm(level ~ pollution+ exposure + smoker,
	
	family=cumulative(parallel=T),data=cough.ungrp  )
summary(cough.fit)
summary(vglm.cough )
 
donner.glm<-glm(vs~mpg+cyl,data=mtcars,family=binomial )
 
res<-hoslem.test(donner.glm$y,fitted(vglm.cough) )



#goodness of fit
1-pchisq(29.9969, 29)
1-pchisq( 4175.81, 6255 )

#Use a likelihood ratio test to check the proportional
#odds assumption in the model above.
#We can test the proportional odds assumption
H0 : same slope for all cumulative logits vs. H1 : different slopes

#example
source("cheese.R")
vglm.fit = vglm(response ~ type, family=cumulative(parallel=TRUE),
	data=cheese.ungrp)
noprop.fit = vglm(response ~ type, family=cumulative, data=cheese.ungrp)

1-pchisq(-2*(logLik(vglm.fit)-logLik(noprop.fit)),
	df=df.residual(vglm.fit)-df.residual((noprop.fit)))

noprop.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution+  (exposure) +( smoker)+
	smoker*exposure + smoker*pollution+pollution*exposure ,
	  family=cumulative(parallel=F),data=cough)

1-pchisq(-2*(logLik(cough.fit)-logLik(noprop.fit)),
	df=df.residual(cough.fit)-df.residual((noprop.fit)))


#Use a likelihood ratio test to determine whether to include or not the 
#interaction terms in the proportional odds cumulative logit model.

#H0: model with main effects fits as well as model with interaction effects


deviance(main.fit)-deviance(cough.fit)
1-pchisq(deviance(main.fit)-deviance(cough.fit), 5)


# In the following questions, use the main effects cumulative logit 
# proportional odds model:

#Interpret each of the three intercepts.

summary(main.fit)







