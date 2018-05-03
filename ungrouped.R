summary( polr( as.factor(level) ~  (smoker) + 
	 (exposure) +   (pollution), 
	weights=count, data=dat) )


for(i in 1:ncol(dat)){dat[,i]<-as.numeric(dat[,i])}

 
 str(cough)
str(dat)
str(cough.ungrp )


vglm.cough<-vglm(as.factor(level) ~exposure+ pollution+ smoker +
	smoker*exposure + smoker*pollution+pollution*exposure ,
  	family=cumulative(parallel=T),data=cough.ungrp ) 

noprop.cough<-vglm(as.factor(level) ~exposure+ pollution+ smoker +
	smoker*exposure + smoker*pollution+pollution*exposure ,
  	family=cumulative(parallel=F),data=cough.ungrp ) 

1-pchisq(4161.64,6252)

1-pchisq(-2*(logLik(vglm.cough)-logLik(noprop.cough)),
	df=df.residual(vglm.cough)-df.residual((noprop.cough)))




cough.ungrp$smoker<-relevel(as.factor(cough.ungrp$smoker), ref="Non")
cough.ungrp$exposure<-relevel(as.factor(cough.ungrp$exposure), ref="no")
cough.ungrp$pollution<-relevel(as.factor(cough.ungrp$pollution), ref="low")
dat




