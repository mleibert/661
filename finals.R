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
cough


cough$smoker<-relevel(as.factor(cough$smoker), ref="non")
cough$exposure<-relevel(as.factor(cough$exposure), ref="no")
cough$pollution<-relevel(as.factor(cough$pollution), ref="low")
str(cough)

vglm(cbind(Y1,Y2,Y3,Y4) ~1,
	family=cumulative(parallel=T),data=cough)

  
#1
inter.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution + exposure + smoker +
	smoker*exposure + smoker*pollution+pollution*exposure ,
	family=cumulative(parallel=T),data=cough)
summary(inter.fit)
1-pchisq(27.29641, 24)


#2
#Use a likelihood ratio test to check the proportional
#odds assumption in the model above.
#We can test the proportional odds assumption
#H0 : same slope for all cumulative logits vs. H1 : different slopes

noprop.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution + exposure + smoker +
	smoker*exposure + smoker*pollution+pollution*exposure ,
	family=cumulative(parallel=F),data=cough)


1-pchisq(-2*(logLik(inter.fit)-logLik(noprop.fit)),
	df=df.residual(inter.fit)-df.residual((noprop.fit)))

#We fail to reject the null and can assume a proportional odds structure


#3
#Use a likelihood ratio test to determine whether to include or not the 
#interaction terms in the proportional odds cumulative logit model.


main.fit<-vglm(cbind(Y1,Y2,Y3,Y4) ~ pollution + exposure + smoker  ,
	family=cumulative(parallel=T),data=cough)
summary(main.fit)
1-pchisq(29.99692,36-29)


#H0: model with main effects fits as well as model with interaction effects

df.residual(inter.fit)
df.residual(main.fit)

1-pchisq(deviance(main.fit)-deviance(inter.fit), 5)

#We fail to reject $H_0$ at $\alpha = 0.05$, thus the model with interaction
#effects doesn't provide a better fit compared to the main effects model.


#In the following questions, use the main effects cumulative logit
#proportional odds model:

#Interpret each of the three intercepts.

summary(main.fit)

#P(grp 1)
a1=2.08
exp(a1)/(1+exp(a1))
exp(a1-0.86476)/(1+exp(a1-0.86476))
 
#P(grp 2)
a2=2.96964
exp(a2)/(1+exp(a2))
exp(a2-0.86476)/(1+exp(a2-0.86476))

exp(-0.40003)

coughing<-data.frame( 
	pol  = (as.numeric(cough$pollution)-1),
	exp  = (as.numeric(cough$exposure)-1),
	Cs = ifelse( (as.numeric(cough$smoker)-1) == 1 , 1,0 ),
	EXs = ifelse( (as.numeric(cough$smoker)-1) == 2 , 1,0 )
)
coughing<-cbind(coughing,matrix(0,12,3))



for(i in 1:nrow(coughing)){
coughing[i,5]<-	t( as.matrix( c(1,as.numeric( coughing[i,1:4]) )) ) %*% 
	as.matrix(coef(main.fit)[ -c(2,3)] )
coughing[i,6]<-t( as.matrix( c(1,as.numeric( coughing[i,1:4]) )) ) %*% 
	as.matrix(coef(main.fit)[ -c(1,3)] )
coughing[i,7]<-t( as.matrix( c(1,as.numeric( coughing[i,1:4]) )) ) %*% 
	as.matrix(coef(main.fit)[ -c(1,2)] )
}

 

coughing[,5:7]<-round(apply(coughing[,5:7],2, function(w)
 (1+exp(-w))^(-1) ),4)


coughing$four<-1-coughing[,ncol(coughing)]
names(coughing)[(ncol(coughing)-3):ncol(coughing)]<-paste0("pi",1:4)
#cough$totals<-rowSums(cough[,-c(1:3)])

coughing<-cbind(cough,coughing)
coughing

coughing$pi3<-coughing$pi3-coughing$pi2
coughing$pi2<-coughing$pi2-coughing$pi1

coughing[(ncol(coughing)-3):ncol(coughing)]*coughing$total

 
a=coef(main.fit)[-c(combn(1:3,2)[,1],7)]
a
sum(a)
exp( sum( a) 	 ) / (	1+exp( sum( a ) ) )

coughing<-cough
coughing$tY<-rowSums(coughing[,4:7])
coughing<-cbind(coughing,round(predict(main.fit,type="response"),3))
names(coughing)[(ncol(coughing)-3):ncol(coughing)]<-paste0("pi",1:4)

cbind(coughing, coughing[,(ncol(coughing)-3):ncol(coughing)]*
	coughing[,(ncol(coughing)-4)] )

round(predict(main.fit,type="response"),4)*cough$totals
print( cbind(coughing, coughing[,(ncol(coughing)-3):ncol(coughing)]*
	coughing[,(ncol(coughing)-4)] ),row.names=F)






















































