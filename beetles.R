rm(list = ls())
setwd("G:\\math\\661")


beetles<-read.table("G:\\math\\661\\beetles.txt",header=T)
beetles$alive<-beetles$n - beetles$dead

attach(beetles)
beetles.logit<- glm(	cbind(dead,alive)~logdose ,family="binomial" )
detach(beetles)

fitted(beetles.logit)


crabs<-CRABS<-read.table("G:\\math\\661\\crabs.txt",header=T)
crabs$color<-as.factor(CRABS$color)


crabs$y<-ifelse(crabs$satellite > 0, 1,0)
crabs<-crabs[,c(2,4,ncol(crabs))] 
crabs$color<-as.factor(crabs$color)

head(crabs)
str(crabs)

crab.glm<-glm( y~.,data=crabs ,family="binomial" )

summary(crab.glm) 
#AIC: 
summary(crab.glm)[[4]]
#residual deviance
summary(crab.glm)[[5]]	

crabI.glm<-glm( y~weight*color,data=crabs ,family="binomial" )
glm( y~weight+color+weight*color,data=crabs ,family="binomial" )


summary(crabI.glm) 
#AIC: 
#residual deviance
summary(crabI.glm)[[5]]	


summary(crab.glm)
summary(crabI.glm)

#nested - full(mixed effect)
1-pchisq(188.54-181.66 , 168-165)
#pretty low p-value: H_0: smaller model fits well
#.05 threshold too low, and the .07 does not show that the nested
#model does better than the mixed effects
summary(crab.glm)[[4]]
summary(crabI.glm)[[4]]





######### compare nested models

delinquent = data.frame(ses = as.factor(rep(c("low", "medium", "high"),
	rep(2,3))),boy = as.factor(rep(c("scout", "nonscout"),3)),
	y = c(11, 42, 14, 20, 8, 2), n = c(54, 211, 118, 152, 204, 61))

# R uses the first levels as reference.
# To use boy="non-scouts" and ses="low" as reference

delinquent$boy = relevel(delinquent$boy, ref="nonscout")
delinquent$ses = relevel(delinquent$ses, ref="low")
fit.null = glm(y/n ~ 1, weights=n, family=binomial, data=delinquent)
fit.boy = glm(y/n ~ boy, weights=n, family=binomial, data=delinquent)
fit.ses = glm(y/n ~ ses, weights=n, family=binomial, data=delinquent)
fit.boyses = glm(y/n ~ ses+boy, weights=n, family=binomial, data=delinquent)
fit.saturate = glm(y/n ~ boy*ses, weights=n, family=binomial, data=delinquent)

summary( fit.saturate  )
summary(fit.boyses )
summary(fit.ses )





