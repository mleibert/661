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
summary(crabI.glm)[[4]]
#residual deviance
summary(crabI.glm)[[5]]	


#w/o interaction
sum(resid(crab.glm,type="pearson")^2)

1-pchisq(summary(crab.glm)[[5]],168	)
1-pchisq(sum(resid(crab.glm,type="pearson")^2),168	)


#w/o interaction
sum(resid(crabI.glm,type="pearson")^2)

1-pchisq(summary(crabI.glm)[[5]],168	)
1-pchisq(sum(resid(crabI.glm,type="pearson")^2),168	)

#homer lemshow
require("ResourceSelection")

hoslem.test(crab.glm$y,fitted(crab.glm))
hoslem.test(crabI.glm$y,fitted(crabI.glm))

