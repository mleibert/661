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
crabs$color<-factor(crabs$color,levels(crabs$color)[c(4,1,2,3)])

head(crabs)
str(crabs)

#main effect & interaction model
crab.glm<-glm( y~.,data=crabs ,family="binomial" )
glm( y~weight+factor(color),data=crabs ,family="binomial" )
summary(crab.glm) 
crabby.glm<-glm(y~weight,data=crabs ,family="binomial" )
summary(crabby.glm) 


#Wald tests
#Just for H0:b1 = 0
sqrt ( diag( vcov(crab.glm) ) )[2]

coef(crab.glm)[2] /   sqrt( diag( vcov(crab.glm) ) )[2] 

2*(1-pnorm(4.353582))

qnorm(0.975)

 anova(crab.glm, test="Chisq")

#wald CI for b1
coef(crab.glm)[2] - qnorm(.975)*sqrt(diag(vcov(crab.glm)))[2]
coef(crab.glm)[2] + qnorm(.975)*sqrt(diag(vcov(crab.glm)))[2]

confint.default(crab.glm)


wald.test(b= coef(crab.glm)[2] , sqrt ( diag( vcov(crab.glm) ) )[2],Terms=1)


#To test the significance of color, controlling for weight we must test 
#H0 : ß2 = ß3 = ß4 = 0. The likelihood-ratio statistic is
#G^2 = -2(L0 - L1) with 3 degrees of freedom

-2 * ( logLik(crabby.glm) - logLik(crab.glm) )

 ( 195.7371- 188.54225   )

1-pchisq(7.194895 , 3)

summary(crabby.glm) 
summary(crab.glm) 

195.73715 - 188.54225

crabI.glm<-glm( y~weight*color,data=crabs ,family="binomial" )
vcov(crab.glm)

#AIC: 
summary(crab.glm)[[4]]
#residual deviance
summary(crab.glm)[[5]]	

confint(crab.glm) 

summary(crab.glm)		#nested model
summary(crabI.glm)	#large model

168-165
length(coef(crabI.glm))-length(coef(crab.glm))


#nested - full(mixed effect)
1-pchisq(188.54-181.66 , 168-165)
#pretty low p-value: H_0: smaller model fits well
#.05 threshold too low, and the .07 does not show that the nested
#model does better than the mixed effects
#AIC is very close, but it is suggesting the larger model as well
AIC(crab.glm)
AIC(crabI.glm)
#However, for BIC the penalty for the larger model is suggesting the smaller
#one
BIC(crab.glm)
BIC(crabI.glm)

#I would say overall it is pretty close, but favoring the LRT I would conclude
#going with the interaction model. Although I dont think it is accurate to
#say that it is signicantly better than the main effects model.


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

#https://www2.stat.duke.edu/courses/Spring13/sta102.001/Lec/Lec20.pdf

donner<-DONNER<-read.csv("G:\\math\\661\\donner.csv",header=T)
donner<-DONNER<-read.table("G:\\math\\661\\donner.txt",header=T)

head(donner)
donner<-donner[,c(2,3,4)]
str(donner)
donner$Male.Gender<-as.factor(donner$Male.Gender)

donner.glm<-glm(Survived~Age+Male.Gender,data=donner,family=binomial )
summary(donner.glm)

plot(	donner[ which(donner[,2] > 15 & donner[,2] < 45
	),2],donner[ which(donner[,2] > 15 & donner[,2] < 45),3])

plot(	donner[ which(donner[,1] == 0),2],donner[ which(donner[,1] == 0),3],
	col=ifelse(donner[,2] > 15 & donner[,2] < 45 , "red", "black"),
	pch=16 )

eta<- sum( coef(donner.glm)*c(1,1,0))
eta<- sum( coef(donner.glm)*c(1,0,1))

anova(donner.glm, test= "LRT")

exp(eta)/(1+exp(eta))

#intepret b1, b2
#Controlling for gender, for every one year increase in age, the log odds
#of surviving decreases by -0.03502802

exp(-0.03502802)

#Controlling for age, males have a 1.19255 lower log odds than women for
#surviving


exp(1.19255)

#these data are not grouped so the deviance or Pearson goodness-of-fit
#tests are not appropriate.






























