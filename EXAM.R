rm(list = ls())
setwd("G:\\math\\661")
dat<-read.table("Bank_loan.txt",header=T)
library(ggplot2)
library(reshape2)

 
#ggplot(data = melt(dat[!(is.na(dat[,ncol(dat)])),]), 
#		mapping = aes(x = value)) + 
#	geom_histogram( bins = 10, color="darkblue", fill="lightblue") + 
#	facet_wrap(~variable, scales = 'free_x')

dat$ed<-as.factor(dat$ed)
summary(dat)

tail(dat)
a<-which(dat$ed == 5)
dat[which(dat$ed == 5),2]<-4
dat[a, ];rm(a)

prosp<-dat[is.na(dat[,ncol(dat)]),]
dat<-dat[!(is.na(dat[,ncol(dat)])),]
nrow(dat);nrow(prosp)

tail(dat)
 
 
# 2a
fit.null<-glm(default~1,family=binomial, data=dat)
fit.sat<-glm(default~.*.,family=binomial, data=dat)	

summary(fit.sat)

  


step(fit.null, scope=list(lower=fit.null, 
	upper=fit.sat), direction="both" )

#step(fit.sat, scope=list(lower=fit.null, 
#	upper=fit.sat), direction="both" )


stepFit<-glm(formula = default ~ debtinc + employ + creddebt + address + 
    age, family = binomial, data = dat)


### GOF
library(ResourceSelection)

res<-hoslem.test(stepFit$y,fitted(stepFit))
res
cbind(res$observed,res$expected)


### LASSO (Indians data set)

library("mlbench")
#library(glmnet)
data("PimaIndiansDiabetes")

X = model.matrix(diabetes ~ .*., data=PimaIndiansDiabetes)
Y = as.numeric(PimaIndiansDiabetes$diabetes=="pos")

cvfit = cv.glmnet(x=X[,-1], y=Y, family="binomial", type.measure="class")
plot(cvfit)

lambda_1se = cvfit$lambda.1se
coef(cvfit, s=lambda_1se)

tail(PimaIndiansDiabetes)

### Bank data set

tail(dat)
X = model.matrix(default ~ .*., data=dat)
Y = as.numeric(dat$default  )

cvfit = cv.glmnet(x=X[,-1], y=Y, family="binomial", type.measure="class")
plot(cvfit)

lambda_1se = cvfit$lambda.1se
coef(cvfit, s=lambda_1se)

lassoFit<-glm(formula = default ~ debtinc + employ + creddebt + address ,
	family = binomial, data = dat)

lassoiFit<-glm(  default ~ employ +  address + debtinc + creddebt +
	age*debtinc + debtinc:creddebt ,
	 family = binomial, data = dat)




summary(stepFit)
summary(lassoFit)
summary(lassoiFit)
BIC(lassoiFit)
BIC(lassoFit)

#H0 : model with 4 covariates fits as well as model with 5 covariates 
deviance(lassoFit)-deviance(stepFit)

1-pchisq(deviance(lassoFit)-deviance(stepFit), 1)

#We fail to reject H0 at alpha = 0:05, 
#thus the model with 5 covariates does not provide a
#better fit compared to the model with 4 covariates.

 


 