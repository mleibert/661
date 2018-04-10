rm(list = ls())
setwd("G:\\math\\661")

 
2 *( -114.14- -114.41 )

1-pchisq(2 *( -114.14- -114.41 ) , 2  )



dat<-read.table("Bank_loan.txt",header=T)
library(ggplot2)
library(reshape2)

 ggplot(data = melt(dat[!(is.na(dat[,ncol(dat)])),]), 
 		mapping = aes(x = value)) + 
 	geom_histogram( bins = 10, color="darkblue", fill="lightblue") + 
 	facet_wrap(~variable, scales = 'free_x')

hist(dat$age)

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
  dat$default <- as.factor(dat$default )
 
# 2a
fit.null<-glm(default~1,family=binomial, data=dat)
fit.sat<-glm(default~. ,family=binomial, data=dat)	
summary(fit.sat)

  


step(fit.null, scope=list(lower=fit.null, 
	upper=fit.sat), direction="forward" ,trace=0 )
step(fit.null, scope=list(lower=fit.null, 
	upper=fit.sat), direction="both" ,trace=0 )

step(fit.sat, scope=list(lower=fit.sat, 
	upper=fit.null), direction="backward" ,trace=0)
step(fit.sat, scope=list(lower=fit.sat, 
	upper=fit.null), direction="both" ,trace=0)


stepFit<-glm(formula = default ~ debtinc + employ + creddebt + address + 
    age, family = binomial, data = dat)


### GOF
library(ResourceSelection)

res<-hoslem.test(stepFit$y,fitted(stepFit))
res
cbind(res$observed,res$expected)


### LASSO (Indians data set)

library("mlbench")
library(glmnet)
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
X = model.matrix(default ~ . , data=dat)
 

Y = as.numeric(dat$default  )

cvfit = cv.glmnet(x=X[,-1], y=Y, family="binomial", type.measure="class")
plot(cvfit)

lambda_1se = cvfit$lambda.1se
coef(cvfit, s=lambda_1se)

lassoFit<-glm(formula = default ~ debtinc + employ + creddebt + address ,
	family = binomial, data = dat)

#worse AIC & BIC see below
#lassoiFit<-glm(  default ~ employ +  address + debtinc + creddebt +
#	age*debtinc + debtinc:creddebt ,
#	 family = binomial, data = dat)




summary(stepFit)
summary(lassoFit)
summary(lassoiFit)
BIC(lassoiFit)
BIC(lassoFit)

#H0 : model with 4 covariates fits as well as model with 5 covariates 
deviance(lassoFit)-deviance(stepFit)

################ 2b
1-pchisq(deviance(lassoFit)-deviance(stepFit), 1)

#We fail to reject H0 at alpha = 0:05, 
#thus the model with 5 covariates does not provide a
#better fit compared to the model with 4 covariates.

 
lassoFit

#2ci

#because this is ungrouped data we use the deviance residuals
h<-hatvalues(lassoFit)

#d<-resid(lassoFit,type="deviance")
#d.std<-d/sqrt(1-h)

e<-resid(lassoFit,type="pearson")
e.std<-e/sqrt(1-h)
hist(e.std)

dat$pii<-predict(lassoFit,   type="response")

 
lassoFit$linear.predictors
lassoFit$fitted.values


as.numeric( which( abs(e.std) > 2 ) )


glm(formula = default ~ debtinc + employ + creddebt + address ,
	family = binomial, data = dat)

data.frame( names(dat) , 1:ncol(dat) )

dat[as.numeric( which( abs(e.std) > 2 ) ), c(6,3,7,4,9,10) ]

table(dat[as.numeric( which( dat$pii > .7 ) ), c(6,3,7,4,9,10) ]$default)
table(dat[as.numeric( which( dat$pii < .08 ) ), c(6,3,7,4,9,10) ]$default)

dat[which(dat$default == 0),]

##2cii

nrow(   dat[which(dat$pii > .3),]   ) / 700

table( dat[ ( which( dat$pii < .3) ),]$default )
table( dat[ ( which( dat$pii > .3) ),]$default )

defaulters<-dat[which(dat$default == 1),]
nrow( defaulters[which(defaulters$pii < .3) ,] ) / nrow(defaulters)

defaulters<-dat[which(dat$default == 0),]
nrow( defaulters[which(defaulters$pii > .3) ,] ) / nrow(defaulters)

##2ciii
install.packages("ROCR")
library(ROCR)


pred = prediction(fitted(lassoFit) ,   dat$default  )
perf = performance(pred, "tpr", "fpr")
plot(perf)
abline(a=0, b=1, lty=2)

auc.perf = performance(pred, "auc")
auc.perf@y.values

#########

head(prosp)

blergs<-prosp[,c(6,3,7,4)]

blergs$pii<-predict(lassoFit,  blergs, type="response")
prosp$pii<-predict(lassoFit,  prosp, type="response")
blergs$pii-prosp$pii

hist(prosp$pii)
boxplot(prosp$pii)

nrow( prosp[which(prosp$pii > .3),]  )  / nrow(prosp)






exp (-.82 + 1.96 * .41)
