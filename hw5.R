rm(list = ls())
setwd("G:\\math\\661")
options(scipen=999)

smoke<-data.frame(
	c(  35 , 44 , 18793 , 52407 , 2 , 32 ),
	c( 45 , 54 , 10673 , 43248 , 12 , 104 ),
	c( 55 , 64 , 5710 , 28612 , 28 , 206 ),
	c( 65 , 74 , 2585 , 12663 , 28 , 186 ),
	c( 75 , 84 , 1462 , 5317 , 31 , 102 )  )

smoke<-t(as.matrix(smoke))
smoke<-as.data.frame(smoke)
rownames(smoke)<-paste0( smoke$V1,"-", smoke$V2)
smoke<-smoke[,-c(1:2)]
names(smoke)<-c(paste0("PY",c("nonsmokers", "smokers")),
	paste0("CD",c("nonsmokers", "smokers")))


smokers<-data.frame(rep(1:5,2),c(rep("NS",5),rep("S",5))	,
	c(smoke$PYnonsmokers,smoke$PYsmokers),
	c(smoke$CDnonsmokers,smoke$CDsmokers))

names(smokers)<-c("age","smoker","PersonYears","Deaths")
smokers$age<-as.factor(smokers$age)
smokers$ageQI<-as.numeric(smokers$age)
smokers$rates<-smokers$Deaths/smokers$PersonYears

str(smokers)



#1a
smokers.fit<- glm(Deaths ~ age+smoker, offset = log(PersonYears),
	family=poisson, data=smokers) 
 
plot( ( smokers.fit$linear.predictors[1:5] )  ,  ylim=c(1.5,6) )
points((smokers.fit$linear.predictors[6:10]))
 
plot( (smokers.fit$fitted.values )   )

 


summary(smokers.fit)

1-pchisq(12.13, 7)

#1b

 
smokersQI.fit<- glm(Deaths ~ ageQI*smoker, offset = log(PersonYears),
	family=poisson, data=smokers) 

summary(smokersQI.fit)
 1-pchisq(59.9 , 6)

plot( ( smokersQI.fit$linear.predictors[1:5] )  ,  ylim=c(1.5,6) )
points((smokersQI.fit$linear.predictors[6:10]))

################################################
################################################
################################################


dat<-read.csv("sex.csv")

dat<-data.frame(
	  (rep(dat$Response,2)),
	c(dat$Female,dat$Male),
	as.factor(c(rep("0",nrow(dat)),rep("1",nrow(dat))))	)

names(dat)<-c("response","counts","gender")
str(dat)
tail(dat)
dat.fit<-glm(response ~ gender, family=poisson, weights=counts, data=dat) 
summary(dat.fit)
 
n = by(dat$counts, dat$gender, sum)

dat


sum( tab[,2]* tab[,1])   / ( sum(tab[,2])  )
sum( tab[,2]*((tab[,1]- 4.183871 )^2) ) / ( sum(tab[,2]) -1)



sum(tab[,2])
tab$PestF<-round(dpois(tab$response ,exp(1.45936 ))*310,2)
tab$PestM<-round(dpois(tab$response ,exp(1.45936+ 0.30850 ))*sum(tab[,3]),2)

#2c
library(MASS)
nb.fit<-glm.nb(response ~ gender, weights=counts, data=dat)
summary(nb.fit)
summary(dat.fit)

UGdat<-as.data.frame(lapply(dat, function(x,p) rep(x,p), dat[["counts"]]))
hist(UGdat[,1], breaks = seq(0,60,by=1))

tab$PestF<-round(,2)
tab$PestM<-round(dpois(tab$response ,exp(1.45936+ 0.30850 ))*sum(tab[,3]),2)


muhat = unique(nb.fit$fitted.values)
dnbinom(tab$response,size = nb.fit$theta, mu = muhat[2]) *310

#2d
require("pscl")
UGdat<-as.data.frame(lapply(dat, function(x,p) rep(x,p), dat[["counts"]]))
fit.zip = zeroinfl(response ~ gender| 1 ,data=UGdat)

summary(fit.zip )
#mixing paramter
as.numeric( exp(coef(fit.zip)[3])/(1+exp(coef(fit.zip)[3])) )
1-as.numeric( exp(coef(fit.zip)[3])/(1+exp(coef(fit.zip)[3])) )


fit.zinb = zeroinfl(response ~ gender| 1 ,dist="negbin",data=UGdat)


#sample mean men
 
sum( dat[which(dat[,3]=="M"),1]*dat[which(dat[,3]=="M"),2] ) / 
sum( dat[which(dat[,3]=="M"),2] )


sum(dat[which(dat[,3]=="M"),2]*(dat[which(dat[,3]=="M"),1]-5.858333)^2) / 
sum( dat[which(dat[,3]=="M"),2] )-1
