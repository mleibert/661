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



dat<-read.csv("sex.csv")

dat<-data.frame(
	  (rep(dat$Response,2)),
	c(dat$Female,dat$Male),
	as.factor(c(rep("F",nrow(dat)),rep("M",nrow(dat))))	)

names(dat)<-c("response","counts","gender")
str(dat)
tail(dat)
dat.fit<-glm(response ~ gender, family=poisson, weights=counts, data=dat) 
summary(dat.fit)

dpois(0:10,exp(1.45936 ))*310


n = by(dat$counts, dat$gender, sum)

dat


  
tab<-cbind(dat[which(dat$gender == "F" ),],dat[which(dat$gender == "M" ),-1])
tab<-tab[,c(1,2,4)]


tab[19,2]<-sum(tab[19:nrow(tab),2])
tab[19,3]<-sum(tab[19:nrow(tab),3])
tab<-tab[1:19,]
tab 
names(tab)[2:3]<-c("Female","Male")


sum(tab[,2])
tab$PestF<-round(dpois(tab$response ,exp(1.45936 ))*310,2)
tab$PestM<-round(dpois(tab$response ,exp(1.45936+ 0.30850 ))*sum(tab[,3]),2)

#2c
library(MASS)
nb.fit<-glm.nb(response ~ gender  , weights=counts, data=dat)
summary(nb.fit)
summary(dat.fit)

library(VGAM)
 


#2d 		#	install.packages("pscl")

library(pscl)
fit.zip = 
zeroinfl(response ~ gender |1  ,  weights=counts,	  data=dat)


fish<-read.table("fish.txt",header=T)
tail(fish)


#2f
tab$PestF<-round(,2)
tab$PestM<-round(dpois(tab$response ,exp(1.45936+ 0.30850 ))*sum(tab[,3]),2)


muhat = unique(nb.fit$fitted.values)
dnbinom(tab$response,size = nb.fit$theta, mu = muhat[2]) *310





#sample mean men
 
sum( dat[which(dat[,3]=="M"),1]*dat[which(dat[,3]=="M"),2] ) / 
sum( dat[which(dat[,3]=="M"),2] )


sum(dat[which(dat[,3]=="M"),2]*(dat[which(dat[,3]=="M"),1]-5.858333)^2) / 
sum( dat[which(dat[,3]=="M"),2] )-1
