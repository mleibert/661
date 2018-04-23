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


smoke$age<-as.numeric(smoke$age)
smoke$age<-as.factor(smoke$age)

smokers<-data.frame(rep(1:5,2),c(rep("NS",5),rep("S",5))	,
	c(smoke$PYnonsmokers,smoke$PYsmokers),
	c(smoke$CDnonsmokers,smoke$CDsmokers))

names(smokers)<-c("age","smoker","PersonYears","Deaths")
smokers$age<-as.factor(smokers$age)
smokers$ageQI<-as.numeric(smokers$age)

str(smokers)

#1a
smokers.fit<- glm(Deaths ~ age+smoker, offset = log(PersonYears),
	family=poisson, data=smokers) 
 
plot(smokers.fit$linear.predictors[1:5]  ,  ylim=c(1.5,6) )
points(smokers.fit$linear.predictors[6:10])

summary(smokers.fit)

1-pchisq(12.13, 7)

#1b

 
smokersQI.fit<- glm(Deaths ~ ageQI*smoker, offset = log(PersonYears),
	family=poisson, data=smokers) 

 1-pchisq(59.9 , 6)




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


data.frame(dat$response[1:19],
	length( dat[which(dat$gender == "F" & dat$response < 19),2] )


)


tab<-cbind(dat[which(dat$gender == "F" ),],dat[which(dat$gender == "M" ),-1])
tab<-tab[,c(1,2,4)]
tab 
names(tab)[2:3]<-c("Female","Male")

sum(dat[which(dat$gender == "F" & dat$response > 19),2])
sum(dat[which(dat$gender == "M" & dat$response > 19),2])
