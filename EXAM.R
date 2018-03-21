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
 
 

fit.null<-glm(default~1,family=binomial, data=dat)
fit.sat<-glm(default~.*.,family=binomial, data=dat)	

summary(fit.sat)

  


step(fit.null, scope=list(lower=fit.null, 
	upper=fit.sat), direction="both" )

step(fit.sat, scope=list(lower=fit.null, 
	upper=fit.sat), direction="both" )


 glm(formula = default ~ debtinc + employ + creddebt + address + 
    age, family = binomial, data = dat)

