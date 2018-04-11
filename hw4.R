rm(list = ls())
setwd("G:\\math\\661")
options(scipen=999)
require("VGAM")
library(MASS)


cheese<-read.table("cheese.txt",header=T)
polr.fit = polr(as.factor(response) ~ type, weights=count, data=cheese)

cheese.ungrp = as.data.frame(
	lapply(cheese, function(x,p) rep(x,p), cheese$count))

 

tail(cheese);tail(cheese.ungrp )
cheese.ungrp <- cheese.ungrp[,-3] 

 

vglm.fit = vglm(response ~ type, 
	family=cumulative(parallel=T), data=cheese.ungrp)
summary(vglm.fit)

mental<-read.table("mental.txt",header=T)
mental<-mental[,c(1,3,2)]
men<-mental[2,]
men[,2]<-4.3

mental.fit<-vglm(impair~ses+life ,family=cumulative(parallel=T), data=mental)
summary(mental.fit)

predict(mental.fit,newdata=men, type=("response"))
men[,3]<-0

predict(mental.fit,newdata=data.frame(men), type="response")

####### 

mat<-matrix(NA,4,5)
mat[1,]<-c(1,1,88,16,2)
mat[2,]<-c(1, 0, 54, 7 ,5)
mat[3,]<-c(0 ,1, 397 ,141 ,24)
mat[4,]<-c(0 ,0 ,235, 189, 39)
mat

mat<-as.data.frame(mat)
names(mat)<-c("race","gender","y1","y2","y3")

heaven.fit = vglm(cbind(y1,y2,y3)~gender+race, 
	family=cumulative(parallel=T), data=mat)

summary(heaven.fit )

matt<-melt(mat,id.vars = c("race","gender"))
matt[,1]<-as.factor(matt[,1])
matt[,2]<-as.factor(matt[,2])

heaven.fitt = polr(variable ~ race+gender, weights=value, data=matt)
summary(heaven.fitt)


with(matt, levels(race))
with(matt, levels(gender ))
 
which( matt[,3] == 1)
levels(matt[,3] )
 
pmat<-mat[1,1:2] 
pmat[,1]<-1	#race 1=black
pmat[,2]<-1 #gender 1=female
pmat[2,]<-c(1,0)
pmat[3,]<-c(0,1)
pmat[4,]<-c(0,0)

predheaven<-predict( heaven.fit  , newdata= pmat, type="response")
summary(heaven.fitt)

B1=-1.0165;B2=-0.7696  
A1=0.0763;	A2=2.3224
exp(2.1093) / (1+exp(2.1093))
exp(A1-B1-B2) / (1+exp(A1-B1-B2))
exp(A1-B1-B2) / (1+exp(A1-B1-B2))
exp(A1-0) / (1+exp(A1-0))
exp(A2-0) / (1+exp(A2-0))

cumsum(predheaven[1,])
apply(predheaven ,1 , cumsum)



















