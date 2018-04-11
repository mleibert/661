rm(list = ls())
setwd("G:\\math\\661")
options(scipen=999)
library("VGAM")


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

predict(mental.fit,newdata=men, type=("response"))

mat<-matrix(NA,4,5)
mat[1,]<-c(1,1,88,16,2)
mat[2,]<-c(1, 0, 54, 7 ,5)
mat[3,]<-c(0 ,1, 397 ,141 ,24)
mat[4,]<-c(0 ,0 ,235, 189, 39)
mat


#########
gender<-c("female","male")
race<-c("black","white")
heaven<-c( "yes" , "unsure" , "no" )
mylist<-list()


for( i in 1:4){

	mylist[[i]]<-data.frame( rep( race[ mat[i,1] ], sum(mat[i,-c(1,2)]) ) , 
		rep( gender[ mat[i,1] ], sum(mat[i,-c(1,2)]) ) ) 
	mylist[[i]]$y<-c( rep( heaven[1], mat[i,3]  ) ,
		 rep( heaven[2], mat[i,4]  ), rep( heaven[3], mat[i,5]  ) )
	names(mylist[[i]])[1:2]<-c("race","gender") }
heaven<-do.call("rbind", mylist)

tail(heaven)
str(heaven)
heaven$y<-as.factor(heaven$y)

heaven$y<-as.numeric(heaven$y)
heaven[,1]<-as.numeric(heaven[,1])
heaven[,2]<-as.numeric(heaven[,2])

heaven[,1]<-ifelse(heaven[,1] == 2, heaven[,1]<-0,heaven[,1]<-1)
heaven[,2]<-ifelse(heaven[,2] == 2, heaven[,2]<-0,heaven[,2]<-1)
############


mat<-as.data.frame(mat)
names(mat)<-c("race","gender","y1","y2","y3")

heaven.fit = vglm(cbind(y1,y2,y3)~gender+race, 
	family=cumulative(parallel=T), data=mat)

summary(heaven.fit )










