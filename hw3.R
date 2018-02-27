setwd("G:\\math\\661")

 
 
crabs<- read.table("crabs.txt",header=T)

#CRABS<-crabs;CRABS$y<-ifelse(CRABS$satellite > 0, 1,0)
#CRABS$color<-as.factor(CRABS$color)

crabs<-CRABS

crabs$y<-ifelse(crabs$satellite > 0, 1,0)
crabs<-crabs[,c(2,4,ncol(crabs))]


head(crabs);head(CRABS);str(CRABS);str(crabs)
crabs.glm<-glm(y~. ,data=crabs,family="binomial")

summary(crabs.glm) 
summary(glm(y~color+weight ,data=CRABS,family="binomial"))$coef

crabsI.glm<-glm(y~color+weight+color*weight,data=crabs,family="binomial")
summary(crabsI.glm)



crabss<-crabs
crabss$bleh<-runif(nrow(crabss),-10,10)
summary(glm(y~color+weight ,data=crabss,family="binomial"))$coef




donner<- read.csv("G:\\661\\Donner.csv",header=T)
