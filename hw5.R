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
names(smoke)<-c(paste0("PY",c("Nonsmokers", "Smokers")),
	paste0("PY",c("Nonsmokers", "Smokers")))



smoke$age<-as.factor(1:nrow(smoke))

cancer<-read.table("Cancer.txt",header=T)
logrisktime <- log(cancer$risktime)
fit <- glm(count ~ factor(histology) + factor(stage) + factor(time),
	family = poisson(link = log), offset = logrisktime,data=cancer)