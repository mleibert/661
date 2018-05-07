rm(list = ls())
setwd("G:\\math\\661")
options(scipen=999)
require(ggplot2)
library(MASS)
source("multiplot.R") 


plants<-read.table("Galapagos.txt",header=T)
plant<-read.table("Galapagos.txt",header=T)

tail(plants)
plants<-plants[,c(which(names(plants) %in% c("species","area", "elevation", 
	"nearest",	"scruz", "adjacent")))]


plants[plants== 0] <- .1
plants<-log(plants)

plants$species<-plant$species

plants.pois<-glm( species ~ . , family=poisson, data=plants)
summary(plants.pois)

1-pchisq( 359.52  , 24  )
anova(plants.pois, test="Chisq")


plot(plants.pois$linear.predictors, rstandard(plants.pois))
abline(h=2, col="orange",lwd =2);abline(h=3, col="red",lwd =2)
plot(plants.pois$linear.predictors, abs(rstandard(plants.pois)))


#the standardized residuals ri ~ N(0,1),absolute values of the ri's 
#larger than about 2 or 3 provide evidence of lack of fit.


1-pchisq(359.52  , 24 )

plants.nb = glm.nb(species ~ ., data=plants)
summary(plants.nb )

1-pchisq(149.432  , 24 )
 

library(ggcorrplot)
plant.cor<-cor(plants[,-1])
cor(plants[,-1])
ggcorrplot(plant.cor, type = "lower",lab = TRUE,
	colors = c("#6D9EC1", "white", "#E46726"))
 
fit.quasi = glm(species ~  area+ elevation +  nearest + scruz + adjacent , 
	data=plants,  family=quasi(link="log", variance="mu^2"))

summary(fit.quasi)
ncol(plants)
ncol(plant)

rm(plant);
plant<-read.table("Galapagos.txt",header=T)
plant[plant== 0] <- .1
plant<-plant[,-1]
plant[, -1]<-log(plant[,-1])
head(plant,3)
pois.null<-glm(species ~ 1,family=poisson, data=plant)
pois.sat<-glm(species ~ . , family=poisson, data=plant)
step.pois=step(pois.null, scope=list(lower=pois.null, 
	upper=pois.sat), direction="both", trace = 0  )

step.pois$linear.predictors 

nb.null<-glm.nb(species ~ 1, data=plant)
nb.sat<-glm.nb(species ~ ., data=plant, maxit = 1000)
step(nb.null, scope=list(lower=nb.null, 	upper=nb.sat), 
	direction="both",trace=0  )
step.nb<-step(nb.null, scope=list(lower=nb.null, 	
upper=nb.sat), direction="both",trace=0  )
summary(step.nb)



plot(step.nb$linear.predictors, rstandard(step.nb),ylim=c(-13,13))
abline(h=2, col="red",lwd =2);abline(h=-2, col="red",lwd =2)



