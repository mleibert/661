JENL<-rbind(
data.frame(smoker = rep("Non",4), "count"= c(158,9,5,0 ) , "level"= 1:4),
data.frame(smoker = rep("Ex",4), "count"=c(167 ,19 , 5, 3), "level"= 1:4),
data.frame(smoker = rep("Current",4),"count"= c(307,102,83 ,68),"level"= 1:4)
)
JENL$exposure<-"no"

JEYL<-rbind(
data.frame(smoker = rep("Non",4), "count"= c(26, 5 ,5, 1) , "level"= 1:4),
data.frame(smoker = rep("Ex",4), "count"=c(38, 12, 4, 4), "level"= 1:4),
data.frame(smoker = rep("Current",4),"count"= c(94, 48, 46, 60),"level"= 1:4)
)
JEYL$exposure<-"yes"




JEL<-rbind(JENL,JEYL)
JEL$pollution<-"low"

####

JENL<-rbind(
data.frame(smoker = rep("Non",4), "count"= c(94, 7, 5 ,1) , "level"= 1:4),
data.frame(smoker = rep("Ex",4), "count"=c(67, 8, 4 ,3), "level"= 1:4),
data.frame(smoker = rep("Current",4),"count"=c(184, 65, 33, 36),"level"= 1:4)
)
JENL$exposure<-"no"

JEYL<-rbind(
data.frame(smoker = rep("Non",4), "count"= c(32, 3, 6, 1) , "level"= 1:4),
data.frame(smoker = rep("Ex",4), "count"=c(39, 11, 4 ,2), "level"= 1:4),
data.frame(smoker = rep("Current",4),"count"= c(77, 48, 39, 51),"level"= 1:4)
)
JEYL$exposure<-"yes"



JEH<-rbind(JENL,JEYL)
JEH$pollution<-"high"

dat<-rbind(JEL,JEH)
dat$smoker<-relevel(as.factor(dat$smoker), ref="Non")
dat$exposure<-relevel(as.factor(dat$exposure), ref="no")
dat$pollution<-relevel(as.factor(dat$pollution), ref="low")
dat$level<-relevel(as.factor(dat$level), ref=1)

head(dat)
 
str(dat)

##########

cough<-data.frame( rep( unique(dat$smoker) ,4 ),
	rep( unique(dat$exposure) ,6 ),
	rep( unique(dat$pollution) ,6 ) )
names(cough)<-c("smoker","exposure","pollution")
cough<-cough[,c(3:1)]
cough 




#Fit a proportional odds cumulative logit model with pairwise interaction 
#efects for all covariates and assess its goodness of fit. 
#Use low air pollution, no job exposure and non-smoker as reference group.

head(dat)

cough.fit<-vglm(cbind(y1,y2,y3) ~ gender+race, family=cumulative(parallel=T))
































