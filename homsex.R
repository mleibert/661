homicide = data.frame(response=rep(0:6,each=2), 
	race=rep(c("black", "white"), 7),
	counts=c(119, 1070, 16, 60, 12, 14, 7, 4, 3, 0, 2, 0, 0, 1))
str(homicide)

homicide$race = relevel(homicide$race, ref="white")

dat<-read.csv("sex.csv")
dat<-data.frame(	  (rep(dat$Response,2)),	c(dat$Female,dat$Male),
	as.factor(c(rep("0",nrow(dat)),rep("1",nrow(dat)))),
	as.factor(c(rep("F",nrow(dat)),rep("M",nrow(dat))))	)
names(dat)<-c("response","counts","gender")
str(dat )


##############


tail(dat)
tail(homicide )

hom.pois = glm(response ~ race, family=poisson, weights=counts, data=homicide)
sex.pois = glm(response ~ gender, family=poisson, weights=counts, data=dat)

hom.nb = glm.nb(response ~ race, weights=counts, data=homicide)
sex.nb = glm.nb(response ~ gender, weights=counts, data=dat)

hom.muhat = unique(hom.nb$fitted.values)
sex.muhat = unique(sex.nb$fitted.values)

hom.n = by(homicide$counts, homicide$race, sum)
sex.n = by(dat$counts, dat$gender, sum)

nwhite.poi = round(dpois(0:6, lambda=hom.muhat[2])*hom.n[1],3)
nblack.poi = round(dpois(0:6, lambda=hom.muhat[1])*hom.n[2],3)

nfemale.poi = dpois(unique(dat[,1]), lambda=sex.muhat[1])*sex.n[1]
nmale.poi = dpois(unique(dat[,1]), lambda=sex.muhat[2])*sex.n[2]



nwhite.negb = round(dnbinom(0:6, size=hom.nb$theta, 
	mu=hom.muhat[2])*hom.n[1],3)
nblack.negb = round(dnbinom(0:6, size=hom.nb$theta,
	mu=hom.muhat[1])*hom.n[2],3)


nfemale.negb = round(dnbinom(unique(dat[,1]), size=sex.nb$theta,
	mu=sex.muhat[1])*sex.n[1],3)
nmale.negb = round(dnbinom(unique(dat[,1]), size=sex.nb$theta, 
	mu=sex.muhat[2])*sex.n[2],3)



#########

tab<-cbind(dat[which(dat$gender == 0),],dat[which(dat$gender == 1 ),-1])
tab<-tab[,c(1,2,5)] 
 names(tab)[2:3]<-c("Female","Male");head(tab) 

tab$poiF<-round( nfemale.poi  ,3)
tab$poiM<-round( nmale.poi ,3)

tab$negbF<-  nfemale.negb 
tab$negbM<-  nmale.negb 



summary(fit.zip )
phi=as.numeric( exp(coef(fit.zip)[3])/(1+exp(coef(fit.zip)[3])) )

sex.n[1] * (     phi + (1-phi)  * dpois(0,exp(1.99107))    )
sex.n[2] * (     phi + (1-phi)  * dpois(0,exp(1.99107 + 0.09242))    )

tab$zipF<- round(c(  as.numeric(
	sex.n[1] * (     phi + (1-phi)  * dpois(0,exp(1.99107)) )    ),
	sex.n[1]*( (1-phi)* dpois(unique(dat[,1])[-1],exp(1.99107)) ) ) ,3) 

tab$zipM<-round(c(  as.numeric(
	sex.n[2] * (     phi + (1-phi)  * dpois(0,exp(1.99107 + 0.09242)) )),
	sex.n[2]*((1-phi)*dpois(unique(dat[,1])[-1],exp(1.99107 + 0.09242)))),3)


summary(fit.zinb )

gam<-1/exp(fit.zinb$theta)
phi=as.numeric( exp(coef(fit.zinb)[3])/(1+exp(coef(fit.zinb)[3])) )

sex.n[1]*(  phi + (1-phi) * dnbinom(0,mu=exp(1.89133),  exp(0.43572))  )
sex.n[2]*(  phi + (1-phi) * dnbinom(0,mu=exp(1.89133+0.14584), exp(0.43572)))

sex.n[1]* ((1-phi) * dnbinom(unique(dat[,1])[-1], mu=exp(1.89133),
	exp(0.43572))   )
sex.n[2]* ((1-phi) * dnbinom(unique(dat[,1])[-1], mu=exp(1.89133 +  0.14584),
	exp(0.43572))   )

tab$zinbF<-round(c( as.numeric(
	sex.n[1]*(  phi + (1-phi) * dnbinom(0,mu=exp(1.89133), exp(0.43572)))),
	sex.n[1]* ((1-phi) * dnbinom(unique(dat[,1])[-1], mu=exp(1.89133),
	exp(0.43572))   )	),3)

tab$zinbM<-round(c( as.numeric(
	sex.n[2]*(  phi + (1-phi) * dnbinom(0,mu=exp(1.89133+0.14584),
		exp(0.43572))) ),
	sex.n[2]* ((1-phi) * dnbinom(unique(dat[,1])[-1], mu=exp(1.89133 +  
		0.14584),exp(0.43572))   )),3)

print(tab,row.names = F)



