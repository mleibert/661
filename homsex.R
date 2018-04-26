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

nfemale.poi = round(dpois(unique(dat[,1]), lambda=sex.muhat[1])*sex.n[1],3)
nmale.poi = round(dpois(unique(dat[,1]), lambda=sex.muhat[2])*sex.n[2],3)


nwhite.negb = round(dnbinom(0:6, size=hom.nb$theta, 
	mu=hom.muhat[2])*hom.n[1],3)
nblack.negb = round(dnbinom(0:6, size=hom.nb$theta,
	mu=hom.muhat[1])*hom.n[2],3)


nfemale.negb = round(dnbinom(unique(dat[,1]), size=sex.nb$theta,
	mu=sex.muhat[1])*sex.n[1],3)
nmale.negb = round(dnbinom(unique(dat[,1]), size=sex.nb$theta, 
	mu=sex.muhat[2])*sex.n[2],3)





