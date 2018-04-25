homicide = data.frame(response=rep(0:6,each=2), race=rep(c("black", "white"), 
	7),counts=c(119, 1070, 16, 60, 12, 14, 7, 4, 3, 0, 2, 0, 0, 1))

homicide$race = relevel(homicide$race, ref="white")



bh<-homicide[which(homicide$race == "black"),]
sum( bh[,3]*((bh[,1]-.522)^2) ) / ( sum(bh[,3]) )

bh<-homicide[which(homicide$race == "white"),]
sum( bh[,3]*((bh[,1]-.092)^2) ) / ( sum(bh[,3]) )



hom.pois = glm(response ~ race, family=poisson, weights=counts, data=homicide)
hom.nb = glm.nb(response ~ race, weights=counts, data=homicide)
summary(hom.nb)

hom.muhat = unique(hom.nb$fitted.values)
hom.n = by(homicide$counts, homicide$race, sum)
nwhite.poi = round(dpois(0:6, lambda=hom.muhat [2])*hom.n[1],3)
nblack.poi = round(dpois(0:6, lambda=hom.muhat [1])*hom.n[2],3)
 

dpois(0:10,exp(-2.383 ))*1149

sum(homicide[which(homicide$race == "white"),3]*
homicide[which(homicide$race == "white"),1])/
sum(homicide[which(homicide$race == "white"),3])

fit.zip = zeroinfl(response ~ race | 1,weights=counts ,data=homicide)
