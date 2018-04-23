homicide = data.frame(response=rep(0:6,each=2), race=rep(c("black", "white"), 
	7),counts=c(119, 1070, 16, 60, 12, 14, 7, 4, 3, 0, 2, 0, 0, 1))

homicide$race = relevel(homicide$race, ref="white")

hom.pois = glm(response ~ race, family=poisson, weights=counts, data=homicide)


muhat = unique(hom.nb$fitted.values)
n = by(homicide$counts, homicide$race, sum)
nwhite.poi = round(dpois(0:6, lambda=muhat[2])*n[1],3)
nblack.poi = round(dpois(0:6, lambda=muhat[1])*n[2],3)
n = by(homicide$counts, homicide$race, sum)

dpois(0:10,exp(-2.383 ))*1149

