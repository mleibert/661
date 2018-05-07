dat<-data.frame(t(rep(1,5)))

dat[1,]<-c(1,1,88,16,2)
dat[2,]<-c(1,0,54,7,5)
dat[3,]<-c(0,1,397,141,24)
dat[4,]<-c(0,0,235,189,39)
names(dat)<-c("race" ,"gender", "y1", "y2", "y3")


dat
dat.bf<-data.frame(rep(1,sum(dat[1,3:5])),rep(1,sum(dat[1,3:5])),
                      rep(NA,sum(dat[1,3:5])))
dat.bf[,3]<-c(rep(1,88),rep(2,16),rep(3,2))
names(dat.bf)<-letters[1:3]

dat.bm<-data.frame(rep(1,sum(dat[2,3:5])),rep(0,sum(dat[2,3:5])),
                   rep(NA,sum(dat[2,3:5])))
dat.bm[,3]<-c(rep(1,54),rep(2,7),rep(3,5))
names(dat.bm)<-letters[1:3]

dat.wf<-data.frame(rep(0,sum(dat[3,3:5])),rep(1,sum(dat[3,3:5])),
                   rep(NA,sum(dat[3,3:5])))
dat.wf[,3]<-c(rep(1,397),rep(2,141),rep(3,24))
names(dat.wf)<-letters[1:3]

dat.wm<-data.frame(rep(0,sum(dat[4,3:5])),rep(0,sum(dat[4,3:5])),
                   rep(NA,sum(dat[4,3:5])))
dat.wm[,3]<-c(rep(1,235),rep(2,189),rep(3,39))
names(dat.wm)<-letters[1:3]

 
1-pchisq(1864,2390)
1-pchisq(9.254164,4)

dat.ungrp<-rbind(dat.bf,dat.bm,dat.wf,dat.wm)
names(dat.ungrp)<-c("race","gender","y")




datt<-data.frame( c(rep(1,6),rep(0,6)),c(rep(1,3),rep(0,3),rep(1,3),rep(0,3)))
names(datt)<-c("race","gender" )
datt$count<-as.numeric( c(dat[1,3:5],dat[2,3:5],dat[3,3:5] ,dat[4,3:5])  )
datt$y<-rep(1:3,4)

library(VGAM)

dat
datt<-dat
datt$gender<-as.factor(c("F","M","F","M"))

 
fit.fit<-vglm(cbind(y1,y2,y3) ~ gender+race, 
	family=cumulative(parallel=T),data=dat) 
summary(fit.fit)

vglm(cbind(y1,y2,y3) ~ gender+race, family=cumulative(parallel=T),data=datt) 


(vglm( y ~ gender+race, family=cumulative(parallel=T), data=dat.ungrp))
vglm(cbind(y1,y2,y3) ~ gender+race, family=cumulative(parallel=T),data=dat) 
polr(as.factor(y) ~ gender+race, weights = count ,  data=datt  )

df.residual(polr(as.factor(y) ~ gender+race, weights = count ,  data=datt) )



exp(3.0919441) / (1 +exp(3.0919441))
exp(3.0919441  -0.7695648) / (1 +exp(3.0919441  -0.7695648))
