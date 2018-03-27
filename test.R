2*(1-pnorm( 2))



-2* (-114.14+113.83 )
1-pchisq(-2* (-114.14+113.83 ), 2)


exp( -.82-1.96*0.41)
exp(-.82+1.96*0.41)



 (-.82)^2  / (.41)^2

1-pchisq(4,1)

library(aod)
library(ggplot2)


mydata <- read.csv("g:\\math\\661\\binary.csv")
head(mydata)
mydata$rank <- factor(mydata$rank)

my.mod <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(my.mod)



1-pchisq((-0.240)^2 ,1)


 .1/.97

2*(1-pnorm(.1/.97 ) )
1-pchisq(  (-.68)^2/(1.03)^2  ,1 )


#G^2 deviances

logL<-c(-116.54,-114.41,-116.27,-114.14,-113.83)
degF<-c(5,4,3,2,0)


2*(-113.83 - -116.54)
2*(-113.83 - -114.41)
2*(-113.83 - -116.27)
2*(-113.83 - -114.14)
2*(-113.83 - -113.83)



2*(-113.83 - logL)
1-pchisq( 2*(-113.83 - logL) ,  degF )
 
round( 1-pchisq( 2*(-113.83 - logL) ,  degF ) ,3 )
2*(-113.83 - -114.14)
1-pchisq(0.62, 2)

2*(-113.83 - -116.54)	#null
2*(-113.83 - -114.41)	#ASP
2*(-113.83 - -116.27)	#AGE
2*(-113.83 - -114.14)	#AGE+ASP
2*(-113.83 - -113.83)	#AGE*ASP
     


5.42-1.16
5.42-4.88
1.16-0.62
4.88-0.62
0.62

1-pchisq(5.42-1.16, 1)
1-pchisq(5.42-4.88, 2)
1-pchisq(1.16-0.62, 2)
1-pchisq(4.88-0.62, 1)
1-pchisq( 0.62, 2)

   AOD<-data.frame(
c(5.42-1.16,5.42-4.88,1.16-0.62,4.88-0.62,0.62),
c(1-pchisq(5.42-1.16, 1),1-pchisq(5.42-4.88, 2),1-pchisq(1.16-0.62, 2),1-pchisq(4.88-0.62, 1),1-pchisq( 0.62, 2))
)
names(AOD)<-c("dG","pvalue")
