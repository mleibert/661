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

