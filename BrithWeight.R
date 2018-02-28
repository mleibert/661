
BrithWeight<-read.table("birthweight.txt",header=T)
attach(BrithWeight)
bwt.logit = glm(low ~ lwt+smoke+as.factor(race)+ptl+ht, family="binomial")
summary(bwt.logit)
detach(BrithWeight)
head(BrithWeight)
library(ResourceSelection)
res = hoslem.test(bwt.logit$y, fitted(bwt.logit))


0.8448629-0.03503