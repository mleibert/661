######### compare nested models

delinquent = data.frame(ses = as.factor(rep(c("low", "medium", "high"),
	rep(2,3))),boy = as.factor(rep(c("scout", "nonscout"),3)),
	y = c(11, 42, 14, 20, 8, 2), n = c(54, 211, 118, 152, 204, 61))

# R uses the first levels as reference.
# To use boy="non-scouts" and ses="low" as reference

delinquent$boy = relevel(delinquent$boy, ref="nonscout")
delinquent$ses = relevel(delinquent$ses, ref="low")
fit.null = glm(y/n ~ 1, weights=n, family=binomial, data=delinquent)
fit.boy = glm(y/n ~ boy, weights=n, family=binomial, data=delinquent)
fit.ses = glm(y/n ~ ses, weights=n, family=binomial, data=delinquent)
fit.boyses = glm(y/n ~ ses+boy, weights=n, family=binomial, data=delinquent)
fit.saturate = glm(y/n ~ boy*ses, weights=n, family=binomial, data=delinquent)

#slide 10
#S+B v S*B
summary(fit.saturate)
summary(fit.boyses )
2-0

#S vs S+B
summary(fit.boyses )
summary(fit.ses)
3-2

#B vs S+B
summary(fit.boyses )
summary(fit.boy)
4-2

#null vs S
summary(fit.ses)
summary(fit.null)
