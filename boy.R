delinquent = data.frame(ses = as.factor(rep(c("low", "medium", "high"),rep(2,3))),
boy = as.factor(rep(c("scout", "nonscout"),3)),
y = c(11, 42, 14, 20, 8, 2), n = c(54, 211, 118, 152, 204, 61))


delinquent$boy = relevel(delinquent$boy, ref="nonscout")
delinquent$ses = relevel(delinquent$ses, ref="low")
fit.null = glm(y/n ~ 1, weights=n, family=binomial, data=delinquent)
fit.boy = glm(y/n ~ boy, weights=n, family=binomial, data=delinquent)
fit.ses = glm(y/n ~ ses, weights=n, family=binomial, data=delinquent)
fit.boyses = glm(y/n ~ ses+boy, weights=n, family=binomial, data=delinquent)
fit.saturate = glm(y/n ~ boy*ses, weights=n, family=binomial, data=delinquent)

summary(fit.saturate )

2* ( logLik(fit.saturate ) -  logLik(fit.null ) )
2* ( logLik(fit.saturate ) -  logLik(fit.boy ) )
2* ( logLik(fit.saturate ) -  logLik(fit.ses ) )
2* ( logLik(fit.saturate ) -  logLik(fit.boyses ) )
2* ( logLik(fit.saturate ) -  logLik(fit.saturate ) )

1-pchisq( 0,0)
1-pchisq( 0.1542901,2)

boyG2<-c(36.41467,28.8021,0.1623331,0.1542901, 0)
boydf<-c(5,4,3,2,0)

1-pchisq(boyG2,boydf)


