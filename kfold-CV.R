
n = nrow(PimaIndiansDiabetes)  
nrep = 5        # repetitions of k-fold CV
kfolds = 10     # 10-fold CV
cv.pred <- matrix(NA, nrow=n, ncol=nrep)

for(j in 1:nrep) {
  folds.i <- sample(rep(1:kfolds, length= n))
  for (k in 1:kfolds) {
    test.i = which(folds.i == k)
    train.dat = PimaIndiansDiabetes[-test.i, ]
    test.dat = PimaIndiansDiabetes[test.i, ]
    fit.train = glm(diabetes ~ pregnant+glucose+mass, 
                    family="binomial", data=train.dat)
    cv.pred[test.i,j] = predict(fit.train, newdata=test.dat)
  }
}

library(ROCR)
pred = prediction(cv.pred, matrix(rep(PimaIndiansDiabetes$diabetes,nrep), 
                                  ncol=nrep))
perf = performance(pred, "tpr", "fpr")
plot(perf)
auc.perf = performance(pred, "auc")@y.values
unlist(auc.perf)
mean(unlist(auc.perf)); sd(unlist(auc.perf))

# To identify cutoff value
cuts = unlist(pred@cutoffs[[1]])
pi0 = exp(cuts)/(1+exp(cuts))
nDp = length(which(PimaIndiansDiabetes=="pos"))
nDn = length(which(PimaIndiansDiabetes=="neg"))
tpr = unlist(pred@tp[[1]])/nDp
fnr = unlist(pred@fn[[1]])/nDn
cuts.dat = cbind(pi0, tpr, fnr)
cuts.dat[tpr>0.8 & tpr < 0.9 & fnr > 0.1 & fnr <0.2, ]
