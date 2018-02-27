source("pmatrix.R")
uffi <- "1j0XnC0ufmHay_vICOvvdVjaXgLAf1WCd"
uffi <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
	uffi ))

head(uffi)
names(uffi)<-c("Y","X1","X2")

uffi.n<-nrow(uffi)
uffi.Y<-as.matrix(uffi[,1])
uffi.X<-matrix(NA,uffi.n,ncol(uffi))
uffi.X[,1]<-1;uffi.X[,2:3]<-as.matrix(uffi[,c(2,3)])
#pmatrix(uffi.X,digits=0)
uffi.b<- solve( t(uffi.X) %*% uffi.X ) %*% t(uffi.X) %*% uffi.Y

uffi.J<-matrix(1,uffi.n,uffi.n)
uffi.H<-uffi.X%*%solve( t(uffi.X) %*% uffi.X ) %*% t(uffi.X)
uffi.Yhat<- uffi.X %*%  solve( t(uffi.X) %*% uffi.X ) %*% t(uffi.X) %*% uffi.Y
uffi.e<-uffi.Y-uffi.Yhat

uffi.H%*%uffi.Y
uffi.p<-dim(uffi.X)[2]-1

sum(uffi.e)	#0

uffi.SSE<-t(uffi.e)%*%(uffi.e)
uffi.SSR<-t(uffi.b)%*%t(uffi.X)%*%uffi.Y - (1/uffi.n)*t(uffi.Y) %*% uffi.J %*% uffi.Y
uffi.SSTO<-t( uffi.Y ) %*% uffi.Y -  (1/uffi.n)*t(uffi.Y) %*% uffi.J %*% uffi.Y
uffi.MSR<-uffi.SSR/uffi.p
uffi.MSE<-uffi.SSE/(uffi.n-uffi.p-1)

uffi.MSR/uffi.MSE

(1/(uffi.n-(uffi.p+1))) * t(uffi.Y-uffi.X%*%uffi.b) %*%  (uffi.Y-uffi.X%*%uffi.b)

uffi.lm<-lm(Y~.,data=uffi)
anova(uffi.lm)
aov(uffi.lm)
summary(uffi.lm)

sqrt(diag(solve(t(uffi.X) %*% uffi.X) * as.numeric(uffi.MSE)))


#H0: b2 = 0

9.312042 / sqrt(4.5477672)

qt( , 21)


sum( as.numeric( anova( lm(Y~. , data=uffi)  )[[2]][-dim(uffi.X)[2]] ) )
uffi.SSR

uffi.SSTO
uffi.SSR+uffi.SSE

#F test

uffi.MSE<-( uffi.SSE / (uffi.n-uffi.p-1) )

uffi.F <-  ( uffi.SSR / uffi.p  ) / ( uffi.SSE / (uffi.n-uffi.p-1) )
summary( lm(Y~. , data=uffi)  ); uffi.F

as.numeric( uffi.MSE ) * solve( t(uffi.X) %*% uffi.X)


