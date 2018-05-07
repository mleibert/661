loans<-read.table("bank_loan.txt",header=T)
head(loans)
rm(list = ls()); setwd("G:\\math\\661"); options(scipen=999); library(MASS)

plants<-read.table("Galapagos.txt",header=T)
plant<-read.table("Galapagos.txt",header=T)


head(plant)

#hist & boxplots

hist(plant$species)
summary(plant$species)
IQR(plant$species)
ggplot(plant, aes(x=species))+  
	geom_histogram(color="darkblue", fill="lightblue",bins=30)

dat<-plant[,c(which(names(plant) %in% c("species","area", "elevation", 
	"nearest",	"scruz", "adjacent")))]
dat[,1]<-log(dat[,1])


#summary stats for continous and categroical variables
plot(dat$species,dat$area)

gglist<-list()
for(i in 2:ncol(dat)){
	gglist[[i]] <- ggplot(dat, aes_string(x="species",y=colnames(dat)[i]
	 )) +	geom_point()	}



multiplot(gglist[[2]], gglist[[3]], gglist[[4]], gglist[[5]],gglist[[6]],
	 cols=2)

dat[,-1]<-log(dat[,-1])

for(i in 2:ncol(dat)){
	gglist[[5+i]] <- ggplot(dat, aes_string(x="species",y=colnames(dat)[i]
	 )) +	geom_point()	}

multiplot(gglist[[2+5]], gglist[[3+5]], gglist[[4+5]], gglist[[5+5]],
	gglist[[5+6]],	 cols=2)
