#### HMSC 1 ######
rm(list = ls())
library(devtools)
library(Rcpp)
library(RcppArmadillo)
library(coda)
library(abind)
library(ape)
library(fields)
library(spam)
library(dotCall64)
library(grid)
library(MCMCpack)
library(MASS)
library(mvtnorm)
library(nnet)
library(parallel)
library(phytools)
library(tensorA)
library(caTools)
library(HMSC)
set.seed(1)

######## SECTION 0 - PRELIMINARIES #######################

##Load the data 
load(file = "Mammal_train.RData")
load(file = "Mammal_validation.RData")


#===========================================
# SECTION 1- MODEL FITTING
#===========================================
#Build the model
model <- hmsc(HMSC.f5, family = "probit", niter = 10000, 
              nburn = 1000, thin = 10)

save(model, file = "HMSC1_train.RData")

load(file = "HMSC1_train.RData")

##Results paramX

#MCMC trace/mixing plots
mixing <- as.mcmc(model, parameters = "paramX")
par(mar = c(1,1,1,1))
plot(mixing)


#Constructing posterior summaries
mixing.x.df <- as.data.frame(mixing)

#draw boxplot for each parameter
par(mar = c(7,4,4,2))
boxplot(mixing.x.df)

## Draw beanplots 
library(beanplot)
beanplot(mixing.x.df, las = 2)

#Average 
average.x <- apply(model$results$estimation$paramX,1:2, mean)

## 95% confidence intervals
CI.025<-apply(model$results$estimation$paramX,1:2,quantile, probs=0.025)
CI.975<-apply(model$results$estimation$paramX,1:2,quantile, probs=0.975)
CI<-cbind(as.vector(CI.025),as.vector(CI.975)) 

#rows 57-285 shows whether the probability
#occurrence of a species increases or increases with increasing value of a given
#environmental covariate 

### Draw confidence interval plots
plot(0,0,xlim=c(1,nrow(CI)),ylim=range(CI),type="n",xlab="",ylab="",main="paramX")
abline(h=0,col="grey")
arrows(x0=1:nrow(CI),x1=1:nrow(CI),y0=CI[,1],y1=CI[,2],code=3,angle=90,length=0.05)
points(1:nrow(CI),average.x,pch=15,cex=1.5)

### Results paramTr

### Constructing MCMC trace/mixing plots
#============================================
### Mixing object

mixing.tr <- as.mcmc(model, parameters = "paramTr")
par(mar = c(1,1,1,1))
plot(mixing.tr)

#Constructing posterior summaries
mixing.tr.df <- as.data.frame(mixing.tr)

#Boxplots for each parameter
par(mar=c(7,4,4,2))
boxplot(mixing.tr.df,las=2)

### Draw beanplots
library(beanplot)
par(mar=c(7,4,4,2))
beanplot(mixing.tr.df,las=2)

### Average
average.tr <-apply(model$results$estimation$paramTr,1:2,mean)

### 95% confidence intervals
CI.025.tr <-apply(model$results$estimation$paramTr,1:2,quantile, probs=0.025)
CI.975.tr <-apply(model$results$estimation$paramTr,1:2,quantile, probs=0.975)
CI.tr <-cbind(as.vector(CI.025),as.vector(CI.975))

### Draw confidence interval plots
plot(0,0,xlim=c(1,nrow(CI)),ylim=range(CI),type="n",xlab="",ylab="",main="paramTr")
abline(h=0,col="grey")
arrows(x0=1:nrow(CI),x1=1:nrow(CI),y0=CI[,1],y1=CI[,2],code=3,angle=90,length=0.05)
points(1:nrow(CI),average.tr,pch=15,cex=1.5)

##### Plotting association networks
corMat <- corRandomEff(model, cor = FALSE)

#Autocorrelated latent variables
##Isolate valies of interest

ltri <- lower.tri(apply(corMat[,,,1], 1:2, quantile, probs = 0.025), diag = TRUE)

##Average
average.cor <- as.vector(apply(corMat[,,,1],1:2,mean)[ltri])

## 95% confidence intervals
corMat.025<-as.vector(apply(corMat[,,,1],1:2,quantile,probs=0.025)[ltri])
corMat.975<-as.vector(apply(corMat[,,,1],1:2,quantile,probs=0.975)[ltri])
CI.cor <-cbind(corMat.025,corMat.975)

### Plot the results
plot(0,0,xlim=c(1,nrow(CI.cor)),ylim=range(CI.cor),type="n",xlab="",,main="cov(paramLatentAuto[[1,1]])")
abline(h=0,col="grey")
arrows(x0=1:nrow(CI.cor),x1=1:nrow(CI.cor),y0=CI.cor[,1],
       y1=CI.cor[,2],code=3,angle=90,length=0.05)
points(1:nrow(CI.cor),average.cor,pch=15,cex=1.5)

## Mixing object 
mixing.auto <-as.mcmc(model,parameters="paramLatentAuto")
par(mar = c(1,1,1,1))
plot(mixing.auto)

#Convert mixing object to a matrix
mixing.auto.df <- as.data.frame(mixing.auto)

### Draw boxplot for each parameters
par(mar=c(7,4,4,2))
boxplot(mixing.auto.df,las=2)

### Draw beanplots
library(beanplot)
par(mar=c(7,4,4,2))
beanplot(mixing.auto.df,las=2)

### Draw estimated correlation matrix
library(corrplot)
corMat<-corRandomEff(model,cor=TRUE)
averageCor<-apply(corMat[,,,1],1:2,mean)
corrplot(averageCor,method="color",col=colorRampPalette(c("blue","white","red"))(200))


### Draw chord diagram
library(circlize)
corMat<-corRandomEff(model,cor=TRUE)
averageCor<-apply(corMat[,,,1],1:2,mean)
colMat<-matrix(NA,nrow=nrow(averageCor),ncol=ncol(averageCor))
colMat[which(averageCor>0.4,arr.ind=TRUE)]<-"red"
colMat[which(averageCor< -0.4,arr.ind=TRUE)]<-"blue"
chordDiagram(averageCor,symmetric=TRUE,annotationTrack=c("name","grid"),grid.col="grey",col=colMat)
chordDiagram(averageCor,symmetric=TRUE,annotationTrack="grid" ,grid.col="grey",col=colMat)

#### Generating predictions for validation data and a plot with R2-values
pred.val.mam <- predict(model, newdata = HMSC.f5.val)

R2t <- Rsquared(model, averageSp=FALSE)
Ymean <- apply(model$data$Y,2,mean)
R2v <- Rsquared(model, averageSp = FALSE, newdata = HMSC.f5.val)
plot(Ymean,R2t,pch=19,
     main=paste("Mean R2 value over species for training ",
                signif(mean(R2t),2),", and for validation ",signif(mean(R2v),2),sep=""))