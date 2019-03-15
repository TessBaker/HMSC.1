library(ISLR)
attach(Smarket)
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

#Load data
mammals_presence_absence_IUCN <- read.csv("~/Desktop/HMSC.IUCN.data/IUCN.data.frames/mammals_presence_absence_IUCN.csv")
Mammal_trait_data <- read.csv("~/Desktop/HMSC.IUCN.data/IUCN.data.frames/Mammal_trait_data.csv")
Phylo_correlation_mat_th <- read.csv("~/Desktop/HMSC.IUCN.data/IUCN.data.frames/Phylo_correlation_mat_th.txt", sep="")
Phylo_correlation <- as.matrix(Phylo_correlation_mat_th)

#Subset trait data to binomial and body mass
Trait.binomial <- cbind(Mammal_trait_data[2], Mammal_trait_data[13])
Trait <- t(Trait.binomial[2])

#Split into train/test (70% train; 30% test)
smp_size <- floor(0.7*nrow(mammals_presence_absence_IUCN))
train_ind <- sample(seq_len(nrow(mammals_presence_absence_IUCN)), size = smp_size)
train <- mammals_presence_absence_IUCN[train_ind,]
test <- mammals_presence_absence_IUCN[-train_ind,]

#Format into HMSC object
X.train <- cbind(train[59:60], train[62], train[70], train[73])
X.test <- cbind(test[59:60], test[62], test[70], test[73])

Y.train <- as.matrix(train[2:58]) 
Y.test <- as.matrix(test[2:58])

Auto.train <- train[78:79]
Auto.train <- as.factor(Auto.train)
Auto.test <- test[78:79]
Auto.test <- as.factor(Auto.test)

#Create HMSC object

HMSC.dat.train <- as.HMSCdata(Y = Y.train, X = X.train, Tr = Trait,
                              Phylo = Phylo_correlation)

save(HMSC.dat.train, file = "HMSC_data_train.RData")

HMSC.dat.test <- as.HMSCdata(Y = Y.test, X = X.test, Tr = Trait, 
                             Phylo = Phylo_correlation)

save(HMSC.dat.test, file = "HMSC_data_test.RData")

