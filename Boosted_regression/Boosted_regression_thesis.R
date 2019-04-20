#### Boosted Regression 

source("./Boosted.Regression/brt.functions.R")

library(gbm)

##Import Data 

data <- read.csv("./Boosted_regression_Lyme_data_updated_thesis.csv")

#The predictors
#Species(col 5: col 123), bio clim variable(col 125:142), urban/rural(col 153:174),
#Park(col 177), Education(col 180)

#Remove species which have no presence
species.dat <- data[5:123]

absent.species <- c()
for(i in 1:length(species.dat)){
  max.val = max(species.dat[i])
  if(max.val == 0) {
    absent.species <- c(absent.species, colnames(species.dat[i]))
  }
}

species <- species.dat[, !(colnames(species.dat) %in% absent.species)]

#Change any non-numeric to numeric (some were of class integer)
case.sums <- as.numeric(data$Case.sum)
data$Case.sum <- case.sums

pop.cou <- as.numeric(data$POP_COU)
pop.urban <- as.numeric(data$POP_URBAN)
area.urban <- as.numeric(data$AREA_URBAN)
pop.ua <- as.numeric(data$POP_UA)
area.ua <- as.numeric(data$AREA_UA)
pop.uc <- as.numeric(data$POP_UC)
area.uc <- as.numeric(data$AREA_UC)
pop.rural <- as.numeric(data$POP_RURAL)
percent.parks <- as.numeric(data$Percent.Pop.N.Park)

data$POP_COU <- pop.cou
data$POP_URBAN <- pop.urban
data$AREA_URBAN <- area.urban
data$POP_UA <- pop.ua
data$AREA_UA <- area.ua
data$POP_UC <- pop.uc
data$AREA_UC <- area.uc
data$POP_RURAL <- pop.rural
data$Percent.Pop.N.Park <- percent.parks


X <- cbind(species, data[125:142], data[153:174], data[177], data[180])

#"Bassariscus.astutus"
#[1] "Blarina.hylophaga"
#[1] "Chaetodipus.hispidus"
#[1] "Cynomys.ludovicianus"
#[1] "Dasypus.novemcinctus"
#[1] "Dipodomys.ordii"
#[1] "Lepus.californicus"
#[1] "Microtus.breweri"
#[1] "Myotis.austroriparius"
#[1] "Odocoileus.hemionus"
#[1] "Peromyscus.attwateri"
#[1] "Rangifer.tarandus"
#[1] "Reithrodontomys.fulvescens"
#[1] "Reithrodontomys.montanus"
#[1] "Sylvilagus.aquaticus"
#[1] "Tadarida.brasiliensis"
#[1] "Vulpes.lagopus"
#[1] "Vulpes.velox"



#The response 
#Lyme sum(col: 195)
Y <- data[195]

model.data <- cbind(X, Y)

## 
lyme.tc5.lr01 <- gbm.step(data = model.data, 
                          gbm.x = 1:143,
                          gmb.y = 144,
                          family = "poisson",
                          tree.complexity =5,
                          learning.rate = 0.001,
                          bag.function = 0.5)



##########
library(rsample)
library(gbm)
library(xgboost)
library(caret)
library(h2o)
library(pdp)
library(ggplot2)
library(lime)

set.seed(1)

#split into train / test set

sample.size <- floor(0.70*nrow(data))

train.ind <- sample(seq_len(nrow(data)), size = sample.size)

train.data <- data[train.ind, ]
test.data <- data[-train.ind, ]

model.data.2 <- cbind(X, Y)

#Remove zeros
model.data.2 <- model.data.2[!(model.data.2$Case.sum ==0),]

X.1 <- model.data.2[1:143]

Y.1 <- model.data.2[144]
case.sum.int <- as.integer(Y.1$Case.sum)
Y.1$Case.sum <- case.sum.int
Y.1 <- as.matrix(Y.1)

##
gbm.fit <- gbm(X.1, Y.1, 
               distribution = "poisson", 
               data = model.data.2,
               n.trees = 10000,
               interaction.depth = 1,
               shrinkage = 0.001,
               cv.fold = 5,
               n.cores = NULL,
               verbose = FALSE)






