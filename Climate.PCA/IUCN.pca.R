rm(list = ls())

library(devtools)
library(scales)
library(ggbiplot)
library(factoextra)
library(PerformanceAnalytics)

mammals_presence_absence_IUCN <- read.csv("~/Desktop/HMSC.IUCN.data/mammals_presence_absence_IUCN.csv")

X <- mammals_presence_absence_IUCN[59:77]

climate.pca <- prcomp(X, center = TRUE, scale = TRUE)
summary(climate.pca)
str(climate.pca)


##Biplots 

fviz_pca_var(climate.pca, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_eig(climate.pca)

climate.biplot <- ggbiplot(climate.pca)

#Correlation matrix and pairs
chart.Correlation(X, histogram = TRUE)