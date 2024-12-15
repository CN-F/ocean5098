#######################
### practice 7.5
library(vegan)
library(qgraph)
library(ade4)
library(mvabund)
library (pvclust)
library(factoextra)
library(ecodist)
library(tree)
library(rpart)
library(ggplot2)
library(randomForest)
library(caret)
library(rattle)
rm(list=ls())

#######################
### dataset
data(tikus)
head(tikus$x, 5)
head(tikus$abund, 5)
tikus_spe<- tikus$abund
tikus_env<- tikus$x
tikus_spe_sel<- tikus_spe[tikus_env$time %in% c(81,83,85),]
bray_site<- vegdist(tikus_spe_sel)
coldiss(bray_site, byrank=F, diag=T)

source('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/coldiss.r') 
qgraph(1-bray_site, layout='spring', vsize=4)
