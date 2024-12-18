xaringanExtra::use_clipboard()
library(vegan)
library(qgraph)
library(ade4)
library(mvabund)
library(pvclust)
library(factoextra)
library(ecodist)
library(tree)
library(rpart)
library(ggplot2)
library(randomForest)
library(caret)
library(rattle)

# to be careful
devtools::install_github("cran/mvpart",force=T)

library(mvpart)
#library(mvpart) # install_github("cran/mvpart", force = T) # after devtools
#library(MVPARTwrap) # install_github("cran/MVPARTwrap", force = T) # after devtools




#  functions from Borcard et al. 2011
source('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/coldiss.r') 



# ?varespec
data (varespec); data(varechem)
varespec[1:5,1:5]
varechem[1:5,1:5]

# Transforming  positive  data to a logarithmic
# scale reduces the range of the data set.
varespec.log<-decostand(varespec,'log')
varespec.log[1:5,1:5]

# Centring by translation, standardize.
# Obj is to remove differences in scale due
# due to diff magnitudes between variables
varechem.stand<-decostand(varechem,'stand')
varechem.stand[1:5,1:5]
# making sure `rowSums is the same
varespec<-decostand(varespec,'total')

# Low weights to variables with
# low counts and many zeros
varespec.hell<-decostand(varespec,'hellinger')
varespec.hell[1:5,1:5]

env.euc<-vegdist(varechem.stand, method='euc')
env.euc

spe.euc<-vegdist(varespec.hell, method='euc')
spe.euc

# default option using vegdist()
spe.bc<-vegdist(varespec)
spe.bc

# Jaccard dissimilarity matrix using vegdist()
spe.jd <- vegdist(varespec,'jac',binary=T) # binary p/a 

coldiss(spe.bc,byrank=F,diag=T)

qgraph(1-spe.bc, layout='spring', vsize=4)

# Step 1: dataset
# ?doubs
data(doubs)
spe.db<-doubs$fish
spa.db<-doubs$xy
# remove empty sample 8 from both datasets
spe.db <-spe.db[-8,]
spa.db<- spa.db[-8,]
# Step 2: chord distance = normalization + euclidean
spe.db.norm<-decostand(spe.db,'normalize') 
spe.db.ch<-vegdist(spe.db.norm,'euc') 
# Step 3: single linkage agglomerative clustering
spe.db.ch.single <-hclust(spe.db.ch,method='single') 
# Step 4: plot dendrogram

spe.db.ch.complete<-hclust(spe.db.ch,method='complete') 

spe.db.ch.UPGMA<-hclust(spe.db.ch,method='average') 

spe.db.ch.ward<-hclust(spe.db.ch,method='ward.D') 

par(mfrow = c(2, 2))
plot(spe.db.ch.single, main='Single linkage agglomerative clustering' ) 
plot(spe.db.ch.complete, main='Complete linkage agglomerative clustering') 
plot(spe.db.ch.UPGMA, main='Average (UPGMA) agglomerative clustering') 
plot(spe.db.ch.ward, main='Ward clustering')

# Single linkage clustering
spe.db.ch.single.coph <- cophenetic (spe.db.ch.single)
cor(spe.db.ch,spe.db.ch.single.coph)

# complete linkage clustering
spe.db.ch.complete.coph <- cophenetic (spe.db.ch.complete)
cor(spe.db.ch,spe.db.ch.complete.coph)

# Average clustering
spe.db.ch.UPGMA.coph <- cophenetic (spe.db.ch.UPGMA)
cor(spe.db.ch,spe.db.ch.UPGMA.coph)

# Ward clustering
spe.db.ch.ward.coph <- cophenetic (spe.db.ch.ward)
cor(spe.db.ch,spe.db.ch.ward.coph)

par(mfrow=c(2,2))

plot(spe.db.ch,spe.db.ch.single.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('Single linkage',paste('Cophenetic correlation',round(cor(spe.db.ch,spe.db.ch.single.coph),3))))
abline (0,1)
lines(lowess(spe.db.ch,spe.db.ch.single.coph),col='red')

plot(spe.db.ch,spe.db.ch.complete.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('Complete linkage',paste('Cophenetic correlation',round(cor(spe.db.ch, spe.db.ch.complete.coph),3))))
abline (0,1)
lines(lowess(spe.db.ch, spe.db.ch.complete.coph),col='red')

plot(spe.db.ch,spe.db.ch.UPGMA.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('UPGMA',paste('Cophenetic correlation',round(cor(spe.db.ch,spe.db.ch.UPGMA.coph),3))))
abline (0,1)
lines(lowess(spe.db.ch,spe.db.ch.UPGMA.coph),col='red')

plot(spe.db.ch,spe.db.ch.ward.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('Ward clustering',paste('Cophenetic correlation',round(cor(spe.db.ch,spe.db.ch.ward.coph),3))))
abline (0,1)
lines(lowess(spe.db.ch,spe.db.ch.ward.coph),col='red')

spe.bc.UPGMA<-hclust(spe.bc,method='average') 
plot(spe.bc.UPGMA, main='Average (UPGMA) agglomerative clustering') 
rect.hclust(spe.bc.UPGMA, k=3, border="red")
rect.hclust(spe.bc.UPGMA, k=6, border="blue")
rect.hclust(spe.bc.UPGMA, k=8, border="green")

plot(spe.bc.UPGMA$height, nrow(varespec):2, 
     type='S',main='Fusion levels - bray-curtis - UPGMA',
     ylab='k (number of clusters)', xlab='h (node height)', col='grey')
text (spe.bc.UPGMA$height,nrow(varespec):2, nrow(varespec):2, col='red', cex=0.8)

# step 1: cut your tree
cutg1<-cutree(spe.bc.UPGMA, k=6)
# step 2: calculate silhouette for the different partitions
sil1<-silhouette (cutg1,spe.bc)
# step 3: plot silhouette 
plot(sil1)

fviz_nbclust(varespec,diss=bcdist(varespec), hcut, method = "wss", hc_method = "average")

## Mantel test
# Optimal number of clusters
# according to mantel statistic 
# Function to compute a binary distance matrix from groups
grpdist<-function(x){
  require (cluster)
  gr<-as.data.frame(as.factor(x))
  distgr<-daisy(gr,'gower')
  distgr
}
# run based on the UPGMA clustering
kt<-data.frame(k=1:nrow(varespec),r=0)
for (i in 2:(nrow(varespec)-1)){
  gr<-cutree(spe.bc.UPGMA,i)
  distgr<-grpdist(gr)
  mt<-cor(spe.bc,distgr, method='pearson')
  kt[i,2] <- mt
}
k.best <- which.max (kt$r)
plot(kt$k,kt$r, 
     type='h', main='Mantel-optimal number of clusters - UPGMA',
     xlab='k (number of groups)',ylab="Pearson's correlation")
axis(1,k.best, 
     paste('optimum', k.best, sep='\n'), col='red',font=2, col.axis='red')
points(k.best,max(kt$r),pch=16,col='red',cex=1.5)

pv.results<-pvclust(t(varespec), # to cluster rows - omit t() to cluster columns,
        method.hclust = "average",
        method.dist = function(x) vegan::vegdist(t(x), "bray"),
        n = 1000) # and other arguments you need
plot(pv.results)

# Calculates hierarchical cluster analysis of species data 
spe.db.ch.UPGMA<-hclust(spe.db.ch,method='average') 
fviz_nbclust(spe.db, hcut, diss=dist(spe.db.norm, method='euclidean'),method = "wss",hc_method = "single")
# Dendrogram with the observed groups
par(mfrow=c(1,2))
plot (spe.db.ch.UPGMA)
rect.hclust (spe.db.ch.UPGMA, k = 6, border = 1:6)
# Spatial distribution of samples with projection of hierarchical classification
UPGMA.cluster <- cutree (spe.db.ch.UPGMA, k = 6)
plot (y ~ x, data = spa.db, pch = UPGMA.cluster, col = UPGMA.cluster, type = 'b', main = 'Chord distance - UPGMA method')
dev.off()

spe.chwo<-reorder.hclust(spe.db.ch.UPGMA,spe.db.ch)
dend<-as.dendrogram(spe.chwo) 
heatmap(as.matrix(spe.db.ch),Rowv=dend,symm=TRUE, margin=c(3,3))

# k-means partitioning of the pre-transformed species data
spe.kmeans <- kmeans(spe.bc, centers=6, nstart=100)
# k-means group number of each observation spe.kmeans$cluster 
spe.kmeans$cluster
# Comparison with the 5-group classification derived from UPGMA clustering
comparison<-table(spe.kmeans$cluster,cutg1)
comparison
# Visualize k-means clusters 
fviz_cluster(spe.kmeans, data = varespec,geom = "point",
             stand = T, ellipse.type = "norm") 

spe.KM.cascade<-cascadeKM(spe.bc,inf.gr=2,sup.gr=10,iter=100,criterion='calinski')
plot(spe.KM.cascade,sortg=TRUE)

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(iris[,1:4], pch = 19,  cex = 0.5,
      col = my_cols[iris$Species],
      lower.panel=NULL)

## fviz_nbclust(iris[, 1:4], kmeans, method = "silhouette")
## spe.KM.cascade<-cascadeKM(iris[,1:4],inf.gr=2, sup.gr=10, iter=100, criterion='calinski')
## plot(spe.KM.cascade,sortg=TRUE)
## set.seed(1)
## irisCluster<-kmeans(iris[, 1:4], 3, nstart= 20)
## table(irisCluster$cluster, iris$Species)
## irisCluster$cluster<-as.factor(irisCluster$cluster)
## plot7<-ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
## plot8<-ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
## grid.arrange(plot7, plot8, ncol=2)
## # conclusions?

tree1<-tree(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
summary(tree1 )
plot(tree1)
text(tree1)

tree2 <- rpart(Species ~ ., data=iris, method="class")
fancyRpartPlot(tree2, main="Iris") # package rattle

# Extra to exciting your curiosity
iris.rf=randomForest(Species~., data=iris, importance=TRUE, proximity=TRUE, ntree=500)
# Required number of trees gives errors for each species and the average for all species (black):
plot(iris.rf,lty=2)
# Misclassification error rates:
iris.rf$confusion
# Importance of individual predictor variables for classification (the further the value is on the right of the plot, the more important):
varImpPlot(iris.rf)
# The membership of a particular class as a function of a variable value can be displayed with this
partialPlot(iris.rf,iris,Petal.Width,"setosa")
# we can predict unclassified observations. We make up some sample new observations from the original dataset to save some time importing (the first three rows are P. setosa, lets see if RandomForest gets that right:
newobs=iris[1:3,1:4]
predict(iris.rf,newobs)
# This last plot conveys the confidence in your predictions for each individual sample. Colors represent species and points are samples. In this case, many samples can be predicted with great certainty (1) and only few classifications are questionable (approaching 0)
plot(margin(iris.rf))

data(doubs)
spe.norm<-decostand(doubs$fish[-8,], 'nor')
env<-doubs$env[-8,]

library(mvpart)
install.packages("mvpart", version="4.1.1")
# par(mfrow=c(1,2))
spe.ch.mvpart <-
  mvpart(data.matrix(spe.norm)~.,
         env,
         margin = 0.08,
         cp=0,
         xv='min', # try 'pick' best number, '1se'
         xval=nrow(doubs$fish),
         xvmult = 100
         )

# create a list of 80% of the rows in the original dataset that we can use for training
validation_index <- createDataPartition(iris$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- iris[-validation_index,]
# use the remaining 80% of data to training and testing the models
idataset <- iris[validation_index,]

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Random Forest
set.seed(10)
fit.rf <- train(Species~., data=idataset, method="rf", metric=metric, trControl=control)
# CART
set.seed(10)
fit.cart <- train(Species~., data=idataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(10)
fit.knn <- train(Species~., data=idataset, method="knn", metric=metric, trControl=control)
# lda
set.seed(10)
fit.lda <- train(Species~., data=idataset, method="lda", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, rf=fit.rf))
summary(results)$statistics$Accuracy

# summarize Best Model
print(fit.lda)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
