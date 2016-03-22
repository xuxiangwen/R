#The k-Means Clustering
iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3))
table(iris$Species, kmeans.result$cluster)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], 
       col = 1:3, pch = 8, cex=2)

#6.2 The k-Medoids Clustering
library(fpc)
pamk.result <- pamk(iris2)
pamk.result$nc
layout(matrix(c(1,2),1,2))
plot(pamk.result$pamobject)

pam.result <- pam(iris2, 3)
table(pam.result$clustering, iris$Species)
layout(matrix(c(1,2),1,2))
plot(pam.result)

#6.3 Hierarchical Clustering
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

#6.4 Density-based Clustering
library(fpc)
iris2 <- iris[-5]
ds <- dbscan(iris2, eps=0.42, MinPts=5)
table(ds$cluster, iris$Species)
plot(ds, iris2)
plot(ds, iris2[c(1,4)])
plotcluster(iris2, ds$cluster)

# create a new dataset for labeling
set.seed(435)
idx <- sample(1:nrow(iris), 10)
newData <- iris[idx,-5]
newData <- newData + 
  matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)
# label new data
myPred <- predict(ds, iris2, newData)
# plot result
plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)
# check cluster labels
table(myPred, iris$Species[idx])