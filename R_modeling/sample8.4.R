X<-iris[,1:4]
G<-gl(3,50)
source("distinguish.bayes.R")
r <- distinguish.bayes(X,G, var.equal=TRUE)
as.numeric(r)-as.numeric(G)
r <- distinguish.bayes(X,G)
as.numeric(r)-as.numeric(G)