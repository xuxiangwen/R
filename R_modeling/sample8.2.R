X<-iris[,1:4]
G<-gl(3,50)
source("distinguish.distance.R")
xg <- distinguish.distance(X,G)
as.numeric(xg) - as.numeric(G)
xg <- distinguish.distance(X,G, var.equal=T)
as.numeric(xg) - as.numeric(G)
