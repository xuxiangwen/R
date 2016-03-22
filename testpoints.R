library(splancs)
source('D:/xujian/project/R/exercise/introduce to data mining/points.R')

opar <- par(no.readonly=TRUE)
par(lty=2, pch=20, cex=0.1, cex.axis=8, cex.lab=8, 
    mai=c(0.4,0.2,0.2,0.2)) 
nf<-layout(matrix(1:6, 2, 3,  byrow = TRUE), c(1, 1, 1), c(1,1))
par(opar)

axis_t<-function(){
  #test axis transform
  x<-seq(0, 10, 0.1)
  y <- sqrt(5^2-(x-5)^2)
  p<-data.frame(x, y)
  plot(p, xlim=c(-10, 10), ylim=c(-10,10))
  p1 <- rotate(p, pi/2)
  points(p1)
  p1 <- rotate(p, pi)
  points(p1)
  p1 <- rotate(p, 3*pi/2)
  points(p1)
  p1 <- hori(p)
  points(p1)
  p1 <- vert(p)
  points(p1)
  p1 <- inverse(p)
  points(p1)
  p1 <- rotate(p1, pi)
  points(p1)
}

plotset <- function(l){
  nf<-layout(matrix(1:6, 2, 3,  byrow = TRUE), c(1, 1, 1), c(1,1)) 
  for(item in l){
    plot(item[,1:2], cex=0.5, pch=20, col=item[,3], 
         xlim=c(-30, 30), ylim=c(-20, 20))     
  }
}

source('D:/xujian/project/R/exercise/introduce to data mining/points.R')
s1 <- set1()
plotset(s1)
s2 <- set2()
plotset(s2)
s3 <- set3()
plotset(s3)

