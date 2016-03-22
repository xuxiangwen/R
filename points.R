#library(maptools)
library(splancs)

rotate<-function(points, angle){
  if (!is.matrix(points)) points <- as.matrix(points)
  s <- sin(angle)
  c <- cos(angle)
  m <- matrix(c(c, s, -s, c) , nrow=2)
  t(m%*%t(points))
}

hori<-function(points){
  if (!is.matrix(points)) points <- as.matrix(points)
  m <- matrix(c(-1, 0, 0, 1) , nrow=2)
  t(m%*%t(points))  
}

vert<-function(points){
  if (!is.matrix(points)) points <- as.matrix(points)
  m <- matrix(c(1, 0, 0, -1) , nrow=2)
  t(m%*%t(points))  
}

inverse<-function(points){
  if (!is.matrix(points)) points <- as.matrix(points)
  m <- matrix(c(0, 1, 1, 0) , nrow=2)
  t(m%*%t(points))  
}

rline<-function(n, x1, y1, x2, y2){
  if (x1==x2) {
    ps <- data.frame(x=rep(x1, n), 
                     y=seq(min(y1, y2), max(y1, y2), length.out=n)) 
  }
  else{
    x <- seq(min(x1, x2), max(x1, x2), length.out=n)
    y <- (y2-y1)/(x2-x1)*(x-x1)+y1
    ps <- data.frame(x, y) 
  }
  return(as.matrix(ps))
}
# ps<-rline(100, 10, 20, 20, 4)
# plot(ps,  xlim=c(0, 30), ylim=c(0,30))
# ps<-rline(50, 10, 20, 10, 4)
# points(ps)
# ps<-rline(50, 10, 4, 20, 4)
# points(ps)


# ps<-rsin(100, 10, 10)
# plot(ps)

getps10 <- function(m){
  #draw first cluster
  a <-seq(0, pi, by=pi/20)
  r<-15; delta<-0;
  
  #draw first cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster1 <- csr(as.matrix(f1), m) 
  
  clusters <- as.data.frame(cluster1)
  clusters$clus <- c(rep(1, dim(cluster1)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}


getps11 <- function(m){
  r<-5; delta<-0;
  #draw first cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster1 <- csr(as.matrix(f1), m) 
  
  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=-10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  clusters <- as.data.frame(rbind(cluster1, cluster2))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps12 <- function(m){
  r<-5; delta<-0;
  #draw first cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=5.2+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster1 <- csr(as.matrix(f1), m) 
  
  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=-5.2+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  clusters <- as.data.frame(rbind(cluster1, cluster2))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps13 <- function(m){
  aa<-15; bb<-5;
  #draw first cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=aa*cos(a), y=6.5+bb*sin(a))
  cluster1 <- csr(as.matrix(f1), m) 
  
  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=aa*cos(a), y=-6.5+bb*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  clusters <- as.data.frame(rbind(cluster1, cluster2))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps14 <- function(m){
  aa<-15; bb<-5;
  #draw first cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=aa*cos(a), y=5.5+bb*sin(a))
  cluster1 <- csr(as.matrix(f1), m) 
  
  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=aa*cos(a), y=-5.5+bb*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  clusters <- as.data.frame(rbind(cluster1, cluster2))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps15 <- function(m){
  r<-3; delta<-0;
  a <-seq(0, 2*pi, by=pi/20)
  #draw first cluster  
  f1 <- data.frame(x=9+(r-delta)*cos(a), y=9*sqrt(3)+(r-delta)*sin(a))
  cluster1 <- csr(as.matrix(f1), m) 
  
  #draw second cluster
  f1 <- data.frame(x=-9+(r-delta)*cos(a), y=9*sqrt(3)+(r-delta)*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  #draw third cluster
  r1<-12
  f1 <- data.frame(x=(r1-delta)*cos(a), y=(r1-delta)*sin(a))
  cluster3 <- csr(as.matrix(f1), m*(r1/r)^2)   
  
  clusters <- as.data.frame(rbind(cluster1, cluster2, cluster3))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]),
                     rep(3, dim(cluster3)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps16 <- function(m){
  r<-3; delta<-0;
  a <-seq(0, 2*pi, by=pi/20)
  #draw first cluster  
  f1 <- data.frame(x=9+(r-delta)*cos(a)*2.5, y=9*sqrt(3)+(r-delta)*sin(a))
  cluster1 <- csr(as.matrix(f1), m) 
  
  #draw second cluster
  f1 <- data.frame(x=-9+(r-delta)*cos(a)*2.5, y=9*sqrt(3)+(r-delta)*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  #draw third cluster
  r1<-12
  f1 <- data.frame(x=(r1-delta)*cos(a)*2.2, y=0.9*(r1-delta)*sin(a))
  cluster3 <- csr(as.matrix(f1), m*(r1/r)^2)   
  
  clusters <- as.data.frame(rbind(cluster1, cluster2, cluster3))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]),
                     rep(3, dim(cluster3)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps17 <- function(){
  set.seed(665544)
  n <- 600
  x <- runif(10, 0, 10)*6 -30
  deltax <- rnorm(n, sd=0.3)*4 
  y <- runif(10, 0, 10)*3.7  -20
  deltay <- rnorm(n, sd=0.3)*3.7
  xy <- cbind(x+deltax, 
              y+deltay, 1:10)
  clusters <- as.data.frame(xy)
  colnames(clusters)<-c('x','y','clus')
  #plot(xy[,1:2], col=xy[,3])
  return(clusters)
}


getps21 <- function(n, m){
  #draw first cluster
  a <-seq(0, pi, by=pi/20)
  r<-5; delta<-0.05*r; f1 <- data.frame(x=10+r*cos(a), y=r*sin(a)) 
  f1 <- rbind(f1, c(0, 0), c(0,10), c(20, 10), c(20, 0))
  f1 <- as.matrix(f1); ps1 <- csr(f1, n/4)
  f2 <- hori(f1); ps2 <- csr(f2, n/4)  
  f3 <- vert(f1); ps3 <- csr(f3, n/4)
  f4 <- rotate(f1, pi); ps4 <- csr(f4, n/4)  
  cluster1 = rbind(ps1, ps2, ps3, ps4)

  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 

  #draw third cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=-10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster3 <- csr(as.matrix(f1), m) 

  clusters <- as.data.frame(rbind(cluster1, cluster2, cluster3))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]),
                     rep(3, dim(cluster3)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps22 <- function(n){ 
  #draw first cluster
  r<-5   
  a <-seq(0, pi, by=pi/20)
  ra <- rev(a)
  f11 <- data.frame(x=2*r*cos(a), y=2*r*sin(a)) 
  f12 <- data.frame(x=3*r*cos(ra), y=3*r*sin(ra)) 
  f1 <- as.matrix(rbind(f11, f12)); ps1 <- csr(f1, n*2.5)
  
  f2 <- vert(f1); ps2 <- csr(f2, n*2.5)
  cluster1 = rbind(ps1, ps2)
  
  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=(r)*cos(a), y=(r)*sin(a))
  cluster2 <- csr(as.matrix(f1), n) 
  clusters <- as.data.frame(rbind(cluster1, cluster2))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)  
}

getps23 <- function(n){ 
  #draw first cluster
  f1 <- matrix(c(0, 0, 20, 0, 10, 10*sqrt(3)), byrow=T, ncol=2)
  cluster1 <- csr(f1, n)
  
  #draw second cluster
  f2 <- sweep(f1, 2, c(-20,0), FUN='+')
  cluster2 <- csr(f2, n)  

  #draw third cluster
  f3 <- sweep(f1, 2, c(-10,-10*sqrt(3)), FUN='+')
  cluster3 <- csr(f3, n)   
  
  clusters <- as.data.frame(rbind(cluster1, cluster2, cluster3))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]),
                     rep(3, dim(cluster3)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)  
}


getps24 <- function(n, m, k){  
  a <-seq(0, pi, by=pi/20)
  r<-5; delta<-0.1*r; f1 <- data.frame(x=10+r*cos(a), y=r*sin(a))   
  #draw first cluster
  f1 <- rbind(f1, c(0, 0), c(0,15), c(30, 15), c(30, 0))
  f1 <- as.matrix(f1); ps1 <- csr(f1, n/4)
  
  aa<-5;bb<-4  
  a <-seq(0, pi/2, by=pi/20)
  ra <- rev(a)
  f21 <- data.frame(x=8+3*aa*cos(a), y=3*bb*sin(a)) 
  f22 <- data.frame(x=8+2*aa*cos(ra), y=2*bb*sin(ra)) 
  f2 <- as.matrix(rbind(f21, f22, f1));  
  f2 <- hori(f2)
  ps2 <- csr(f2, n/4)  
  
  f3 <- vert(f1); ps3 <- csr(f3, n/4)
  f4 <- vert(f2); ps4 <- csr(f4, n/4)  
  cluster1 = rbind(ps1, ps2, ps3, ps4)
  
  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  #draw third cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=-10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster3 <- csr(as.matrix(f1), m) 
  
  #draw forth cluster
  aa<-5;bb<-4  ;delta<-0.05
  a <-seq(pi/2, 3*pi/2, by=pi/20)
  ra <- rev(a)
  f11 <- data.frame(x=-8+2*aa*(1+0.05)*cos(a), y=2*bb*(1+0.05)*sin(a)) 
  f12 <- data.frame(x=-8+3*aa*(1-0.033)*cos(ra), y=3*bb*(1-0.033)*sin(ra)) 
  f1 <- as.matrix(rbind(f11, f12)); ps1 <- csr(f1, k)
  cluster4 = ps1
  
  clusters <- as.data.frame(rbind(cluster1, cluster2, 
                                  cluster3, cluster4))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]),
                     rep(3, dim(cluster3)[1]),
                     rep(4, dim(cluster4)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps25 <- function(n, m, k){  
  a <-seq(0, pi, by=pi/20)
  r<-5; delta<-0.1*r; 
  
  #draw first cluster  
  a <- seq(-pi, pi, length.out=n) 
  ps <- as.matrix(data.frame(x=a, y=sin(a)))
  ps1 <- rotate(ps, pi/2)
  ps2 <- sweep(ps1, 2, c(3,1.5), FUN='*')
  ps3 <- sweep(ps2, 2, c(-28,0), FUN='+')  
  cluster1 = ps3
  
  #draw second cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster2 <- csr(as.matrix(f1), m) 
  
  #draw third cluster
  a <-seq(0, 2*pi, by=pi/20)
  f1 <- data.frame(x=-10+(r-delta)*cos(a), y=(r-delta)*sin(a))
  cluster3 <- csr(as.matrix(f1), m) 
  
  #draw forth cluster
  aa<-5;bb<-4  
  a <-seq(pi/2, 3*pi/2, by=pi/20)
  ra <- rev(a)
  f11 <- data.frame(x=-8+2*aa*cos(a), y=2*bb*sin(a)) 
  f12 <- data.frame(x=-8+3*aa*cos(ra), y=3*bb*sin(ra)) 
  f1 <- as.matrix(rbind(f11, f12)); ps1 <- csr(f1, k)
  cluster4 <- ps1
  
  cluster5 <- rline(30, -5, 0, 5, 0) 
  
  clusters <- as.data.frame(rbind(cluster1, cluster2, 
                                  cluster3, cluster4, cluster5))
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]),
                     rep(3, dim(cluster3)[1]),
                     rep(4, dim(cluster4)[1]),
                     rep(5, dim(cluster5)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps26 <- function(){  
  #plot(c(0,0), cex=0.5, pch=20, xlim=c(-30, 30), ylim=c(-20, 20))
  #draw first cluster  
  a <- seq(0, 2*pi, length.out=100) 
  ps <- as.matrix(data.frame(x=a, y=sin(a)))
  ps1 <- sweep(ps, 2, c(5,5), FUN='*') 
  cluster1 = ps1
  #points(cluster1)
  
  #draw second cluster  
  ps1 <- rotate(cluster1, pi/16)   
  ps2 <- sweep(ps1, 2, c(0,0.5), FUN='+')
  cluster2 <- ps2
  #points(cluster2)
  
  #draw third cluster
  ps1 <- sweep(cluster1, 2, c(4,4), FUN='+')  
  ps2 <- rotate(ps1, pi/8)   
  cluster3 <- ps2
  #points(cluster3)
  
  #draw forth cluster
  ps1 <- sweep(cluster3, 2, -cluster3[dim(cluster3)[1],], FUN='+')  
  ps2 <- rotate(ps1, -pi/8)   
  ps3 <- sweep(ps2, 2, cluster3[dim(cluster3)[1],], FUN='+')  
  ps4 <- sweep(ps3, 2, c(-1,0), FUN='+')
  cluster4 <- ps4
  #points(cluster4)  
  
  #draw fifth cluster
  ps1 <- vert(cluster2)  
  ps2 <- sweep(ps1, 2, c(-2,-2), FUN='+') 
  cluster5 <- ps2
  #points(cluster5)
  
  clusters <- as.data.frame(rbind(cluster1, cluster2, 
                                  cluster3, cluster4, cluster5))
  clusters <- sweep(clusters, 2, c(-15,-8), FUN='+') 
  clusters$clus <- c(rep(1, dim(cluster1)[1]), 
                     rep(2, dim(cluster2)[1]),
                     rep(3, dim(cluster3)[1]),
                     rep(4, dim(cluster4)[1]),
                     rep(5, dim(cluster5)[1]))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

getps31 <- function(m, k, n){  
  #plot(c(0,0), cex=0.5, pch=20, xlim=c(-30, 30), ylim=c(-20, 20))
  r<-2; delta<-0.2; 
  #first cluster
  a <-seq(pi, 3*pi, by=pi/72)
  f1 <- data.frame(x=(r-delta)*cos(a), y=(r-delta)*sin(a))
  frame1 <- rbind(f1, f1[1,])
  cluster1<- csr(as.matrix(frame1), m) 
  #points(cluster1, cex=0.5, pch=20)
  
  #second cluster
  a <-seq(3/4*pi, 11/4*pi, by=pi/72);
  f1 <- data.frame(x=-5*sqrt(2)+(r-delta)*cos(a), y=5*sqrt(2)+(r-delta)*sin(a))
  frame2 <- rbind(f1, f1[1,])
  cluster2<- csr(as.matrix(frame2), m) 
  #points(cluster2, cex=0.5, pch=20)  
  
  #third cluster
  a <-seq(1/4*pi, 9/4*pi, by=pi/72)
  f1 <- data.frame(x=5*sqrt(2)+(r-delta)*cos(a), y=5*sqrt(2)+(r-delta)*sin(a))
  frame3 <- rbind(f1, f1[1,])
  cluster3<- csr(as.matrix(frame3), m) 
  #points(cluster3, cex=0.5, pch=20)   

  #forth cluster
  a <-seq(4/3*pi, 5/3*pi, by=pi/72);r<-8
  f1 <- data.frame(x=(r-delta)*cos(a), y=(r-delta)*sin(a))
  frame4 <- rbind(f1, f1[1,])
  cluster4<- csr(as.matrix(frame4), k) 
  #points(cluster4, cex=0.5, pch=20)  

  #fifth cluster
  r<-2;delta<-0
  a <-seq(pi, 3*pi, by=pi/72)
  f1 <- data.frame(x=(r-delta)*cos(a), y=(r-delta)*sin(a))
  frame1 <- rbind(f1, f1[1,])
  a <-seq(3/4*pi, 11/4*pi, by=pi/72);
  f1 <- data.frame(x=-5*sqrt(2)+(r-delta)*cos(a), y=5*sqrt(2)+(r-delta)*sin(a))
  frame2 <- rbind(f1, f1[1,])  
  a <-seq(1/4*pi, 9/4*pi, by=pi/72)
  f1 <- data.frame(x=5*sqrt(2)+(r-delta)*cos(a), y=5*sqrt(2)+(r-delta)*sin(a))
  frame3 <- rbind(f1, f1[1,])  
  a <-seq(4/3*pi, 5/3*pi, by=pi/72);r<-8
  f1 <- data.frame(x=(r-delta)*cos(a), y=(r-delta)*sin(a))
  frame4 <- rbind(f1, f1[1,])
  
  r<-18
  a1 <-rev(seq(pi, 4/3*pi, by=pi/72));f1 <- data.frame(x=r*cos(a1), y=r*sin(a1))
  a2 <-rev(seq(3/4*pi, pi, by=pi/72));f2 <- data.frame(x=r*cos(a2), y=r*sin(a2))
  a3 <-rev(seq(1/4, 3/4*pi, by=pi/72));f3 <- data.frame(x=r*cos(a3), y=r*sin(a3))
  a4 <-rev(seq(-2/3*pi, 1/4*pi, by=pi/72));f4 <- data.frame(x=r*cos(a4), y=r*sin(a4))
  frame5 <- rbind(f1, frame1, f2, frame2, f3, frame3, f4, frame4 )
  cluster5<- csr(as.matrix(frame5), n) 
  #points(cluster5, cex=0.5, pch=20)    
  
  if (is.null(cluster1))  
    len1<-0
  else
    len1<-dim(cluster1)[1]
  if (is.null(cluster2))  
    len2<-0
  else
    len2<-dim(cluster2)[1]
  if (is.null(cluster3))  
    len3<-0
  else
    len3<-dim(cluster3)[1]
  if (is.null(cluster4))  
    len4<-0
  else
    len4<-dim(cluster4)[1]
  if (is.null(cluster5))  
    len5<-0
  else
    len5<-dim(cluster5)[1]
  clusters <- as.data.frame(rbind(cluster1, cluster2, 
                                  cluster3, cluster4, cluster5))
  clusters$clus <- c(rep(1, len1), 
                     rep(2, len2),
                     rep(3, len3),
                     rep(4, len4),
                     rep(5, len5))
  colnames(clusters)<-c('x','y','clus')
  return(clusters)
}

set1 <- function(outlier=F, randerror=T){

  l<-list('1' <- getps11(200), '2' <- getps12(200), '3' <- getps13(400),
          '4' <- getps14(400), '5' <- getps15(100), '6' <- getps16(100))
  ran(l, outlier, randerror)
}

set2 <- function(outlier=F, randerror=T){
  l<-list('1' <- getps21(200, 600), '2' <- getps22(200), 
       '3' <- getps23(200), '4' <- getps24(200, 200, 2000), 
       '5' <- getps25(50, 200, 2000), '6' <- getps26())
  ran(l, outlier, randerror)
}

set3 <- function(outlier=F, randerror=T){
  l<-list('1' <- getps31(50, 25, 16000), '2' <- getps31(200, 100, 4000), 
       '3' <- getps31(0, 0, 6000), '4' <- getps31(200, 100, 0))
  ran(l, outlier, randerror)
}

ran <- function(l, outlier, randerror){
  set.seed(665544)  

  outliers <- data.frame(x = c(30, 29, -30), 
                           y = c(19, 20, -20),
                           clus = c(0, 0, 100))    

  if (randerror){
    for(i in 1:length(l)){
      count <- length(l[[i]][,1])
      l[[i]][,1] <- l[[i]][,1] + rnorm(count, sd=0.1) 
      l[[i]][,2] <- l[[i]][,2] + rnorm(count, sd=0.1)
      if (outlier){
        l[[i]] <- rbind(l[[i]], outliers)
      }    
    } 
  }
  return(l)
}

# 
# runifrect<-function(n, min_x, max_x, min_y, max_y){
#   data.frame(x=runif(n, min_x, max_x), y=runif(n, min_y, max_y))
# }
# ps<-runifrect(1000, -20, 20, -10, 10) 
# plot(ps)
# 
# runifcircle<-function(n, center_x, center_y, r){
#   
#   n1<-round(n*4/pi, 0)
#   ps <- data.frame(x=runif(n1, center_x-r, center_x+r), 
#                        y=runif(n1, center_y-r, center_y+r))
#   dis <- sqrt((ps$x-center_x)^2+(ps$y-center_y)^2)
#   ps1 <- ps[which(dis <= r),]
#   return(ps1)
# }
# ps<-runifcircle(300, 0, 0, 5)
# plot(ps)
# 
# rnormcircle<-function(n, center_x, center_y, sdx, sdy){  
#   n1<-round(n*4/pi, 0)
#   ps <- data.frame(x=rnorm(n1, center_x, sdx), 
#                    y=rnorm(n1, center_y, sdy))
#   return(ps)
# }
# ps<-rnormcircle(500, 0, 0, 10, 20)
# plot(ps)
# 
# runifannulus<-function(n, center_x, center_y, r1, r2){  
#   n1<-round(n*4*r2^2/pi/(r2^2-r1^2), 0)
#   ps <- data.frame(x=runif(n1, center_x-r2, center_x+r2), 
#                    y=runif(n1, center_y-r2, center_y+r2))
#   dis <- sqrt((ps$x-center_x)^2+(ps$y-center_y)^2)
#   ps1 <- ps[which(dis>=r1 & dis<=r2),]
#   return(ps1)
# }
# ps<-runifannulus(300, 0, 0, 5, 10)
# plot(ps,  xlim=c(-20, 20), ylim=c(-20,20))
# 
# 
# getps1 <- function(){
#   ps1<-runifrect(1000, -20, 20, -10, 10)  
#   ps2<-runifcircle(500, -10, 0, 5)
#   ps3<-runifcircle(500, 10, 0, 5)
#   return(rbind(ps1, ps2, ps3))
# }
# ps <- getps1()
# plot(ps, xlim=c(-25, 25), ylim=c(-15,15))
# 
# getps2 <- function(){ 
#   ps1<-runifcircle(200, 0, 0, 5)
#   ps2<-runifannulus(600, 0, 0, 10, 15)
#   return(rbind(ps1, ps2))
# }
# ps <- getps2()
# plot(ps, xlim=c(-20, 20), ylim=c(-20,20))
# 
# getps3 <- function(){ 
#   ps1<-runifcircle(200, -5, 0, 5)
#   ps2<-runifcircle(200, 5, 0, 5)
#   return(rbind(ps1, ps2))
# }
# ps <- getps3()
# plot(ps, xlim=c(-10, 10), ylim=c(-10,10))
#         
# 
# 
# rsin<-function(n, x0, y0, scalex=1, scaley=1){
# x<- seq(0, 2*pi, length.out=n) 
# y<- sin(x) 
# ps <- data.frame(x=x*scalex+x0, 
#                  y=y*scaley+y0)
# return(as.matrix(ps))
# }
# 
# 
# 
# 
