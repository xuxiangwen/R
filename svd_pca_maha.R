initPlot <- function(xlim, ylim){
  plot(NULL, xlim=xlim, ylim=ylim, cex=0.5, xlab='', ylab='')
}

drawLine <- function(v, col='black'){
  v1 <- rbind(c(0,0), v[,1])
  v2 <- rbind(c(0,0), v[,2])  
  lines(v1, col=col, pch='1')
  lines(v2, col=col, pch='2')
  text(t(v)+0.1, labels=paste('v', 1:ncol(v), sep=''), cex=0.5)
}

drawPoint <- function(p, col='black', showNum=TRUE){
  pch = 20;
  if (showNum) pch=as.character(1:nrow(p)) 
  points(p, cex=0.5, pch=pch, col=col)
}

#######################################################
#理解PCA, 分析PCA的计算过程
#######################################################
x <- matrix(c(1, 0.8, 2, 2.3, 3, 2.7 ), ncol=2, byrow=TRUE)
x1 <- scale(x, center=TRUE, scale=FALSE);x1
t(x1)%*%x1/(nrow(x)-1); var(x);cor(x)

#需要对x做一个转换(正交变换), 使得其方差在某个方向最大，其他方向依次递减
#1.用svd的方法
#show original point
initPlot(xlim=c(-2, 2), ylim=c(-2, 2))
drawPoint(x1)
drawLine(matrix(c(1,0,0,1), nrow=2))

s <- svd(x1)
x2 <- x1%*%s$v   #相当于把矩阵中的每个行向量都影射到了以v为基新的空间里面，x2就是在新坐标里面点的坐标
drawLine(s$v, col='blue')  #从图上可以看出在新坐标下，v1方向方差最大
#drawPoint(x2, col='blue')  
s$d  #从图或者还是奇异值大小，我们可以忽略v2方向的value，这样只保留v1方向
x1%*%s$v[,1]   #这就是将一个二维数据变成了一维数据

t(x1)%*%x1;
round(t(x2)%*%x2); #现在方差等于奇异值的平方（即特征值）

#2. 用特征值
initPlot(xlim=c(-2, 2), ylim=c(-2, 2))
drawPoint(x1)
drawLine(matrix(c(1,0,0,1), nrow=2))

e <- eigen(t(x1)%*%x1)
x2 <- x1%*%e$vectors   
drawLine(e$vectors, col='blue')  
e$values 
x1%*%e$vectors[,1]   

t(x1)%*%x1;
round(t(x2)%*%x2);

#2. 用主成分分析PCA
initPlot(xlim=c(-2, 2), ylim=c(-2, 2))
drawPoint(x1)
drawLine(matrix(c(1,0,0,1), nrow=2))

p <- princomp(x1)
summary(p, loadings=TRUE)
predict(p)            #得到在新的基下的坐标
screeplot(p,type="lines")

#主成分分析
student<-data.frame(
  X1=c(148, 139, 160, 149, 159, 142, 153, 150, 151, 139,
       140, 161, 158, 140, 137, 152, 149, 145, 160, 156,
       151, 147, 157, 147, 157, 151, 144, 141, 139, 148),
  X2=c(41, 34, 49, 36, 45, 31, 43, 43, 42, 31,
       29, 47, 49, 33, 31, 35, 47, 35, 47, 44,
       42, 38, 39, 30, 48, 36, 36, 30, 32, 38),
  X3=c(72, 71, 77, 67, 80, 66, 76, 77, 77, 68,
       64, 78, 78, 67, 66, 73, 82, 70, 74, 78,
       73, 73, 68, 65, 80, 74, 68, 67, 68, 70),
  X4=c(78, 76, 86, 79, 86, 76, 83, 79, 80, 74,
       74, 84, 83, 77, 73, 79, 79, 77, 87, 85,
       82, 78, 80, 75, 88, 80, 76, 76, 73, 78)
)
student.pr <- princomp(student, cor = TRUE)
pca <- summary(student.pr, loadings=TRUE);pca

#比较特征值和pca中的内容
c <- cor(student)
v <- eigen(c)$vectors;v
l <- eigen(c)$values;l
cor(student)
i<-1;t(c%*%v[,i]);l[i]*v[,i]
i<-2;t(c%*%v[,i]);l[i]*v[,i]
i<-3;t(c%*%v[,i]);l[i]*v[,i]
i<-4;t(c%*%v[,i]);l[i]*v[,i]
c%*%v;v%*%diag(l)
v%*%diag(l)%*%solve(v);c
(scale(as.matrix(student))%*%v)[1:10,]
predict(student.pr)
screeplot(student.pr,type="lines")

student.pr <- princomp(student, cor = FALSE)
summary(student.pr, loadings=TRUE)

#主成分分析：变量分类
#主成分分析：主成分回归
x<-c(1.00,
     0.79, 1.00,
     0.36, 0.31, 1.00,
     0.96, 0.74, 0.38, 1.00,
     0.89, 0.58, 0.31, 0.90, 1.00,
     0.79, 0.58, 0.30, 0.78, 0.79, 1.00,
     0.76, 0.55, 0.35, 0.75, 0.74, 0.73, 1.00,
     0.26, 0.19, 0.58, 0.25, 0.25, 0.18, 0.24, 1.00,
     0.21, 0.07, 0.28, 0.20, 0.18, 0.18, 0.29,-0.04, 1.00,
     0.26, 0.16, 0.33, 0.22, 0.23, 0.23, 0.25, 0.49,-0.34, 1.00,
     0.07, 0.21, 0.38, 0.08,-0.02, 0.00, 0.10, 0.44,-0.16, 0.23, 1.00,
     0.52, 0.41, 0.35, 0.53, 0.48, 0.38, 0.44, 0.30,-0.05, 0.50, 0.24, 1.00,
     0.77, 0.47, 0.41, 0.79, 0.79, 0.69, 0.67, 0.32, 0.23, 0.31, 0.10, 0.62, 1.00,
     0.25, 0.17, 0.64, 0.27, 0.27, 0.14, 0.16, 0.51, 0.21, 0.15, 0.31, 0.17, 0.26, 1.00,
     0.51, 0.35, 0.58, 0.57, 0.51, 0.26, 0.38, 0.51, 0.15, 0.29, 0.28, 0.41, 0.50, 0.63, 1.00,
     0.21, 0.16, 0.51, 0.26, 0.23, 0.00, 0.12, 0.38, 0.18, 0.14, 0.31, 0.18, 0.24, 0.50, 0.65, 1.00)  
names<-c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
         "X10", "X11", "X12", "X13", "X14", "X15", "X16")
R<-matrix(0, nrow=16, ncol=16, dimnames=list(names, names))
for (i in 1:16){
  for (j in 1:i){
    R[i,j]<-x[(i-1)*i/2+j]; R[j,i]<-R[i,j]
  }
}
pr<-princomp(covmat=R); load<-loadings(pr)
plot(load[,1:2]); text(load[,1], load[,2], adj=c(-0.4, 0.3))

# x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
#            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
# colnames(x) <- c("x", "y")
# (cl <- kmeans(x, 2))
# plot(x, col = cl$cluster)
# points(cl$centers, col = 1:2, pch = 8, cex = 2)

#Understand SVD
fun <- function(x){
  sqrt(1-x^2)
}
curve(fun, -1, 1, 100)

angle <- c(seq(0, pi, by=pi/120))
X <- apply(t(angle), 2, function(x) c(cos(x), sin(x)))
plot(t(X), col='Green', pch=20, xlim=c(-2,2), ylim=c(-2,2.5), cex=0.1)
A <- matrix(c(1, 0, 1.2, 0.8), ncol=2)
T <- A%*%X
points(t(T), col='Red', pch=20, cex=0.1)


#画矩阵A的奇异值以及v
s <- svd(A)
s$u%*%diag(s$d)%*%t(s$v)
T1 <- s$u%*%diag(s$d)%*%t(s$v)%*%X
points(t(T1), col='CYAN', pch=20, cex=0.1)
A%*%s$v%*%solve(diag(s$d)) - s$u

v1 <- rbind(c(0,0), s$v[,1])
v2 <- rbind(c(0,0), s$v[,2])
lines(v1)
lines(v2)
#the distance of all points
dis<-apply(T, 2, function(x) c(x[1], x[2], sqrt(x[1]^2+x[2]^2)))
summary(dis[3,])
s$d;
maxp <- dis[, which.max(dis[3,])][1:2];maxp
minp <- dis[, which.min(dis[3,])][1:2];minp
maxv <- rbind(c(0,0), maxp)
minv <- rbind(c(0,0), minp)
A%*%s$v
lines(maxv, col='Blue')
lines(minv, col='Blue')

#画矩阵AV的向量
v <- A%*%s$v
v1 <- rbind(c(0,0), v[,1])
v2 <- rbind(c(0,0), v[,2])
lines(v1, col='Red', type='b')
lines(v2, col='Red', type='b')

#画矩阵t(A)%*%A的特征向量和特征值
T2 <- t(A)%*%A%*%X
points(t(T2), col='Black', pch=20, cex=0.1)
dis<-apply(T2, 2, function(x) c(x[1], x[2], sqrt(x[1]^2+x[2]^2)))
summary(dis[3,])
e <- eigen(t(A)%*%A);e
maxp <- dis[, which.max(dis[3,])][1:2];maxp
minp <- dis[, which.min(dis[3,])][1:2];minp
maxv <- rbind(c(0,0), maxp)
minv <- rbind(c(0,0), minp)
lines(maxv, col='Black', type='b')
lines(minv, col='Black', type='b')
e$vectors%*%diag(e$values)
data.frame(maxp, minp)

#PCA
angle <- c(seq(0, 2*pi, by=pi/120))
X <- t(apply(t(angle), 2, function(x) c(cos(x), sin(x))))
plot(X, col='Green', pch=20, xlim=c(-2,3), ylim=c(-1,3), cex=0.1)
A <- matrix(c(1, 0, 1.2, 0.8), ncol=2)
X1 <- sweep(t(A%*%t(X)), 2, c(-1,-2))
plot(X1, col='Green', pch=20, xlim=c(-2,3), ylim=c(-1,3), cex=0.1)

X2 <- scale(X1, center = TRUE, scale = FALSE)
points(X2, col='blue', pch=20, cex=0.1)
s <- svd(X2);s
s$u%*%diag(s$d)%*%t(s$v)
X3 <- s$u%*%diag(s$d)%*%t(s$v)
points(X3, col='Red', pch=20, cex=0.1)

v1 <- rbind(c(0,0), s$v[,1])
v2 <- rbind(c(0,0), s$v[,2])
lines(v1, col='Red', lty=2)
lines(v2, col='Red', lty=2)

#用特征矩阵做正交变换
X4 <- t(s$v)%*%t(X2)
points(t(X4), col='Blue', pch=20, cex=0.1)
abline(h=0, col='Blue', lty=2)
abline(v=0, col='Blue', lty=2)

#用mahalanobis
X5 <- solve(diag(s$d))%*%t(s$v)%*%t(X2)
points(t(X5), col='Blue', pch=20, cex=0.1)

dis<-apply(X5, 2, function(x) c(x[1], x[2], sqrt(x[1]^2+x[2]^2)))
summary(dis[3,])



#特征值与特征向量
eigen(cbind(c(1,-1), c(-1,1)))
eigen(cbind(c(1,-1), c(-1,1)), symmetric = FALSE)
eigen(cbind(1, c(1,-1)), only.values = TRUE)
eigen(cbind( 1, 3:1, 1:3))
A <- t(array(c(1:5, c(8, 6, 10, 9)), dim=c(3,3)))
Sm <- crossprod(A, A)
ev <- eigen(Sm); ev
Sm%*%ev$vectors;ev$vectors%*%diag(ev$values)

#奇异值分解
A <- t(array(c(1:5, c(8, 6, 10, 9)), dim=c(3,3)))
svdA <- svd(A); svdA
attach(svdA)
u %*% diag(d) %*% t(v);A
detach(svdA)

#最小拟合与QR分解
x<-c(0.0, 0.2, 0.4, 0.6, 0.8)
y<-c(0.9, 1.9, 2.8, 3.3, 4.2)
lsfit.sol <- lsfit(x, y);lsfit.sol
d <- cbind(x, y)
lm(y~x,date=d)

#QR分解
X<-matrix(c(rep(1,5), x), ncol=2)
Xplus <- qr(X); Xplus