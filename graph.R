#直方图
#打印多个图
nf<-layout(matrix(c(1,2,3,4,5,6,7,8), 2, 4,  byrow = TRUE), c(1,1,1,1), c(1,1))
layout.show(nf)
hist(iris$Sepal.Length, 10, main='');lines(density(iris$Sepal.Length), col = "blue")
hist(iris$Sepal.Width, 10, main='');lines(density(iris$Sepal.Width), col = "blue")
hist(iris$Petal.Length, 10, main='');lines(density(iris$Petal.Length), col = "blue")
hist(iris$Petal.Width, 10, main='');lines(density(iris$Petal.Width), col = "blue")

hist(iris$Sepal.Length, 20, main='');lines(density(iris$Sepal.Length), col = "blue")
hist(iris$Sepal.Width, 20, main='');lines(density(iris$Sepal.Width), col = "blue")
hist(iris$Petal.Length, 20, main='');lines(density(iris$Petal.Length), col = "blue")
hist(iris$Petal.Width, 20, main='');lines(density(iris$Petal.Width), col = "blue")

#核密度函数
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
hist(w, freq = FALSE)
lines(density(w), col="blue")

plot(density(rep(0, 1000)))

set.seed(10)
dat<-c(rgamma(300,shape=2,scale=2),rgamma(100,shape=10,scale=2))
plot(dat, pch=20, cex=0.1)
hist(dat)

dfn<-function(x,a,alpha1,alpha2,theta){  
  a*dgamma(x,shape=alpha1,scale=theta)+(1-a)*dgamma(x,shape=alpha2,scale=theta)
}  
pfn<-function(x,a,alpha1,alpha2,theta){  
  a*pgamma(x,shape=alpha1,scale=theta)+(1-a)*pgamma(x,shape=alpha2,scale=theta)
}  
curve(dfn(x,0.75,2,10,2),add=T,col="red", xlim=c(-10, 40), ylim=c(-0.01, 0.15)) 
lines(density(dat))

dfn1<-function(x){  
  0.5*dnorm(x,3,1)+0.5*dnorm(x,-3,1)
}  

#画多个图
par(mfrow=c(2,2))  
curve(dfn1(x),from=-6,to=6, , xlim=c(-6, 6), ylim=c(0, 0.2))  
x <- seq(-6, 6, by=0.01)
y <- apply(t(x), 2, function(x1) dfn1(x1))
t <- cbind(x, y)
data<-c(rnorm(200,3,1),rnorm(200,-3,1))  
plot(density(data,bw=8), xlim=c(-6, 6), ylim=c(0, 0.2), col='Blue'); lines(t)
plot(density(data,bw=0.8), xlim=c(-6, 6), ylim=c(0, 0.2), col='Blue') ; lines(t) 
plot(density(data,bw=0.08), xlim=c(-6, 6), ylim=c(0, 0.2), col='Blue'); lines(t)

#手工计算高斯核密度
ker.density=function(x,h){  
  x=sort(x)  
  n=length(x);s=0;t=0;y=0  
  for(i in 2:n)  
    s[i]=0  
  for(i in 1:n){  
    for(j in 1:n)  
      s[i]=s[i]+exp(-((x[i]-x[j])^2)/(2*h*h))  
    t[i]=s[i]  
  }  
  for(i in 1:n)  
    y[i]=t[i]/(n*h*sqrt(2*pi))  
  z=complex(re=x,im=y)  
  hist(x,freq=FALSE)  
  lines(z)  
}  
ker.density(data,0.8)
lines(t, col='Blue') 
lines(density(data,bw=0.8), col='Red')

x <- 44:76
lines(x, dnorm(x, mean(w), sd(w)), col = "red")
#经验分布
plot(ecdf(w),verticals = TRUE, do.p = FALSE)
x <- 44:78
lines(x, pnorm(x, mean(w), sd(w)), col='red')
#手工经验分布图
w_sort <- sort(w)
w1 <- data.frame(r=rank(w_sort), s=w_sort)
ec <- apply(w1, 1, function(x) x[1]/length(w_sort))
lines(c(w_sort[1]-3, unique(w_sort)), c(0, unique(ec)), col='blue', type='b')

#QQ图
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5, 
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
qqnorm(w);qqline(w)
w <- rnorm(100, mean=10, sd=20)
qqnorm(w, pch=20, cex=0.75, col='blue');qqline(w)
w <- runif(100, 0, 100)
qqnorm(w);qqline(w)

y <- rt(200, df = 5)
qqnorm(y); 
qqline(y, col = 2)
#QQ散点图
qqplot(y, rt(300, df = 5))

#手工QQ图:绘制散点图（q,x）
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
w_sort <- sort(w)
p <- (rank(w_sort, ties.method='first')-0.375)/(length(w_sort)+0.25)
qqnorm(w)
points(qnorm(p), w_sort, col='Blue')

#从结果看，R里面应该是用了下面的公式
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
w_sort <- sort(w)
p <- (rank(w_sort, ties.method='first')-0.5)/length(w_sort)
qqnorm(w)
points(qnorm(p), w_sort, col='Blue')
qqline(w)
points(qnorm(p), qnorm(p)*sd(w)+mean(w), col='red')
lines(qnorm(p), qnorm(p)*sd(w)+mean(w), col='red')

#nf<-layout(matrix(1:2, 1, 2, byrow = TRUE), c(1), c(1,1))
qqnorm(w);
points(qnorm(p), w_sort, type = "p", pch='.')
qqline(w)
lm_s <- lm(w_sort~qnorm(p))
s <- summary(lm_s)

#一元线性回归里面提取参数
ab<-s$coefficients[,1]
#看上去，一元线性回归的直线和qqline的还是有些区别的
#问题是qqline是如何做的
abline(ab[1],ab[2])

#程序直接划画pp，qq图
n=100; x<-rnorm(n, mean=50, sd=10) #产生100个正态随机变量
p=pnorm(x, mean(x), sd(x)) #求正态分布函数值（正态累积概率）
t=(rank(x)-1/2)/n#求观察累积概率
q=qnorm(t) #求分位数值
plot(p,t)#画P-P图 
qqnorm(x);qqline(x)
points(q,x, type = "p", pch='.') #画Q-Q图;


a<-rnorm(50);a;min(a);max(a)
qqnorm(a);qqline(a)
#随机性并没有这么美好，即使数据真的来自正态分布，
#你也有可能很容易观察到歪歪扭扭的QQ图，尤其是小样本的情况下
library(animation)
set.seed(710)
ani.options(interval = 0.1, nmax = 50)
par(mar = c(3, 3, 2, 0.5), mgp = c(1.5, 0.5, 0), tcl = -0.3)
sim.qqnorm(n = 30, pch = 19, col = "red", 
           last.plot = expression(abline(0, 1)))

#茎叶图
x<-c(25, 45, 50, 54, 55, 61, 64, 68, 72, 75, 75,
     78, 79, 81, 83, 84, 84, 84, 85, 86, 86, 86,
     87, 89, 89, 89, 90, 91, 91, 92, 100)
sort(x)
stem(x)             #10个数为一段
stem(x, scale=2)    #5个数为一段
stem(x, scale = .5) #20个数为一段

#箱线图 上下伸出的触须最远处为1.5倍四分位数间距 超出这个范围用。表示
boxplot(x) 
quant <- quantile(x)
R1 <- quantile(x, 3/4) - quantile(x, 1/4) 
R <- (quantile(x, 3/4) + quantile(x, 1/4))/2
#points(rep(1, 5), quant)
points(rep(1, 2), c(quantile(x, 1/4) - 1.5*R1, R+1.5*R1))
median(x)
mean(x)

A <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04,
       79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
B <- c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95,
       79.97)
boxplot(A, B, notch=T, names=c('A', 'B'), col=c(2,3))
boxplot(A, B,  names=c('A', 'B'), col=c(3,4))
boxplot(iris)

#另外一种箱线图
y<-c(1600, 1610, 1650, 1680, 1700, 1700, 1780, 1500, 1640,
     1400, 1700, 1750, 1640, 1550, 1600, 1620, 1640, 1600,
     1740, 1800, 1510, 1520, 1530, 1570, 1640, 1600)
f<-factor(c(rep(1,7),rep(2,5), rep(3,8), rep(4,6)))
plot(f,y)
boxplot(y)

#多元散点图
plot(iris[,1:4], pch=20)
df<-data.frame(
  Age=c(13, 13, 14, 12, 12, 15, 11, 15, 14, 14, 14,
        15, 12, 13, 12, 16, 12, 11, 15 ),
  Height=c(56.5, 65.3, 64.3, 56.3, 59.8, 66.5, 51.3,
           62.5, 62.8, 69.0, 63.5, 67.0, 57.3, 62.5,
           59.0, 72.0, 64.8, 57.5, 66.5),
  Weight=c( 84.0, 98.0, 90.0, 77.0, 84.5, 112.0,
            50.5, 112.5, 102.5, 112.5, 102.5, 133.0,
            83.0, 84.0, 99.5, 150.0, 128.0, 85.0,
            112.0),
  Level=c(rep(1, 6), rep(2, 7), rep(3, 6))
)
plot(df)
attach(df)
plot(~Age+Height)
plot(Weight~Age+Height)
detach(df)

#另外几种种多变量散点图
pairs(df)
attach(df)
#在给定的Age下，Weight和Height的散点图
coplot(Weight ~ Height | Age) 
coplot(Weight ~ Height | Age+Level)
detach(df)
VADeaths
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")

#三维图形
x<-seq(0,2800, 400); y<-seq(0,2400,400)

z<-scan()
1180 1320 1450 1420 1400 1300 700 900
1230 1390 1500 1500 1400 900 1100 1060
1270 1500 1200 1100 1350 1450 1200 1150
1370 1500 1200 1100 1550 1600 1550 1380
1460 1500 1550 1600 1550 1600 1600 1600
1450 1480 1500 1550 1510 1430 1300 1200
1430 1450 1470 1320 1280 1200 1080 940

Z<-matrix(z, nrow=8)
#三维图形映象
image(x, y, Z)
#三维图形等值线
contour(x, y, Z, levels = seq(min(z), max(z), by = 80))
#三维图形表面曲线
persp(x, y, Z)
persp(x, y, Z, theta = 30, phi = 45, expand = 0.7)

x<-y<-seq(-2*pi, 2*pi, pi/15)
f<-function(x,y) sin(x)*sin(y)
z<-outer(x, y, f)
contour(x,y,z,col="blue")
persp(x,y,z,theta=30, phi=30, expand=0.7,col="lightblue")

#其他的一些三维3d图
ellipse <- function(a, b, by=0.1) {
  x<-seq(-a, a, by=by)
  y <- sqrt(abs(1-x^2/a^2))*b
  unique(data.frame(x=x, y=y))
}
shape <- ellipse(10, 10, by=0.1)
plot(shape$x, shape$y, lty=1)

#椭球
ellipsoid <- function(a, b, c, by=0.1) {
  x<-seq(-a, a, by=by)
  y<-seq(-b, b, by=by)
  z2<-(1-x^2/a^2-y^2/b^2)*c^2
  i<-which(z2>=0)
  unique(data.frame(x=x[i], y=y[i], z=sqrt(z2[i])))
}
shape <- ellipsoid(10, 10, 10, by=0.5)
x <- shape$x; y<-shape$y; z<-shape$z
persp(x=x,y=y,z=z,theta=30, phi=30, expand=0.7,col="lightblue")

#椭球2 可以旋转
library(scatterplot3d)
ellipsoid <- function(a, b, c, by=0.1) {
  x<-seq(-a, a, by=by)
  limit <- sqrt(abs(1-x^2/a^2))*b
  y<-lapply(limit, function(f) seq(-f, f, by=by))
  x <- unlist(sapply(1:length(x), function(i) rep(x[i], length(y[[i]]))))
  y <- unlist(y)
  z <- round(sqrt(abs(1-x^2/a^2-y^2/b^2))*c,4)
  unique(data.frame(x=x, y=y, z=z))
}

fxy <- function(a, b, by=0.1) {
  x<-seq(-a, a, by=by)
  limit <- sqrt(abs(1-x^2/a^2))*b
  y<-lapply(limit, function(f) seq(0, f, by=by))
  x <- unlist(sapply(1:length(x), function(i) rep(x[i], length(y[[i]]))))
  y <- unlist(y)
  unique(data.frame(x=x, y=y))
}

f <- function(x, y, a, b, c)#实现函数
{
  round(sqrt(abs(1-x^2/a^2-y^2/b^2))*c,4)
}

xy <- fxy(10, 10, by=0.1)
x <- xy$x; y<-xy$y
z <- f(x, y, a=10, b=10, c=10)
plot3e(x, y, z, col="red", size=5)

shape <- ellipsoid(10, 10, 10, by=0.5)
x <- shape$x; y<-shape$y; z<-shape$z
plot3d(x, y, z, col="red", size=5)


#椭球3 
f <- function(x, y, a, b, c)#实现函数
{
  (abs(1-x^2/a^2-y^2/b^2)
}

a <- 10; b<-10; c<-10; by=0.5
x <- seq(-a, a, by=by)
y <- seq(-b, b, by=by)
z2 <- outer(x,y, function(x, y) (1-x^2/a^2-y^2/b^2)*c^2)
z2[z2<0] <- 0
z <- sqrt(z2)
persp(x, y, z, col="red", size=5)
a <- seq(0,100,10)
for (i in a)
{
  for (j in seq(0, 360, 15))
  persp(x,y,z,theta=i,phi=j,expand=0.7,col="blue")#这里让观看的角度每隔一定时间变一次，theta应该是水平方向#的，phi是竖直方向上的角度
  Sys.sleep(1)#让R的运行暂停2秒
}

#R语言实现三维画图http://www.klshu.com/25.html
#3维立体图
x <- seq(-2,3,0.05)#x在区间[-2,3]之间，取值隔0.05取一个
y <- seq(-1,7,0.05)#y在区间[-1,7]之间,取值隔0.05取一个
f <- function(x,y)#实现函数
{
  x^4-2*x^2*y-2*x*y+2*y^2+4.5*x-4*y+4
}
z <- outer(x,y,f)#让x,y在函数f的关系下作外积运算，形成网格，这样才能绘出三维图形
a <- seq(0,100,10)

for (i in a)
{
  persp(x,y,z,theta=i,phi=30,expand=0.7,col="blue")#这里让观看的角度每隔一定时间变一次，theta应该是水平方向#的，phi是竖直方向上的角度
  Sys.sleep(3)#让R的运行暂停三秒
}

#多元数据的图表
#轮廓图
outline <- function(x, txt = TRUE){
  if (is.data.frame(x) == TRUE)
    x <- as.matrix(x)
  m <- nrow(x); n <- ncol(x)
  plot(c(1,n), c(min(x),max(x)), type = "n",
       main = "The outline graph of Data",
       xlab = "Number", ylab = "Value")
  for(i in 1:m){
    lines(x[i,], col=i)
    if (txt == TRUE){
      k <- dimnames(x)[[1]][i]
      text(1+(i-1)%%n, x[i,1+(i-1)%%n], k)
    }
  }
}

X<-read.table("course.data")
#采用轮廓图能够直观的看出哪些学生成绩相似，优秀。对于课程而言，能看出课程的好坏和分散程度
outline(X)
outline(t(as.matrix(X)))

cols <- apply(t(iris[,5]), 2, function(x, lels) which(levels(iris[,5])==x))
outline(iris[,1:4])

#星图/雷达图
#很容易看出哪些学生成绩好，差，比较均衡，哪些偏科
stars(X)
stars(X, full=FALSE, draw.segments = TRUE,
      key.loc = c(5,0.5), mar = c(2,0,0,0))

stars(iris[,1:4])

#调和曲线图：将多维空间的点，对应于二维平面的一条曲线
unison <- function(x){
  if (is.data.frame(x) == TRUE)
    x <- as.matrix(x)
  t <- seq(-pi, pi, pi/30)
  m <- nrow(x); n<-ncol(x)
  f <- array(0, c(m,length(t)))
  for(i in 1:m){
    f[i,] <- x[i,1]/sqrt(2)
    for( j in 2:n){
      if (j%%2 == 0)
        f[i,] <- f[i,]+x[i,j]*sin(j/2*t)
      else
        f[i,] <- f[i,]+x[i,j]*cos(j%/%2*t)
    }
  }
  plot(c(-pi,pi), c(min(f), max(f)), type = "n",
       main = "The Unison graph of Data",
       xlab = "t", ylab = "f(t)")
  for(i in 1:m) lines(t, f[i,] , col = i)
}

#Chernoff Faces
library(aplpack)
faces(iris[1:10,,1:4])
library(TeachingDemos)
faces2(iris[1:4])

#调和曲线常用于聚类。相同类的曲线会缠绕在一起，不同类形成不同束，非常直观
unison(X)

#使用R语言画圆弧条形图
country<-c("老挝","菲律宾","尼泊尔","孟加拉","阿富汗","新加坡","泰国","蒙古","新西兰","印度尼西亚","印度","澳大利亚","台湾","日本","香港","柬埔寨","马来西亚","巴基斯坦","韩国","斯里兰卡","中国","越南")  
percent<-c(90,81,80,77,75,74,73,72,68,68,68,67,65,63,61,60,59,58,53,51,49,48)  
d<-data.frame(country,percent)  
png("d:\\test.png",width = 2048, height = 2048)  
f<-function(name,value){  
  
  xsize=200  
  plot(0, 0,xlab="",ylab="",axes=FALSE,xlim=c(-xsize,xsize),ylim=c(-xsize,xsize))  
  for(i in 1:length(name)){  
    info = name[i]  
    percent = value[i]  
    k = (1:(360*percent/100)*10)/10  
    r=xsize*(length(name)-i+1)/length(name)  
    #print(r)  
    x=r*sin(k/180*pi)  
    y=r*cos(k/180*pi)  
    text(-18,r,info,pos=2,cex=3)  
    text(-9,r,paste(percent,"%"),cex=3)  
    lines(x,y,col="red")  
  }  
}  

f(country,percent)  
dev.off()  
