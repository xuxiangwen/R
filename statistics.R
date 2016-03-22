#正态分布
data <- rnorm(100, mean=0, sd=1);data               #仿真函数，生成符合正太分布的数据
dnorm(2, mean=0, sd=1, log = FALSE)                           #密度函数
pnorm(2, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)      #分布函数
qnorm(0.977, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)       #分布函数的逆函数

#泊松分布
dpois(x, lambda, log = FALSE)
ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)
rpois(n, lambda)
#lambda是possi分布中的λ

#-------------------------------------------------
#正态性检验
#-------------------------------------------------

#非参数检验方法包括Kolmogorov-Smirnov检验（D检验）和Shapiro- Wilk （W 检验）。  
#SAS中规定：当样本含量n ≤2000时，结果以Shapiro – Wilk（W 检验）为准，
#当样本含量n >2000 时，结果以Kolmogorov – Smirnov（D 检验）为准。  
#SPSS中则这样规定：
#（1）如果指定的是非整数权重，则在加权样本大小位于3和50之间时，
#计算 Shapiro-Wilk 统计量。对于无权重或整数权重，
#在加权样本大小位于3 和 5000 之间时，计算该统计量。
#由此可见，部分SPSS教材里面关于“Shapiro – Wilk适用于样本量3-50之间的数据”
#的说法是在是理解片面，误人子弟。
#（2）单样本 Kolmogorov-Smirnov 检验可用于检验变量（例如income）是否为正态分布。
#对于此两种检验，如果P值大于0.05，表明资料服从正态分布。
#http://blog.csdn.net/zzminer/article/details/8858469
#http://blog.sina.com.cn/s/blog_403aa80a01019lwd.html
#http://blog.sciencenet.cn/blog-113790-630979.html

#Shapiro–Wilk 夏皮罗-威尔克
#当p值小于某个某个显著水平（比如0.05）,则认为样本不符合来自正态分布的总体
#容易受异常值影响
x <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
shapiro.test(x)
#统计量W 最大值是1，最小值是n*(a1)^2/(n-1)
#w值越高，越表示样本与正态分布匹配。
#但也存在w很大，p值很小的情况，这样需要拒绝正态分布的假设
shapiro.test(runif(100, min = 2, max = 4))

#分布拟合检验Kolmogorov-Smirnov
#http://www.dataguru.cn/article-3089-1.html
x<-rt(100,5)
ks.test(x, "pf",2,5)
ks.test(rnorm(100),rnorm(50))
ks.test(rnorm(100),"pnorm")
ks.test(rnorm(100),"punif")
ks.test(rnorm(100, 10, 20),"pnorm", 10, 20)

#手工算Kolmogorov-Smirnov
#原假设：糖浓度服从正态分布
blood <- data.frame(
  sugar = c(68, 72, 75, 76, 77, 78, 80, 81, 84, 86, 87, 92),
  freq = c(2, 2, 2, 2, 6, 3, 6, 3, 2, 2, 2, 3)
)  
blood$cumfreq <- cumsum(blood$freq) 
blood$Fn <- blood$cumfreq/sum(blood$freq)
(blood$sugar-mean(blood$sugar))/sd(blood$sugar)
blood$z <- (blood$sugar-80)/6
blood$F0 <- pnorm(blood$z)
blood$delta <- blood$Fn - blood$F0
max(blood$delta)
ks.test(blood$sugar,blood$freq)
ks.test(blood$sugar,"pnorm")
ks.test(blood$sugar,"pnorm")
ks.test(blood$sugar,"pnorm")

blood$cumfreq <- cumsum(blood$freq) 
blood$Fn <- blood$cumfreq/sum(blood$freq)
b <- rep(blood$sugar, blood$freq)
blood$p<-pnorm((blood$sugar-mean(b))/sd(b))
blood$delta <- blood$Fn - blood$p

#-------------------------------------------------
# 方差分析
#-------------------------------------------------
#一元方差分析
#1
f.test <- function(alpha, n1, n2, s21, s22){
  q<-s21/s22
  p<- pf(q, n1, n2)        
  print(paste("p =", round(p, 4)))    
  print(paste("q =", round(q, 4)))
  
  q_alpha<-qf(alpha, n1, n2)     
  print(paste("q(alpha) =", round(q_alpha, 4)))
  if (p>alpha)
    print("reject")  
  else
    print("pass")
}
esti.mu1_mu2 <- function(alpha, x1, x2, se, n_s){
  m1 <- mean(x1)
  m2 <- mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  c(
    m1-m2+qt((1-alpha)/2, n_s)*(se/n_s)^0.5 *(1/n1+1/n2)^0.5, 
    m1-m2-qt((1-alpha)/2, n_s)*(se/n_s)^0.5 *(1/n1+1/n2)^0.5
  )
}
a <- data.frame(A=c(40, 42, 48, 45, 38), 
                B=c(26, 28, 34, 32, 30), 
                C=c(39, 50, 40, 50, 43))
m <- as.matrix(a)

st <- sum((m-mean(m))^2)
se <- sum(sweep(m, 2, colMeans(m))^2)
sa <- sum(apply(m, 2, function(x) (mean(x)-mean(m))^2*length(x)))

#1.1
alpha <- 0.95
n1 <- dim(m)[2]-1
n2 <- length(m) - dim(m)[2]
f.test(alpha, n1, n2, sa/n1, se/n2)

#1.2
ma_mb <- esti.mu1_mu2(0.95, m[,1], m[,2], se, length(m)-dim(m)[2])
ma_mc <- esti.mu1_mu2(0.95, m[,1], m[,3], se, length(m)-dim(m)[2])
mb_mc <- esti.mu1_mu2(0.95, m[,2], m[,3], se, length(m)-dim(m)[2])
ma_mb
ma_mc
mb_mc

#2
F1 <- c(14, 13, 9, 15, 11, 13, 14, 11)
F2 <- c(10, 12, 7, 11, 8, 12, 9, 10, 13, 9, 10, 9)
F3 <- c(11, 5, 9, 10, 6, 8, 8, 7)
F <- c(F1, F2, F3)

st <- sum((F1 - mean(F))^2) + sum((F2 - mean(F))^2) + sum((F3 - mean(F))^2)
se <- sum((F1 - mean(F1))^2) + sum((F2 - mean(F2))^2) + sum((F3 - mean(F3))^2)
sa <- sum((mean(F1)-mean(F))^2*length(F1)) + sum((mean(F2)-mean(F))^2*length(F2)) + sum((mean(F3)-mean(F))^2*length(F3))

#2.1
alpha <- 0.95
n1 <- 3-1
n2 <- length(F) - 3
f.test(alpha, n1, n2, sa/n1, se/n2)

#2.2
m1_m2 <- esti.mu1_mu2(0.95, F1, F2, se, length(F)-3)
m1_m3 <- esti.mu1_mu2(0.95, F1, F3, se, length(F)-3)
m2_m3 <- esti.mu1_mu2(0.95, F2, F3, se, length(F)-3)
m1_m2
m1_m3
m2_m3

#3
a <- data.frame(A1=c(192, 189, 176, 185, 190), 
                A2=c(190, 201, 187, 196, 200), 
                A3=c(188, 179, 191, 183, 194),
                A4=c(187, 180, 188, 175, 182))
m <- as.matrix(a)

st <- sum((m-mean(m))^2)
se <- sum(sweep(m, 2, colMeans(m))^2)
sa <- sum(apply(m, 2, function(x) (mean(x)-mean(m))^2*length(x)))

alpha <- 0.95
n1 <- dim(m)[2]-1
n2 <- length(m) - dim(m)[2]
f.test(alpha, n1, n2, sa/n1, se/n2)

#4
F1 <- c(8, 6, 4, 2)
F2 <- c(6, 6, 4, 4)
F3 <- c(8, 10, 10, 10, 12)
F4 <- c(4, 4, 2)
F <- c(F1, F2, F3, F4)

st <- sum((F1 - mean(F))^2) + sum((F2 - mean(F))^2) + sum((F3 - mean(F))^2) + sum((F4 - mean(F))^2)
se <- sum((F1 - mean(F1))^2) + sum((F2 - mean(F2))^2) + sum((F3 - mean(F3))^2) + sum((F4 - mean(F4))^2)
sa <- sum((mean(F1)-mean(F))^2*length(F1)) + sum((mean(F2)-mean(F))^2*length(F2))  + sum((mean(F3)-mean(F))^2*length(F3))+ sum((mean(F4)-mean(F))^2*length(F4))

alpha <- 0.95
n1 <- 4-1
n2 <- length(F) - 4
f.test(alpha, n1, n2, sa/n1, se/n2)

#5
a <- data.frame(A1=c(29.6, 24.3, 28.5, 32), 
                A2=c(27.3, 32.6, 30.8, 34.8), 
                A3=c(5.8, 6.2, 11.0, 8.3),
                A4=c(21.6, 17.4, 18.3, 19.0),
                A5=c(29.2, 32.8, 25, 24.2))
m <- as.matrix(a)

st <- sum((m-mean(m))^2)
se <- sum(sweep(m, 2, colMeans(m))^2)
sa <- sum(apply(m, 2, function(x) (mean(x)-mean(m))^2*length(x)))

alpha <- 0.95
n1 <- dim(m)[2]-1
n2 <- length(m) - dim(m)[2]

f.test(alpha, n1, n2, sa/n1, se/n2)

#二元方差分析
#6
a <- array(c(14, 10, 11, 11, 13, 9, 10, 12,
             9,  7, 10,  8,  7, 11, 6, 10,
             5, 11, 13, 14, 12, 13,14, 10), c(2, 4, 3)
);a
st <- sum((a-mean(a))^2)
se <- sum(apply(a, c(2,3), function(x) (x-mean(x))^2))
sa <- dim(a)[1]*dim(a)[2]*sum(apply(a, 3, function(x) (mean(x)-mean(a))^2))
sb <- dim(a)[1]*dim(a)[3]*sum(apply(a, 2, function(x) (mean(x)-mean(a))^2))

ab<-function(x){
  x1 <- apply(x, c(3,2), mean)
  x2 <- sweep(x1, 2, apply(x1, 2, mean))
  x3 <- sweep(x2, 1, apply(x1, 1, mean))
  x4 <- x3 + mean(x1)
  return(x4)    
}
sab <- dim(a)[1]*sum(ab(a)^2)

alpha <- 0.95
n1 <- dim(a)[3]-1
n2 <- dim(a)[2]*dim(a)[3]*(dim(a)[1]-1)
f.test(alpha, n1, n2, sa/n1, se/n2)

alpha <- 0.95
n1 <- dim(a)[2]-1
n2 <- dim(a)[2]*dim(a)[3]*(dim(a)[1]-1)
f.test(alpha, n1, n2, sb/n1, se/n2)

alpha <- 0.95
n1 <- (dim(a)[2]-1)*(dim(a)[3]-1)
n2 <- dim(a)[2]*dim(a)[3]*(dim(a)[1]-1)
f.test(alpha, n1, n2, sab/n1, se/n2)

#因为交互作用效应不明显，将sab+se作为新的se
alpha <- 0.95
n1 <- dim(a)[3]-1
n2 <- dim(a)[2]*dim(a)[3]*(dim(a)[1]-1)+(dim(a)[2]-1)*(dim(a)[3]-1)
f.test(alpha, n1, n2, sa/n1, (se+sab)/n2)

alpha <- 0.95
n1 <- dim(a)[2]-1
n2 <- dim(a)[2]*dim(a)[3]*(dim(a)[1]-1)+(dim(a)[2]-1)*(dim(a)[3]-1)
f.test(alpha, n1, n2, sb/n1, (se+sab)/n2)

#7
a <- array(c(1.63, 1.35, 1.27,
             1.34, 1.30, 1.22,
             1.19, 1.14, 1.27,
             1.30, 1.09, 1.32), c(1, 3, 4)
);a
st <- sum((a-mean(a))^2)
se <- sum(apply(a, c(2,3), function(x) (x-mean(x))^2))
sa <- dim(a)[1]*dim(a)[2]*sum(apply(a, 3, function(x) (mean(x)-mean(a))^2))
sb <- dim(a)[1]*dim(a)[3]*sum(apply(a, 2, function(x) (mean(x)-mean(a))^2))

ab<-function(x){
  x1 <- apply(x, c(3,2), mean)
  x2 <- sweep(x1, 2, apply(x1, 2, mean))
  x3 <- sweep(x2, 1, apply(x1, 1, mean))
  x4 <- x3 + mean(x1)
  return(x4)    
}
sab <- dim(a)[1]*sum(ab(a)^2)
sa+sb+se+sab;st

alpha <- 0.95
n1 <- dim(a)[3]-1
n2 <- (dim(a)[2]-1)*(dim(a)[3]-1)
f.test(alpha, n1, n2, sa/n1, (se+sab)/n2)

alpha <- 0.95
n1 <- dim(a)[2]-1
n2 <- (dim(a)[2]-1)*(dim(a)[3]-1)
f.test(alpha, n1, n2, sb/n1, (se+sab)/n2)

#############################################
# 一元线性回归
#############################################
#8
d <- data.frame(x=c(300, 400, 500, 600, 700, 800), 
                y=c(40, 50, 55, 60, 67, 70))
m <- as.matrix(d)
plot(m)
sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2)
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])))
b <- sxy/sxx
a <- mean(m[,2]) - b*mean(m[,1])
a;b;
a+b*300;

#9
d <- data.frame(x=c(0.1, 0.3, 0.4, 0.55, 0.7, 0.8, 0.95), 
                y=c(15, 18, 19, 21, 22.6, 23.8, 26))
m <- as.matrix(d)
#9.1
plot(m)
#9.2
sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2)
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])))
b <- sxy/sxx
a <- mean(m[,2]) - b*mean(m[,1])
a;b;
a+b*0.1;
#9.3
syy <- sum(sweep(t(m[,2]), 1, mean(m[,2]))^2)
Qe <- syy - b*sxy
sigma2 <- Qe/(dim(m)[1]-2);sigma2
#9.4
n <- dim(m)[1]-2
q <- b*sqrt(sxx)/sqrt(sigma2)
t.test <- function(alpha, n, q){
  p<-pt(q,n)        
  print(paste("freedom =", n))
  print(paste("p =", round(p, 4)))    
  print(paste("q =", round(q, 4)))
  q_1_alpha2<-qt((1+alpha)/2, n)                    
  q_alpha2<-qt((1-alpha)/2, n)     
  print(paste("q(alpha/2) =", round(q_alpha2, 4)))
  print(paste("q(1-alpha/2) =", round(q_1_alpha2, 4)))
  if (p>1-alpha/2 || p<alpha/2)
    print("reject")
  else
    print("pass")
}
t.test(0.95, n, q)

#9.5
s <- sqrt(sigma2)/sqrt(sxx)
n <- dim(m)[1]-2
t.esti <- function(alpha, n, b, s){
  c(
    b+qt((1-alpha)/2, n)*s, 
    b-qt((1-alpha)/2, n)*s
  )
}
t.esti(0.95, n, b, s)

#9.6
x0 <- 0.5
y0 <- a+b*x0
s <- sqrt(sigma2)*sqrt(1/dim(m)[1]+(x0-mean(m[,1]))^2/sxx)
n <- dim(m)[1]-2
t.esti(0.95, n, y0, s)

#9.7
x0 <- 0.5
y0 <- a+b*x0
s <- sqrt(sigma2)*sqrt(1+1/dim(m)[1]+(x0-mean(m[,1]))^2/sxx)
n <- dim(m)[1]-2
t.esti(0.95, n, y0, s)

#10
d <- data.frame(x=c(17.1, 10.5, 13.8, 15.7, 11.9, 10.4, 15.0, 16.0, 17.8, 
                    15.8, 15.1, 12.1, 18.4, 17.1, 16.7, 16.5, 15.1, 15.1), 
                y=c(16.7, 10.4, 13.5, 15.7, 11.6, 10.2, 14.5, 15.8, 17.6,
                    15.2, 14.8, 11.9, 18.3, 16.7, 16.6, 15.9, 15.1, 14.5))
m <- as.matrix(d)
#10.1
plot(m)
#10.2
sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2)
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])))
b <- sxy/sxx
a <- mean(m[,2]) - b*mean(m[,1])
a;b;
a+b*0.1;
#10.3
x0 <- 14
y0 <- a+b*x0

syy <- sum(sweep(t(m[,2]), 1, mean(m[,2]))^2)
Qe <- syy - b*sxy
sigma2 <- Qe/(dim(m)[1]-2);sigma2

s <- sqrt(sigma2)*sqrt(1+1/dim(m)[1]+(x0-mean(m[,1]))^2/sxx)
n <- dim(m)[1]-2
t.esti(0.95, n, y0, s)

#11
d <- data.frame(x=c(20.0, 16.0, 19.8, 18.4, 17.1, 15.5, 14.7, 17.1,
                    15.4, 16.2, 15.0, 17.2, 16.0, 17.0, 14.4), 
                y=c(31.4, 22.0, 34.1, 29.1, 27.0, 24.0, 20.9, 27.8,
                    20.8, 28.5, 26.4, 28.1, 27.0, 28.6, 24.6))
m <- as.matrix(d)
#11.1
plot(m)
#11.2
sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2)
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])))
b <- sxy/sxx
a <- mean(m[,2]) - b*mean(m[,1])
a;b;
a+b*20.0;

#12
d <- data.frame(x=c(1952, 1956, 1960, 1964, 1968, 1972, 1976, 
                    1980, 1984, 1988, 1992, 1996, 2000, 2004), 
                y=c(29.3, 28.8, 28.5, 28.4, 29.4, 27.6, 27.7, 
                    27.7, 27.8, 27.4, 27.8, 27.1, 27.3, 27.1))
d$x <- 1:dim(d)
d$y <- d$y - 20
m <- as.matrix(d)

#12.1
plot(m)
sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2);sxx
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])));sxy
b <- sxy/sxx;b
a <- mean(m[,2]) - b*mean(m[,1]);a
a;b;
a+b*1;

#12.2
syy <- sum(sweep(t(m[,2]), 1, mean(m[,2]))^2)
Qe <- syy - b*sxy
sigma2 <- Qe/(dim(m)[1]-2);sigma2

n <- dim(m)[1]-2
q <- b*sqrt(sxx)/sqrt(sigma2)
t.test(0.95, n, q)

#12.3
x0 <- 15
y0 <- a+b*x0;y0+20

#关于年份
d <- data.frame(x=c(1952, 1956, 1960, 1964, 1968, 1972, 1976, 
                    1980, 1984, 1988, 1992, 1996, 2000, 2004), 
                y=c(29.3, 28.8, 28.5, 28.4, 29.4, 27.6, 27.7, 
                    27.7, 27.8, 27.4, 27.8, 27.1, 27.3, 27.1))
#d$x <- 1:dim(d)
#d$y <- d$y - 20
m <- as.matrix(d)

#关于年份
plot(m)
sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2);sxx
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])));sxy
b <- sxy/sxx;b
a <- mean(m[,2]) - b*mean(m[,1]);a
a;b;
a+b*1;

#13
d <- data.frame(x=c(9, 8.5, 9.25, 9.75, 9.0, 10.00, 9.5, 9.0,
                    9.25, 9.5, 9.25, 10.00, 10.00, 9.75, 9.5), 
                y=c(6.5, 6.25, 7.25, 7.00, 6.75, 7, 6.5, 7.0, 
                    7.00, 7.00, 7.00, 7.5, 7.25, 7.25, 7.25))
m <- as.matrix(d)

#13.1
plot(m)
sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2);sxx
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])));sxy
b <- sxy/sxx;b
a <- mean(m[,2]) - b*mean(m[,1]);a
a;b;
a+b*9;

#13.2
n <- dim(m)[1]-2
q <- b*sqrt(sxx)/sqrt(sigma2)
t.test(0.95, n, q)

syy <- sum(sweep(t(m[,2]), 1, mean(m[,2]))^2)
Qe <- syy - b*sxy
sigma2 <- Qe/(dim(m)[1]-2);sigma2
s <- sqrt(sigma2)/sqrt(sxx)
n <- dim(m)[1]-2
t.esti(0.95, n, b, s)

#14
d <- data.frame(x=c(rep(3, 3), rep(4, 3), rep(9, 3),
                    rep(15, 3), rep(40, 2)),
                y=c(28, 33, 22, 10, 36, 24, 15, 22, 10,
                    6, 14, 9, 1, 1))
m <- as.matrix(d)

#14.1
plot(m)
#14.2
m[, 2] <- log(m[, 2])
plot(m)

sxx <- sum(sweep(t(m[,1]), 1, mean(m[,1]))^2);sxx
sxy <- sum(sweep(t(m[,1]), 1, mean(m[,1]))*sweep(t(m[,2]), 1, mean(m[,2])));sxy
b <- sxy/sxx;b
a <- mean(m[,2]) - b*mean(m[,1]);a
a;b;
exp(a)
a+b*3;

#15
x <- data.frame(
  x1=rep(1, 15),
  x2=c(rep(10, 3), rep(15, 3), rep(20, 3), 
       rep(25, 3), rep(30, 3)),
  x3=c(rep(10, 3), rep(15, 3), rep(20, 3), 
       rep(25, 3), rep(30, 3))^2
)
x;
y=c(25.2, 27.3, 28.7, 29.8, 31.1, 27.8, 31.2, 32.6, 29.7,
    31.7, 30.1, 32.3, 29.4, 30.8, 32.8)
x <- as.matrix(x)
x
#15.1
plot(x[,2], y)
#15.2
b <- solve(t(x)%*%x)%*%t(x)%*%y; b

#16
x <- data.frame(
  x0=c( 1,  1,  1,  1,  1,  1,  1,  1),
  x1=c(-1, -1, -1, -1,  1,  1,  1,  1),
  x2=c(-1, -1,  1,  1, -1, -1,  1,  1),
  x3=c(-1,  1, -1,  1, -1,  1, -1,  1)
)
x;
y=c(7.6, 10.3, 9.2, 10.2, 8.4, 11.1, 9.8, 12.6)
x <- as.matrix(x)
x
#16.1
b <- solve(t(x)%*%x)%*%t(x)%*%y; b
#16.2？
x <- data.frame(
  x0=c( 1,  1,  1,  1,  1,  1,  1,  1),
  x1=c(-1, -1, -1, -1,  1,  1,  1,  1),
  x3=c(-1,  1, -1,  1, -1,  1, -1,  1)
)
x <- as.matrix(x)
x
b <- solve(t(x)%*%x)%*%t(x)%*%y; b

###################################
#参数估计
###################################
#点估计：矩估计法和最大似然法各有利弊。矩法简单，不需要知道总体的分布，
#而极大似然需要知道总体的分布形式

#估计量三大准则：
#无偏性：估计量的数学期望应该等于未知参数的真值。
#有效性：估计量的取值更加靠近真值，也就是方差更小
#一致性（相合性）：当n趋近无穷大的时候，估计量应该趋紧于真值

#矩估计法：如果能用，则计算简单，但相对其他估计方法来说，效率较低
#R Model: Page 211
moment_fun<-function(p){
  f<-c(p[1]*p[2]-A1, p[1]*p[2]-p[1]*p[2]^2-M2)
  J<-matrix(c(p[2], p[1], p[2]-p[2]^2, p[1]-2*p[1]*p[2]),
            nrow=2, byrow=T)
  list(f=f, J=J)
}

#牛顿法
x<-rbinom(100, 20, 0.7); n<-length(x)
A1<-mean(x); M2<-(n-1)/n*var(x)
p<-c(10,0.5); Newtons(moment_fun, p)
#用公式法
N <- (A1-M2)/A1; P<-A1^2/(A1-M2)
N;P

#最大似然估计法
#R Model: Page 218
#Cauchy分布
x <- rcauchy(1000,1)
f <- function(p) sum((x-p)/(1+(x-p)^2))
out <- uniroot(f, c(0, 5))
out

#optimize求解一维变量函数的极小值。
#用似然方程根可能更准确一些，但需要先求导数
loglike <- function(p) sum(log(1+(x-p)^2))
out <- optimize(loglike, c(0, 5))
out 


#多元函数求解最小值。nlm看上去是用梯度下降法求解极值。
obj<-function(x){
  f<-c(10*(x[2]-x[1]^2), 1-x[1])
  sum(f^2)
}

x0<-c(-1.2,1); nlm(obj,x0)


#区间估计
#均值u的区间估计
#方差已知，用正态分布
#方差未知，用t分布
interval_estimate1<-function(x, sigma=-1, alpha=0.05){
  n<-length(x); xb<-mean(x)
  if (sigma>=0){
    tmp<-sigma/sqrt(n)*qnorm(1-alpha/2); df<-n
  }
  else{
    tmp<-sd(x)/sqrt(n)*qt(1-alpha/2,n-1); df<-n-1
  }
  data.frame(mean=xb, df=df, a=xb-tmp, b=xb+tmp)
}

#单侧置信区间
#side=0 双侧，side<0 求上限，side>0 求下限
interval_estimate4<-function(x, sigma=-1, side=0, alpha=0.05){
  n<-length(x); xb<-mean(x)
  if (sigma>=0){
    if (side<0){
      tmp<-sigma/sqrt(n)*qnorm(1-alpha)
      a <- -Inf; b <- xb+tmp
    }
    else if (side>0){
      tmp<-sigma/sqrt(n)*qnorm(1-alpha)
      a <- xb-tmp; b <- Inf
    }
    else{
      tmp <- sigma/sqrt(n)*qnorm(1-alpha/2)
      a <- xb-tmp; b <- xb+tmp
    }
    df<-n
  }
  else{
    if (side<0){
      tmp <- sd(x)/sqrt(n)*qt(1-alpha,n-1)
      a <- -Inf; b <- xb+tmp
    }
    else if (side>0){
      tmp <- sd(x)/sqrt(n)*qt(1-alpha,n-1)
      a <- xb-tmp; b <- Inf
    }
    else{
      tmp <- sd(x)/sqrt(n)*qt(1-alpha/2,n-1)
      a <- xb-tmp; b <- xb+tmp
    }
    df<-n-1
  }
  data.frame(mean=xb, df=df, a=a, b=b)
}


X<-c(14.6, 15.1,14.9, 14.8, 15.2,15.1)
interval_estimate1(X, sigma=0.2)
interval_estimate1(X, sigma=-1)

X<-c(10.1, 10, 9.8, 10.5, 9.7, 10.1, 9.9, 10.2, 10.3, 9.9)
interval_estimate1(X)
t.test(X)

X<-c(1050, 1100, 1120, 1250, 1280)
interval_estimate4(X, side=1)
t.test(X, alternative = "greater")

#方差的区间估计
#u已知，用卡方分布, 自由度n
#u未知，用卡方分布, 自由度n-1
interval_var1<-function(x, mu=Inf, alpha=0.05){
  n<-length(x)
  if (mu<Inf){
    S2 <- sum((x-mu)^2)/n; df <- n
  }
  else{
    S2 <- var(x); df <- n-1
  }
  a<-df*S2/qchisq(1-alpha/2,df)
  b<-df*S2/qchisq(alpha/2,df)
  data.frame(var=S2, df=df, a=a, b=b)
}

#单侧置信区间
#side=0 双侧，side<0 求上限，side>0 求下限
interval_var3<-function(x,mu=Inf,side=0,alpha=0.05){
  n<-length(x)
  if (mu<Inf){
    S2<-sum((x-mu)^2)/n; df<-n
  }
  else{
    S2<-var(x); df<-n-1
  }
  if (side<0){
    a<-0
    b <- df*S2/qchisq(alpha,df)
  }
  else if (side>0){
    a <- df*S2/qchisq(1-alpha,df)
    b <- Inf
  }
  else{
    a<-df*S2/qchisq(1-alpha/2,df)
    b<-df*S2/qchisq(alpha/2,df)
  }
  data.frame(var=S2, df=df, a=a, b=b)
}

X<-c(10.1,10,9.8,10.5,9.7,10.1,9.9,10.2,10.3,9.9)
interval_var1(X, mu=10)
interval_var1(X)

X<-c(10.1,10,9.8,10.5,9.7,10.1,9.9,10.2,10.3,9.9)
interval_var3(X, side=-1)
interval_var1(X, alpha=0.1)


#两个均值差的区间估计
#两个方差已知，用正态分布
#方差未知，但相同，用t分布
#方差未知，但不同，用样本方差替换方差，用t分布
interval_estimate2<-function(x, y,
                             sigma=c(-1,-1), var.equal=FALSE, alpha=0.05){
  n1<-length(x); n2<-length(y)
  xb<-mean(x); yb<-mean(y)
  if (all(sigma>=0)){
    tmp<-qnorm(1-alpha/2)*sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
    df<-n1+n2
  }
  else{
    if (var.equal == TRUE){
      Sw<-((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2)
      tmp<-sqrt(Sw*(1/n1+1/n2))*qt(1-alpha/2,n1+n2-2)
      df<-n1+n2-2
    }
    else{
      S1<-var(x); S2<-var(y)
      nu<-(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
      tmp<-qt(1-alpha/2, nu)*sqrt(S1/n1+S2/n2)
      df<-nu
    }
  }
  data.frame(mean=xb-yb, df=df, a=xb-yb-tmp, b=xb-yb+tmp)
}

#单侧置信区间
#side=0 双侧，side<0 求上限，side>0 求下限
interval_estimate5<-function(x, y,
                             sigma=c(-1,-1), var.equal=FALSE, side=0, alpha=0.05){
  n1<-length(x); n2<-length(y)
  xb<-mean(x); yb<-mean(y); zb<-xb-yb
  if (all(sigma>=0)){
    if (side<0){
      tmp<-qnorm(1-alpha)*sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
      a <- -Inf; b <- zb+tmp
    }
    else if (side>0){
      tmp<-qnorm(1-alpha)*sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
      a <- zb-tmp; b <- Inf
    }
    else{
      tmp<-qnorm(1-alpha/2)*sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
      a <- zb-tmp; b <- zb+tmp
    }
    df<-n1+n2
  }
  else{
    if (var.equal == TRUE){
      Sw<-((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2)
      if (side<0){
        tmp<-sqrt(Sw*(1/n1+1/n2))*qt(1-alpha,n1+n2-2)
        a <- -Inf; b <- zb+tmp
      }
      else if (side>0){
        tmp<-sqrt(Sw*(1/n1+1/n2))*qt(1-alpha,n1+n2-2)
        a <- zb-tmp; b <- Inf
      }
      else{
        tmp<-sqrt(Sw*(1/n1+1/n2))*qt(1-alpha/2,n1+n2-2)
        a <- zb-tmp; b <- zb+tmp
      }
      df<-n1+n2-2
    }
    else{
      S1<-var(x); S2<-var(y)
      nu<-(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
      if (side<0){
        tmp<-qt(1-alpha, nu)*sqrt(S1/n1+S2/n2)
        a <- -Inf; b <- zb+tmp
      }
      else if (side>0){
        tmp<-qt(1-alpha, nu)*sqrt(S1/n1+S2/n2)
        a <- zb-tmp; b <- Inf
      }
      else{
        tmp<-qt(1-alpha/2, nu)*sqrt(S1/n1+S2/n2)
        a <- zb-tmp; b <- zb+tmp
      }
      df<-nu
    }
  }
  data.frame(mean=zb, df=df, a=a, b=b)
}

x<-rnorm(100, 5.32, 2.18)
y<-rnorm(100, 5.76, 1.76)
interval_estimate2(x,y, sigma=c(2.18, 1.76))
interval_estimate2(x,y, var.equal=T)
interval_estimate2(x,y, var.equal=F)

x<-rnorm(12, 501.1, 2.4)
y<-rnorm(17, 499.7, 4.7)
interval_estimate2(x, y, var.equal=TRUE)
interval_estimate2(x, y)
t.test(x, y)

interval_estimate5(x, y, side=0)
interval_estimate5(x, y, side=-1)
interval_estimate2(x, y, alpha=0.1)

#配对数据的区间估计,其实和普通的mu区间估计相同
#查看x和y的每一组数据是否有差异
X<-c(11.3, 15.0, 15.0, 13.5, 12.8, 10.0, 11.0, 12.0, 13.0, 12.3)
Y<-c(14.0, 13.8, 14.0, 13.5, 13.5, 12.0, 14.7, 11.4, 13.8, 12.0)
t.test(X-Y)
t.test(X, Y)

#方差之比（sigma1^2/sigma2^2）的区间估计
#u1,u2已知，用F(n1, n2)分布
#u1,u2未知，用F(n1-1, n2-1)分布
interval_var2<-function(x,y,
                        mu=c(Inf, Inf), alpha=0.05){
  n1<-length(x); n2<-length(y)
  if (all(mu<Inf)){
    Sx2<-1/n1*sum((x-mu[1])^2); Sy2<-1/n2*sum((y-mu[2])^2)
    df1<-n1; df2<-n2
  }
  else{
    Sx2<-var(x); Sy2<-var(y); df1<-n1-1; df2<-n2-1
  }
  r<-Sx2/Sy2
  a<-r/qf(1-alpha/2,df1,df2)
  b<-r/qf(alpha/2,df1,df2)
  data.frame(rate=r, df1=df1, df2=df2, a=a, b=b)
}

interval_var4<-function(x,y,
                        mu=c(Inf, Inf), side=0, alpha=0.05){
  n1<-length(x); n2<-length(y)
  if (all(mu<Inf)) {
    Sx2<-1/n1*sum((x-mu[1])^2); df1<-n1
    Sy2<-1/n2*sum((y-mu[2])^2); df2<-n2
  }
  else{
    Sx2<-var(x); Sy2<-var(y); df1<-n1-1; df2<-n2-1
  }
  r<-Sx2/Sy2
  if (side<0) {
    a<-0
    b <- r/qf(alpha,df1,df2)
  }
  else if (side>0) {
    a <- r/qf(1-alpha,df1,df2)
    b <- Inf
  }
  else{
    a<-r/qf(1-alpha/2,df1,df2)
    b<-r/qf(alpha/2,df1,df2)
  }
  data.frame(rate=r, df1=df1, df2=df2, a=a, b=b)
}

A<-scan()
79.98 80.04 80.02 80.04 80.03 80.03 80.04 79.97
80.05 80.03 80.02 80.00 80.02

B<-scan()
80.02 79.94 79.98 79.97 79.97 80.03 79.95 79.97

interval_var2(A, B, mu=c(80,80))
interval_var2(A, B)
#方差测试
var.test(A,B)
interval_var4(A, B, side=-1)
interval_var4(A, B, side=1)
interval_var2(A, B, alpha=0.1)

#非正态总体的区间估计
#方差已知，用正态分布
#方差未知，样本方差替换总体方差，用正态分布
#和interval_estimate1的区别在于，总是采用正态分布。
interval_estimate3<-function(x,sigma=-1,alpha=0.05){
  n<-length(x); xb<-mean(x)
  if (sigma>=0)
    tmp<-sigma/sqrt(n)*qnorm(1-alpha/2)
  else
    tmp<-sd(x)/sqrt(n)*qnorm(1-alpha/2)
  data.frame(mean=xb, a=xb-tmp, b=xb+tmp)
}

x<-rexp(50, 1/2.266)
interval_estimate3(x)
interval_estimate1(x)

#R-Model page 252
#4.1 
fun <- function(alpha){  
  x <- c(0.1, 0.2, 0.9, 0.8, 0.7, 0.7)
  length(x)/(alpha+1)+sum(apply(t(x), 2, log))
}
x <- c(0.1, 0.2, 0.9, 0.8, 0.7, 0.7)
#矩估计法
alpha <- (2*mean(x)-1)/(1-mean(x));alpha
#最大似然估计
alpha1 <- -1/mean(apply(t(x), 2, log))-1;alpha1
uniroot(fun, c(0,1))

#4.2 
fun <- function(lamda){
  x<-c(rep(5,365), rep(15, 245), rep(25, 150),
       rep(35, 100), rep(45, 70), rep(55, 45),
       rep(65, 25))
  length(x)/lamda-sum(x)
}
uniroot(fun, c(0,1))
#指数分布lamda的最大似然估计等于1/平均数
x<-c(rep(5,365), rep(15, 245), rep(25, 150),
     rep(35, 100), rep(45, 70), rep(55, 45),
     rep(65, 25))
lamda <- 1/mean(x);lamda

#4.3
fun <- function(lamda){
  x<-c(rep(0, 17), rep(1, 20), rep(2, 10),
       rep(3, 2), rep(4, 1))
  sum(x)/lamda-length(x)
}
uniroot(fun, c(0,10))
#泊松分布lamda的最大似然估计等于平均数
x<-c(rep(0, 17), rep(1, 20), rep(2, 10),
     rep(3, 2), rep(4, 1))
lamda <- mean(x);lamda

#4.4
fun<-function(x){
  f1<--13+x[1]+((5-x[2])*x[2]-2)*x[2]
  f2<--29+x[1]+((x[2]+1)*x[2]-14)*x[2]
  f1^2+f2^2
}
x0<-c(0.5,-2); nlm(fun,x0)

#4.5
#对于正态分布的u和sigma的估计
#矩估计  ：u=mean(x) ; sigma=sum((x-mean(x))^2)/n
#最大似然: u=mean(x) ; sigma=sum((x-mean(x))^2)/n
#也就是说最大似然和矩估计对于simga的估计都不是无偏的
x <- c(54, 67, 68, 78, 70, 66, 67, 70, 65, 69)
mu <- mean(x); mu
#interval_estimate1(x, sigma=-1, alpha=0.1)
t.test(x, mu=72, conf.level=0.9)
t.test(x,alternative="less",mu=72)
t.test(x,alternative="less",mu=68)

#4.6
X<-c(140, 137, 136, 140, 145, 148, 140, 135, 144, 141)
Y<-c(135, 118, 115, 140, 128, 131, 130, 115, 131, 125)
t.test(X-Y)
interval_estimate4(X-Y)

#4.7
X<-c(0.143, 0.142, 0.143, 0.137)
Y<-c(0.140, 0.142, 0.136, 0.138, 0.140)
interval_estimate5(X, Y, var.equal=T)
t.test(X,Y,var.equal=TRUE)

#4.8
X<-c(140, 137, 136, 140, 145, 148, 140, 135, 144, 141)
Y<-c(135, 118, 115, 140, 128, 131, 130, 115, 131, 125)
interval_var2(X, Y)
var.test(X,Y)
#由于方差不相同
t.test(X, Y)

#4.9
x<-c(rep(0, 7), rep(1, 10), rep(2, 12),
     rep(3, 8), rep(4, 3), rep(5, 2))
interval_estimate3(x)

#4.10
x <- c(1067, 919, 1196, 785, 1126, 936, 918, 1156, 920, 948)
interval_estimate4(x, side=1)
t.test(x, alternative='greater')

###################################
#假设检验
###################################
#H0: null hypothesis
#H1: alternative hypothesis
#alpha : evidence level
#假设检验的基本思想
#1. 用了反证法的思想。先假设原假设成立，然后看由此产生的后果。
#如果导致一个非常不合理的现象产生（p(H0)<=aplha(显著性水平)），则拒绝原假设。
#反之，则表示我们不能拒绝原假设。
#2. 区别于纯数学中的反证法。这里不合理的现象其实就是小概率事件。
#常识：小概率事件一般不会发生。但也有例外，黑天鹅事件。

#假设检验的两类错误。
#         H0=True   H0=False
#接受H0             Error II(取伪)
#拒绝H0   Error I(弃真)
#接受H0更加精确的说法是不拒绝H0

#alpha ：P{拒绝H0|H0=True}
#beta ：P{接受H0|H1=True}
#一般在给定样本的情况，减少第一类错误，第二类错误的概率就会增加
#减少第二类错误，第一类错误的概率也会增加
#如果希望同时减少，则需要增加样本容量

P_value<-function(cdf, x, paramet=numeric(0), side=0){
  n<-length(paramet)
  P<-switch(n+1,
            cdf(x),
            cdf(x, paramet),
            cdf(x, paramet[1], paramet[2]),
            cdf(x, paramet[1], paramet[2], paramet[3])
  )
  if (side<0) P
  else if (side>0) 1-P
  else if (P<1/2) 2*P
  else 2*(1-P)
}        

#单个正态总体均值u的假设检验
#方差已知，用正态分布
#方差未知，用t分布
#side=-1 左侧p值， side=1 右侧p值，side=0 双侧p值
#side=0   原假设u=u0
#side=-1  原假设u>=u0
#side=1   原假设u<=u0
mean.test1<-function(x, mu=0, sigma=-1, side=0){
  n<-length(x); xb<-mean(x)
  if (sigma>0){
    z<-(xb-mu)/(sigma/sqrt(n))
    P<-P_value(pnorm, z, side=side)
    data.frame(mean=xb, df=n, Z=z, P_value=P)
  }
  else{
    t<-(xb-mu)/(sd(x)/sqrt(n))
    P<-P_value(pt, t, paramet=n-1, side=side)
    data.frame(mean=xb, df=n-1, T=t, P_value=P)
  }
}

#某器件的寿命，是否有理由认为平均寿命大于225小时
X<-c(159, 280, 101, 212, 224, 379, 179, 264,
     222, 362, 168, 250, 149, 260, 485, 170)
mean.test1(X, mu=225, side=1)
interval_estimate4(X, side=1)
mean.test1(X, mu=225, side=-1)
interval_estimate4(X, side=-1)
mean.test1(X, mu=225, side=0)
interval_estimate4(X, side=0)

t.test(X, alternative = "greater", mu = 225)

#两个总体的情况
#均值差的假设检验
#方差已知，正态分布
#方差未知，但相等，t分布
#方差未知，但不相同，用样本方差替换方差，t分布
#side=0   原假设u1=u2
#side=-1  原假设u1>=u2
#side=1   原假设u1<=u2
mean.test2<-function(x, y,
                     sigma=c(-1, -1), var.equal=FALSE, side=0){
  n1<-length(x); n2<-length(y)
  xb<-mean(x); yb<-mean(y)
  if (all(sigma>0)){
    z<-(xb-yb)/sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
    P<-P_value(pnorm, z, side=side)
    data.frame(mean=xb-yb, df=n1+n2, Z=z, P_value=P)
  }
  else{
    if (var.equal == TRUE){
      Sw<-sqrt(((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2))
      t<-(xb-yb)/(Sw*sqrt(1/n1+1/n2))
      nu<-n1+n2-2
    }
    else{
      S1<-var(x); S2<-var(y)
      nu<-(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
      t<-(xb-yb)/sqrt(S1/n1+S2/n2)
    }
    P<-P_value(pt, t, paramet=nu, side=side)
    data.frame(mean=xb-yb, df=nu, T=t, P_value=P)
  }
}

X<-c(78.1,72.4,76.2,74.3,77.4,78.4,76.0,75.5,76.7,77.3)
Y<-c(79.1,81.0,77.3,79.1,80.0,79.1,79.1,77.3,80.2,82.1)
mean(X);mean(Y)
mean.test2(X, Y, var.equal=T, side=-1)
mean.test2(X, Y, side=-1)
interval_estimate5(X, Y, var.equal=TRUE, side=-1)
interval_estimate5(X, Y, side=-1)
t<-t.test(X, Y, var.equal=TRUE, alternative = "less");t

#成对的数据t检验
X<-c(78.1,72.4,76.2,74.3,77.4,78.4,76.0,75.5,76.7,77.3)
Y<-c(79.1,81.0,77.3,79.1,80.0,79.1,79.1,77.3,80.2,82.1)
t.test(X-Y, alternative = "less")

#正态总体方差的假设检验
#u已知，用卡方分布, 自由度n
#u未知，用卡方分布, 自由度n-1
#side=0   原假设sigma^2=sigma0^2
#side=-1  原假设sigma^2=>sigma0^2
#side=1   原假设sigma^2=<sigma0^2
var.test1<-function(x, sigma2=1, mu=Inf, side=0){
  n<-length(x)
  if (mu<Inf){
    S2<-sum((x-mu)^2)/n; df=n
  }
  else{
    S2<-var(x); df=n-1
  }
  chi2<-df*S2/sigma2;
  P<-P_value(pchisq, chi2, paramet=df, side=side)
  data.frame(var=S2, df=df, chisq2=chi2, P_value=P)
}

X<-scan()
136 144 143 157 137 159 135 158 147 165
158 142 159 150 156 152 140 149 148 155

mean.test1(X, mu=149, sigma=sqrt(75))
mean.test1(X, mu=149)
var.test1(X, sigma2=75, mu=149)
var.test1(X, sigma2=75)

#两个总体方差
#u1,u2已知，用F(n1, n2)分布
#u1,u2未知，用F(n1-1, n2-1)分布
#side=0   原假设sigma1^2=sigma2^2
#side=-1  原假设sigma1^2=>sigma2^2
#side=1   原假设sigma1^2=<sigma2^2
var.test2<-function(x, y, mu=c(Inf, Inf), side=0){
  n1<-length(x); n2<-length(y)
  if (all(mu<Inf)){
    Sx2<-sum((x-mu[1])^2)/n1; Sy2<-sum((y-mu[2])^2)/n2
    df1=n1; df2=n2
  }
  else{
    Sx2<-var(x); Sy2<-var(y); df1=n1-1; df2=n2-1
  }
  r<-Sx2/Sy2
  P<-P_value(pf, r, paramet=c(df1, df2), side=side)
  data.frame(rate=r, df1=df1, df2=df2, F=r, P_value=P)
}

X<-c(78.1,72.4,76.2,74.3,77.4,78.4,76.0,75.5,76.7,77.3)
Y<-c(79.1,81.0,77.3,79.1,80.0,79.1,79.1,77.3,80.2,82.1)
var.test2(X,Y)
interval_var4(X, Y)
var.test(X,Y)

var.test2(X, Y, side=-1)
interval_var4(X, Y, side=-1)
var.test(X, Y, alternative='less')

#二项分布总体的假设检验
#蔬菜种子p0=0.85 随机抽取500粒，用种衣剂处理后，有445粒发芽。
#试检验种衣剂对种子发芽率有无效果
binom.test(445,500,p=0.85)
b<-t(apply(t(c(0:55,445:500)), 2, function(x, n, p, prec) c(x, round(choose(n,x)*p^x*(1-p)^(n-x),prec)), n=500, p=0.85, prec=8))
b;sum(b[,2])
qbinom(0.975, 500, 0.89)

binom.test(445,500,p=0.85, alternative='less')
b<-t(apply(t(c(0:445)), 2, function(x, n, p, prec) c(x, round(choose(n,x)*p^x*(1-p)^(n-x),prec)), n=500, p=0.85, prec=8))
b;sum(b[,2])
pbinom(445, 500, 0.85)

binom.test(445,500,p=0.85, alternative='greater')
b<-t(apply(t(445:500), 2, function(x, n, p, prec) c(x, round(choose(n,x)*p^x*(1-p)^(n-x),prec)), n=500, p=0.85, prec=8))
b;sum(b[,2])
1-pbinom(444, 500, 0.85)

#计算二项分布的概率
b<-t(apply(t(0:10), 2, function(x, n, p, prec) c(x, round(choose(n,x)*p^x*(1-p)^(n-x),prec)), n=10, p=0.7, prec=8))
b;b[2,2]/b[1,2]
b<-t(apply(t(0:10), 2, function(x, n, p, prec) c(x, round(choose(n,x)*p^x*(1-p)^(n-x),prec)), n=10, p=0.5, prec=8))
b;b[2,2]/b[1,2]
b<-t(apply(t(0:10), 2, function(x, n, p, prec) c(x, round(choose(n,x)*p^x*(1-p)^(n-x),prec)), n=10, p=0.2, prec=8))
b;b[2,2]/b[1,2]


#以往经验，新生儿染色体异常率一般为1%，观察了400名新生儿，只有一例异常
#问该地区异常率是否低于一般水平
binom.test(1, 400, p = 0.01, alternative = "less")
binom.test(c(1, 399), p = 0.01, alternative = "less")
b<-t(apply(t(0:1), 2, function(x, n, p, prec) c(x, round(choose(n,x)*p^x*(1-p)^(n-x),prec)), n=400, p=0.01, prec=8))
b

#若干非常重要的非参数检验
#贝叶斯方法 待定
pbinom(0, 10, 0.2)
pbinom(1, 10, 0.2)

#Pearson拟合优度卡方检验
#理论分布完全已知的情况
#1000个消费者品尝5种啤酒（A, B, C, D, E)
#根据这些数据判断消费者对这5种啤酒爱好有无偏差
X<-c(210, 312, 170, 85, 223)
names(X) <- c('A', 'B', 'C', 'D', 'E')
n<-sum(X); m<-length(X)
p<-rep(1/m, m)
y <-cbind(x=X, p)
K <- sum(apply(y , 1, function(x, n) x[1]^2/n/x[2], n)) - n;K
K<-sum((X-n*p)^2/(n*p));K
Pr<-1-pchisq(K, m-1);Pr
#只用一行可以完成上述工作
chisq.test(X)

#用Pearson拟合优度卡方检验学生成绩是否服从正态分布
X<-scan()
25 45 50 54 55 61 64 68 72 75 75
78 79 81 83 84 84 84 85 86 86 86
87 89 89 89 90 91 91 92 100

#cut函数Convert Numeric to Factor 把numeric分成多个区域
A<-table(cut(X, br=c(0,69,79,89,100)))
p<-pnorm(c(70,80,90), mean(X), sd(X))
p1<-p[1]
for(i in 2:length(p)){
  p1[i]<-p[i]-p[i-1]
}
p1[length(p)+1]<-1-p[length(p)]
chisq.test(A,p=p1)

range<-c(0,69,79,89,100)
A<-table(cut(X, br=c(0,69,79,89,100)))
range <- c(0,69,79,89,100)
A<-table(cut(X, br=))
p<-pnorm(range[-c(1, length(range))], mean(X), sd(X)); p[length(p)+1]<-1
p1<-p[1]
for(i in 2:length(p)){
  p1[i]<-p[i]-p[i-1]
} 
chisq.test(A,p=p1)

#大麦的杂交后代关于芒性比例无芒：长芒：短芒=9：3：4
#实际观测值为335:125:160
#尝试检测是否符合理论假设
chisq.test(c(335, 125, 160), p=c(9/16, 3/16, 4/16))

#验证电话总机某段时间接到的呼叫次数是否服从Poisson分布
#能否确认在某个时段内接到的呼叫次数服从Possion分布(alpha=0.1)
#X代表第几分钟，Y表示对应接到电话的次数
X<-0:6; Y<-c(7, 10, 12, 8, 3, 2, 0)
q<-ppois(X, mean(rep(X,Y)))
p[1]<-q[1]; p[n]<-1-q[n-1]
for (i in 2:(n-1))
  p[i]<-q[i]-q[i-1]
Z<-c(Y[1:4], Y[5]+Y[6])
n<-length(Z); p<-p[1:n-1]; p[n]<-1-q[n-1]
chisq.test(Z, p=p)



#理论分布依赖于若干个参数
#如果r个未知参数，需要用极大似然估计
#和前面相比，自由度改变了，自由度变成了m-1-r

#Kolmogorov-Smirnov检验
#单样本检验
#对某设备进行寿命检验，记录10次无故障工作时间
#检验时间是否服从lamda=1/1500的指数分布
X<-c(420, 500, 920, 1380, 1510, 1650, 1760, 2100, 2300, 2350)
ks.test(X, "pexp", 1/1500)

#双样本检验。检查两个样本是否相同分布
X<-scan()
0.61 0.29 0.06 0.59 -1.73 -0.74 0.51 -0.56 0.39
1.64 0.05 -0.06 0.64 -0.82 0.37 1.77 1.09 -1.28
2.36 1.31 1.05 -0.32 -0.40 1.06 -2.47

Y<-scan()
2.20 1.66 1.38 0.20 0.36 0.00 0.96 1.56 0.44
1.50 -0.30 0.66 2.31 3.29 -0.27 -0.37 0.38 0.70
0.52 -0.71

ks.test(X, Y)

#列联表数据的独立性检验
#Pearson卡方检验
#研究吸烟是否和肺癌有关
x<-c(60, 7, 5, 11)
dim(x)<-c(2,2)
colnames(x)=c('smoke', 'no smoke')
rownames(x)=c('lung cancer', 'no lung cancer')
chisq.test(x,correct = FALSE)
chisq.test(x)
fisher.test(x)

#调查了年收入和工作满意程度
#研究收入是否和工作满意程度有关
x<-scan()
20 24 80 82 22 38 104 125
13 28 81 113 7 18 54 92

dim(x)<-c(4,4)
dimnames(x)<-list(c('<6000', '6000~15000', '15000-25000', '>25000'),
                  c('Very Disappointed','Disappointed', 'Satisfied','Very Satisfied'))
x
chisq.test(x)
chisq.test(x,correct = FALSE)

#卡方独立性检验的另外一个例子
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)

mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)

#Fisher精确的独立检验
#http://www.cnblogs.com/Dzhouqi/p/3440575.html
#研究乙肝免疫球蛋白预防胎儿感染HBC的效果。
#问是否两组新生儿的HBV感染率是否有差别
x<-c(4,5,18,6); dim(x)<-c(2,2)
dimnames(x)<-list(c('Prevention', 'No Prevention'),
                  c('Positive','Negative'))
x
fisher.test(x)
#手工计算
#组合函数combn, choose
#               Positive Negative
#Prevention           4       18
#No Prevention        5        6
p <- t(apply(t(0:9), 2, function(x) c(x, choose(22, x)*choose(11, 9-x)/choose(33, 9))))
p
fisher.test(x, alternative='greater');sum(p[5:10,2])
fisher.test(x, alternative='less');sum(p[1:5,2])
fisher.test(x);sum(p[1:5,2])+p[10,2]

#免疫球蛋白预防胎儿感染HBC的效果。
#问是否两组新生儿的HBV感染率是否有差别
x<-c(3,1,1,3); dim(x)<-c(2,2)
dimnames(x)<-list(c('Milk First','Tea First'),
                  c('Guess Milk First', 'Guess Tea First')
)
x
fisher.test(x)
#             Guess Milk First Guess Tea First
# Milk First                3               1
# Tea First                 1               3
x<-c(3,1,1,3); dim(x)<-c(2,2)
dimnames(x)<-list(c('Milk First','Tea First'),
                  c('Guess Milk First', 'Guess Tea First')
)
fisher.test(x)
p <- t(apply(t(0:4), 2, function(x) c(x, choose(4, x)*choose(4, 4-x)/choose(8, 4))))
p
sum(p[1:2,2])+sum(p[4:5,2]);fisher.test(x)
sum(p[4:5,2]);fisher.test(x, alternative='greater')
sum(p[1:4,2]);fisher.test(x, alternative='less')

#Fisher精确检验
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)

#Cochran-Mantel-Haenszel检验
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
chisq.test(mytable)

#McNemar检验
#用A, B两种方法鉴定202分痰标本中的抗酸杆菌，请问检出率有无差别？
X<-c(49,21,25,107); dim(X)<-c(2,2)
dimnames(X)<-list(c('A+','A-'),
                  c('B+', 'B-')
)
X
mcnemar.test(X,correct=FALSE)

#符号检验
#检验一个样本是否来自某个总体
#联合国在世界上66个大城市的生活花费指数，其中北京得指数为99
#检验北京是在中位数之上还是之下
X<-scan()
66 75 78 80 81 81 82 83 83 83 83
84 85 85 86 86 86 86 87 87 88 88
88 88 88 89 89 89 89 90 90 91 91
91 91 92 93 93 96 96 96 97 99 100
101 102 103 103 104 104 104 105 106 109 109
110 110 110 111 113 115 116 117 118 155 192

n <- length(X); k<-sum(X>99)
p <- t(apply(t(0:n), 2, function(x) c(x, choose(n, x)*(1/2)^n)))
p
sum(p[1:(k+1),2]);binom.test(sum(X>99), length(X), al="less")
sum(p[(k+1):n,2]);binom.test(sum(X>99), length(X), al="greater")
sum(p[1:(k+1),2])*2;binom.test(sum(X>99), length(X))

#符号检验
#成对样本来检验两个总体是否存在
#两种饲料养猪增重效果是否有差异
x<-scan()
25 30 28 23 27 35 30 28 32 29 30 30 31 16

y<-scan()
19 32 21 19 25 31 31 26 30 25 28 31 25 25

binom.test(sum(x<y), length(x))

#Spearman 秩相关检验（秩相关系数）
#六人参加表演竞赛，两人打分评定
#验证两人的打分有无相关性
x<-c(1,2,3,4,5,6); y<-c(6,5,4,3,2,1)
cor.test(x, y, method = "spearman")
cor.test(x, y, method = "pearson")
cor.test(x, y, method = "kendall")

#Kendall相关检验（相关系数）
#幼儿园对9对双胎胞之上检查
#判断双胞胎先后出生智力是否相关
x<-c(86, 77, 68, 91, 70, 71, 85, 87, 63)
y<-c(88, 76, 64, 96, 65, 80, 81, 72, 60)
cor.test(x, y, method = "spearman")
cor.test(x, y, method = "pearson")
cor.test(x, y, method = "kendall")

#Wilcoxon秩检验
#检验电池寿命的中位数是140安培
X<-scan()
137.0 140.0 138.3 139.0 144.3 139.1 141.7 137.3 133.5 138.2
141.1 139.2 136.5 136.5 135.6 138.0 140.9 140.6 136.3 134.1

wilcox.test(X, mu=140, alternative="less",
            exact=FALSE, correct=FALSE, conf.int=TRUE)

#成对数据
#比起符号检验，在探测差异性方面更有效
x<-c(459, 367, 303, 392, 310, 342, 421, 446, 430, 412)
y<-c(414, 306, 321, 443, 281, 301, 353, 391, 405, 390)
wilcox.test(x, y, alternative = "greater", paired = TRUE)
wilcox.test(x-y, alternative = "greater")
wilcox.test(x, y, alternative = "greater")
binom.test(sum(x>y), length(x), alternative = "greater")

#非成对数据
#检验血铅值有无差异
x<-c(24, 26, 29, 34, 43, 58, 63, 72, 87, 101) #非血铅组
y<-c(82, 87, 97, 121, 164, 208, 213)  #血铅组
wilcox.test(x,y,alternative="less",exact=FALSE,correct=FALSE)
wilcox.test(x, y, alternative="less", exact=FALSE)

#R-Modeling
#5.1
x <- scan()
220 188 162 230 145 160 238 188 247 113
126 245 164 231 256 183 190 158 224 17

mean.test1(x, 225)
t.test(x, mu=225)

#5.2
x <- scan()
1067 919 1196 785 1126 936 918 1156 920 948

mean.test1(x, 1000, side=1)
t.test(x, mu=1000, alternative='greater')
1-pnorm(1000,mean(x),sd(x))

#5.3
x <- scan()
113 120 138 120 100 118 138 123

y <- scan()
138 116 125 136 110 132 130 110

mean.test1(x-y, 0, side=0)
t.test(x-y, mu=0, alternative='two.sided')

mean.test2(x, y, var.equal=F)
t.test(x, y, var.equal=F)
mean.test2(x, y, var.equal=T)
t.test(x, y, var.equal=T)

#5.4 检查新药效果，40名二型糖尿病随机分到两组
x <- scan() #试验组
-0.70 -5.60 2.00 2.80 0.70 3.50 4.00 5.80 7.10 -0.50
2.50 -1.60 1.70 3.00 0.40 4.50 4.60 2.50 6.00 -1.40

y <- scan() #对照组
3.70 6.50 5.00 5.20 0.80 0.20 0.60 3.40 6.60 -1.10
6.00 3.80 2.00 1.60 2.00 2.20 1.20 3.10 1.70 -2.00

#1）检查是否符合正态分布
#图示法: QQ图，PP图
#从例子来看，qq图和pp图中，
#感觉只要数据打大概趋势是直线，散点聚集在固定直线的周围
#就可以认为其符合正态分布
qqnorm(x); qqline(x)
qqnorm(y); qqline(y)

#计算综合统计量：Shapiro-Wilk法(W检验)
shapiro.test(x) #会有警告信息，因为有重复值
x[which(x==2.5)][1]=2.5+1E-6
shapiro.test(x)
shapiro.test(y) #会有警告信息，因为有重复值
y[which(y==2.0)][1]=2+1E-6
shapiro.test(y)

#正态分布拟合优度检验（卡方，Kolmogorov-Smirov法检验）
ks.test(x, 'pnorm', mean(x), sd(x))
ks.test(y, 'pnorm', mean(y), sd(y))

chisq_p <- function(X, ranges){
  A<-table(cut(X, br=ranges))
  p<-pnorm(ranges[-c(1, length(range))], mean(X), sd(X)); p[length(p)+1]<-1
  p1<-numeric(0);p1[1]<-p[1]
  for(i in 2:length(p)){
    p1[i]<-p[i]-p[i-1]
  } 
  return(list(x=A, p=p1))
}
range <- quantile(x); range[1]<--Inf
d <- chisq_p(x, range);d
chisq.test(d$x,p=d$p)

range <- quantile(y); range[1]<--Inf
d <- chisq_p(y, range);d
chisq.test(d$x,p=d$p)

#2)
t.test(x, y, var.equal=T)
mean.test2(x, y, var.equal=T)
t.test(x, y, var.equal=F)
mean.test2(x, y, var.equal=F)
t.test(x-y)
mean.test1(x-y)

#3)
var.test(x, y)
var.test2(x, y)

#5.5 检验新药对抗凝血酶活力的影响
x <- scan() #试验组
126 125 136 128 123 138 142 116 110 108 115 140

y <- scan() #对照组
162 172 177 170 175 152 157 159 160 162

#1
shapiro.test(x)
shapiro.test(y)
ks.test(x, 'pnorm', mean(x), sd(x))
ks.test(y, 'pnorm', mean(y), sd(y))

#2
var.test(x, y)
var.test2(x, y)

#3
t.test(x, y, var.equal=T, alternative='less')
t.test(x, y, var.equal=F, alternative='less')
wilcox.test(x,y,alternative="less",exact=FALSE,correct=FALSE)
wilcox.test(x, y, alternative="less", exact=FALSE)

#5.6 二项分布总体的假设检验：
binom.test(57, 400, p=0.147, alternative = "two.sided")
binom.test(57, 400, p=0.147, alternative = "greater")
binom.test(57, 400, p=0.147, alternative = "less")

#5.7 二项分布总体的假设检验：
binom.test(178, 328, p=0.5, alternative = "greater")

#5.8 利用pearson卡方检验是否符合特定分布：
chisq.test(c(315, 101, 108, 32), p=c(9/16, 3/16, 3/16, 1/16))

#5.9 利用pearson卡方检验是否符合泊松分布：
x <- c(92, 68, 28, 11, 1)
p<-ppois(0:2, mean(rep(0:4, x)));p[length(p)+1]<-1
p1<-p[1]
for(i in 2:length(p)){
  p1[i]<-p[i]-p[i-1]
} 
chisq.test(c(92, 68, 28, 12), p=p1)

#5.10 ks检验 两个分布是否相同：
x <- scan()
2.36 3.14 7.52 3.48 2.76 5.43 6.54 7.41

y <- scan()
4.38 4.25 6.53 3.28 7.21 6.55

ks.test(x, y)

#5.11 列联数据的独立性检验：
x <- c(358, 2492, 229, 2745)
dim(x) <- c(2, 2)
dimnames(x)<-list(c('剖腹产', '顺产'),
                  c('使用监测仪','未使用监测仪'))
x
chisq.test(x,correct = F)
chisq.test(x,correct = T)
fisher.test(x)
mcnemar.test(x,correct=F)
mcnemar.test(x,correct=T)

#5.12 列联数据的独立性检验：
x <- c(45, 46, 28, 11, 12, 20, 23, 12, 10, 28, 30, 35)
dim(x) <- c(4, 3)
chisq.test(x,correct = F)
chisq.test(x,correct = T)

#5.13 因有的格子的频数小于5，故采用fiser确切概率法检验独立性。
x <- c(3, 6, 4, 4)
dim(x) <- c(2, 2)
fisher.test(x)

#5.14 由于是在相同个体上的两次试验，故采用McNemar检验。
x <- c(3, 6, 4, 4)
dim(x) <- c(2, 2)
mcnemar.test(x,correct=F)
mcnemar.test(x,correct=T)

#5.15 
x <- scan()
13.32 13.06 14.02 11.86 13.58 13.77 13.51 14.42 14.44 15.43

#1)符号检验
binom.test(sum(x>14.6), length(x), p=0.5, alternative='less')
#2)
wilcox.test(x, mu=14.6, alternative="less", exact=F, correct=F, conf.int=T)
wilcox.test(x, mu=14.6, alternative="less", exact=F, correct=T, conf.int=T)

#5.16
x <- scan()
48.0 33.0 37.5 48.0 42.5 40.0 42.0 36.0 11.3 22.0
36.0 27.3 14.2 32.1 52.0 38.0 17.3 20.0 21.0 46.1

y <- scan()
37.0 41.0 23.4 17.0 31.5 40.0 31.0 36.0 5.7 11.5
21.0 6.1 26.5 21.3 44.5 28.0 22.6 20.0 11.0 22.3

#1 符号法检查是否有显著差异
binom.test(sum(x<y), length(x))
binom.test(sum(x>=y), length(x))

#2 Wilcoxon符号检验法
wilcox.test(x,y,paired=TRUE,exact=FALSE,correct=F)
wilcox.test(x,y,paired=TRUE,exact=FALSE,correct=T)

#3 Wilcoxon秩和检验法
wilcox.test(x,y,alternative="two.sided",exact=F,correct=F)
wilcox.test(x,y,alternative="two.sided",exact=F,correct=T)

#4 
#正态检验
qqnorm(x); qqline(x)
qqnorm(y); qqline(y)
#计算综合统计量：Shapiro-Wilk法(W检验)
shapiro.test(x) 
shapiro.test(y) 
#正态分布拟合优度检验（卡方，Kolmogorov-Smirov法检验）
ks.test(x, 'pnorm', mean(x), sd(x))
ks.test(y, 'pnorm', mean(y), sd(y))
#方差齐次检验
var.test(x, y)
#p值很大，所以认为x,y方差相同
#既然符合正态的假设，且方差相同，可以调用一下t检验
t.test(x, y, var.equal=T)
t.test(x, y, var.equal=F)
t.test(x, y, var.equal=T, paired=T)
t.test(x-y, var.equal=T)
#各项p值都显示，x,y的均值并不相同
#综上所述，Wilcoxon符号秩检验的差异检出能力最强，符号检验的差异检出最弱。

#5.17
x <- scan()
24 17 20 41 52 23 46 18 15 29

y <- scan()
8 1 4 7 9 5 10 3 2 6

cor.test(x, y, method='spearman')
cor.test(x, y, method='kendall')
cor.test(x, y, method='pearson')

#5.18
x <- scan()
0 1 9 7 3
2 2 11 4 1

dim(x) <- c(5 ,2)
x <- t(x)
chisq.test(x,correct = FALSE) #但因为有很多频数小于5，所以结果不可靠

x<-rep(1:5,c(0,1,9,7,3));y<-rep(1:5,c(2,2,11,4,1))
wilcox.test(x,y,exact=F)

