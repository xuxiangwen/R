toothpaste<-data.frame(
  X1=c(-0.05, 0.25,0.60,0, 0.25,0.20, 0.15,0.05,-0.15, 0.15,
       0.20, 0.10,0.40,0.45,0.35,0.30, 0.50,0.50, 0.40,-0.05,
       -0.05,-0.10,0.20,0.10,0.50,0.60,-0.05,0, 0.05, 0.55),
  X2=c( 5.50,6.75,7.25,5.50,7.00,6.50,6.75,5.25,5.25,6.00,
        6.50,6.25,7.00,6.90,6.80,6.80,7.10,7.00,6.80,6.50,
        6.25,6.00,6.50,7.00,6.80,6.80,6.50,5.75,5.80,6.80),
  Y =c( 7.38,8.51,9.52,7.50,9.33,8.28,8.75,7.87,7.10,8.00,
        7.89,8.15,9.10,8.86,8.90,8.87,9.26,9.00,8.75,7.95,
        7.65,7.27,8.00,8.50,8.75,9.21,8.27,7.67,7.93,9.26)
)

attach(toothpaste)

lm.sol<-lm(Y ~ X1 + X2 + I(X2^2) + X1:X2, data=toothpaste)
summary(lm.sol)

lm.sol1 <- lm(Y~X1, data=toothpaste)
lm.sol2 <- lm(Y~X2, data=toothpaste)

source("Reg_Diag.R")
Reg_Diag(lm.sol)

#从结果来看：
#第11号向本residual最大，且standard和student残差绝对值大于2
#第5，24号的standard, student和DFFITS统计量超过规定指标
#经过分析，第5，11和24号样本点应该在回归计算中删除

Y1 <- Y[-c(5, 11, 24)]
X21 <- X2[-c(5, 11, 24)]
X11 <- X1[-c(5, 11, 24)]

lm.sol1 <- lm(Y1~X11, data=toothpaste)
lm.sol2 <- lm(Y1~X21, data=toothpaste)

plot(Y1~X11)
abline(lm.sol1)
plot(Y1~X21)
abline(lm.sol2)

lm.sol<-lm(Y1 ~ X11 + X21 + I(X21^2) + X11:X21, data=toothpaste)
summary(lm.sol)
#可以看出，残差标准差从0.2063降低到0.164，相关系数的平方从0.9209提高到0.9518

detach(toothpaste)
