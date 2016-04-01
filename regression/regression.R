
#--------------------------------
#尝试各种函数的线性回归
#  主要获得线性回归各指标和图形的关系，获得对数据的一种直觉
#--------------------------------

#正态分布
x <- 1：50
theta <- c(1, 2)
y <- cbind(rep(1, length(x)), x) %*% theta
d <- data.frame(x, y) + rnorm(length(x), 0 ,1)
p <- ggplot(d, aes(x=x, y=y))
p + geom_point()
fit <- lm(y~x)
summary(fit)
