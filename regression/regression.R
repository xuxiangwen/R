library(dplyr)
library(ggplot2)

#--------------------------------
#尝试各种函数的线性回归
#  主要获得线性回归各指标和图形的关系，获得对数据的一种直觉
#--------------------------------

show_lm <- function(x, y) {
  fit <- lm(y~x)
  fit.coef <- coef(fit)
  print(summary(fit))
  d <- data.frame(x=x, y=y)
  p <- ggplot(d, aes(x=x, y=y))
  p + geom_point() + geom_abline(intercept=fit.coef[1], slope=fit.coef[2] )
} 

x <- 1:50
theta <- c(5, 2)
y.f <- as.vector(cbind(rep(1, length(x)), x) %*% theta)

#无任何噪音
y <- y.f 
show_lm(x, y)


#正态分布（同分布）
y <- y.f + rnorm(length(x), 0 ,5)
show_lm(x, y)

#正态分布（随机均值,方差）
y <- y.f +  sapply(1:length(x),  function(i) rnorm(1,  runif(1, 0, 1), 5*runif(1, 1, 2)))
show_lm(x, y)

#方差逐渐变大的正态分布
y <- y.f +  sapply(1:length(x),  function(i) rnorm(1,  runif(1, 0, 1), 5+i*0.3))
show_lm(x, y)

library(MASS)
mdl <- glm(use ~wind,family=binomial, shuttle)
summary(mdl)
shuttle1 <- shuttle 
shuttle1$use1 <- 2- as.numeric(shuttle$use)
mdl1 <- glm(use1 ~wind,family=binomial, shuttle1)
summary(mdl1)

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

summary(lm(y~x))
z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit)[2:3])

mdl <- glm(count ~ spray, family = poisson, InsectSprays )

library(MASS)
data(shuttle)
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))

## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
exp(coef(fit))

shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))


