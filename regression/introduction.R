library(UsingR); 
library(reshape); 

#===================================================
# Introduction
#===================================================

#---------------------------------------------------
# Francis Galton’s height data
# Francis Galton, the 19th century polymath, can be credited with discovering regression. 
# In his landmark paper Regression Toward Mediocrity in Hereditary Stature he compared 
# the heights of parents and their children. He was particularly interested in the idea 
# that the children of tall parents tended to be tall also, but a little shorter than 
# their parents. Children of short parents tended to be short, but not quite as short 
# as their parents. He referred to this as “regression to mediocrity” (or regression to
# the mean).
# 我的理解，使因为两者之间的回归系数0.6463<1, 这样Intercept的影响就体现出来了。为何身高的
# 关系这样，可能在于生物学上，每个物种都有一个自身理想的身高，如果太高，太矮，都不容易生存。所以
# 就# 必须回归平均，但这样或许有些牵强。
#---------------------------------------------------

data(galton)
long <- melt(galton)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(. ~ variable)
g

#通过动态的操作，可以显示mse的变化
library(manipulate)
library(dplyr)
myHist <- function(mu){
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour= "black", binwidth=1)
  g <- g + geom_vline(xintercept = mu, size = 3)
  g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
  g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

ggplot(galton, aes(x = parent, y = child)) + geom_point()

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
  #g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
  g <- ggplot(freqData %>% filter(freq>0), aes(x = parent, y = child))
  g <- g  + scale_size(range = c(2, 20), guide = "none" )
  g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
  g <- g + geom_point(aes(colour=freq, size = freq))
  g <- g + scale_colour_gradient(low = "lightblue", high="white")
  g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
  mse <- mean( (y - beta * x) ^2 )
  g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
  g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

fit <- lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)
summary(fit)


#回归可以从好几个角度理解
#1）机器学习，误差平方和最小
#2）统计学，误差的方差最小
#3）统计学，最大似然方法。因为对误差正态分布的假设，所以采用一系列假设检验对模型进行评估
#计算回归系数和间距有两种方法
#1）梯度下降
#2）线性代数的公式

#相关系数就是两个向量的余弦
#回归系数就的关于相关系数 X 标准方差(Y) / 标准方差(X)
#回归系数 = cor(y, x) *  sd(y) / sd(x)
#截距 intersect = mean(x) - beta1 * mean(y)
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

beta1 <- cor(y, x) *  sd(x) / sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))

yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])

yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

#Regression to the mean
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g

#Using regression for prediction
library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

fit <- lm(price ~ carat, data = diamond)
coef(fit)

fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)

fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)

newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx

predict(fit, newdata = data.frame(carat = newx))


#plot the original Galton data points with larger dots for more freq pts
y <- galton$child
x <- galton$parent
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)), 
     pch = 21, col = "black", bg = "lightblue",
     cex = .07 * freqData$freq, xlab = "parent", ylab = "child")

#original regression line, children as outcome, parents as predictor
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x) * cor(y, x),  #slope
       lwd = 3, col = "red")

#new regression line, parents as outcome, children as predictor
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercept
       sd(y) / cor(y, x) / sd(x), #slope
       lwd = 3, col = "blue")

#assume correlation is 1 so slope is ratio of std deviations
abline(mean(y) - mean(x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x),  #slope
       lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19) #big point of intersection

#Quiz 1
#1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

sum(x * w)/sum(w)

#2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
mean(x)
lm(y~x)
lm(x~y)
x_norm <- (x - mean(x))
y_norm <- (y - mean(y))
lm(y_norm~x_norm)

#3
data(mtcars)
lm(mpg ~wt, data=mtcars )
lm(wt ~mpg, data=mtcars )

#6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
scale(x, TRUE, TRUE)


#---------------------------------------------------
# Estimating residual variation
#---------------------------------------------------
#Residual standard error
sqrt(sum(fit$residuals^2)/(n - 2))
summary(fit)$sigma
sqrt(deviance(fit)/(n-2))

#---------------------------------------------------
# Multiple R-squared
#---------------------------------------------------
sTot <- sum((galton$child - mu)^2)
sRes <- sum(fit$residuals^2)
1-sRes/sTot
summary(fit)
summary(fit)$r.squared
cor(galton$child, galton$parent)^2

# R squared 单纯看R Squared会有很多误判
data(anscombe)
example(anscombe)

#---------------------------------------------------
# Regression inference
#---------------------------------------------------
library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2))
ssx <- sum((x - mean(x))^2)

seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1

fit <- lm(y~x)
summary(fit)

pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable

sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10

library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"
g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g

#Quiz 2
#Consider the following data with x as the predictor and y as as the outcome.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
#1. Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.
summary(lm(y ~ x))$coef

#2. Consider the previous problem, give the estimate of the residual standard deviation.
fit <- lm(y ~ x)
a <- summary(fit)
a$sigma

#3. In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome). Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint?
data(mtcars)
fit <- lm(mpg ~ I(wt - mean(wt)), data = mtcars)
confint(fit)

#5. Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (1,000 lbs). A new car is coming weighing 3000 pounds. Construct a 95% prediction interval for its mpg. What is the upper endpoint?
fit <- lm(mpg ~ wt, data = mtcars)
predict(fit, newdata = data.frame(wt = 3), interval = "prediction")

#6. Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. Give the lower endpoint.
fit <- lm(mpg ~ wt, data = mtcars)
confint(fit)[2, ] * 2

fit <- lm(mpg ~ I(wt * 0.5), data = mtcars)
confint(fit)[2, ]

#9. Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor. About what is the ratio of the the sum of the squared errors, ∑ni=1(Yi−Y^i)2 when comparing a model with just an intercept (denominator) to the model with the intercept and slope (numerator)?
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit1)$r.squared

sse1 <- sum((predict(fit1) - mtcars$mpg)^2)
sse2 <- sum((predict(fit2) - mtcars$mpg)^2)
sse1/sse2


#--------------------------------------
# Multivariable examples and tricks
#--------------------------------------

require(datasets); data(swiss);require(dplyr)
require(GGally); require(ggplot2)
g <- ggpairs(swiss, lower = list(continuous = "smooth")) #, params = c(method="loess"))
g

summary(lm(Fertility ~ . , data = swiss))
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

#Simulation study
n = 100; x2 <- 1 : n; x1 = .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

#What if we include a completely unnecessary variable
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)

#Insect Sprays
require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

head(InsectSprays,20)
sprays <- group_by(InsectSprays, spray)
sprays.mean <- summarise(sprays, dist = mean(count, na.rm = TRUE)) 
sprays.mean 
c(
  A=sprays.mean[1,]$dist,
  B=sprays.mean[2,]$dist-sprays.mean[1,]$dist,
  C=sprays.mean[3,]$dist-sprays.mean[1,]$dist,
  D=sprays.mean[4,]$dist-sprays.mean[1,]$dist,
  E=sprays.mean[5,]$dist-sprays.mean[1,]$dist,
  F=sprays.mean[6,]$dist-sprays.mean[1,]$dist
)

summary(lm(count ~ spray, data = InsectSprays))$coef
summary(lm(count ~
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F'))
           , data = InsectSprays))$coef
summary(lm(count ~
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays))$coef

summary(lm(count ~ spray-1 , data = InsectSprays))$coef

spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

#Adjustment
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

fit <- lm(mpg~  wt + factor(cyl),mtcars );summary(fit)
fit1 <- lm(mpg~  factor(cyl),mtcars );summary(fit1)
fit2 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
fit3 <- lm(mpg~  wt * factor(cyl),mtcars );summary(fit3)

#Simulation demonstrating variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n);
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2);
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)


#head(dfbeta(fit))
#hatvalues
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
plot(x, y)
fit <- lm(y~x)
hatvalues(fit)
dfbeta(fit)
fit1 <- lm(y[-5]~x[-5])
dfbeta(fit1)
influence(lm(y ~ x))$hat
influence(lm(y ~ x))
influence.measures(lm(y ~ x))

fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
fit2 <- lm(mpg ~ factor(cyl) * wt, data = mtcars)
summary(fit1)$coef
##              Estimate Std. Error t value  Pr(>|t|)
## (Intercept)    33.991     1.8878  18.006 6.257e-17
## factor(cyl)6   -4.256     1.3861  -3.070 4.718e-03
## factor(cyl)8   -6.071     1.6523  -3.674 9.992e-04
## wt             -3.206     0.7539  -4.252 2.130e-04
summary(fit2)$coef
##                 Estimate Std. Error t value  Pr(>|t|)
## (Intercept)       39.571      3.194 12.3895 2.058e-12
## factor(cyl)6     -11.162      9.355 -1.1932 2.436e-01
## factor(cyl)8     -15.703      4.839 -3.2448 3.223e-03
## wt                -5.647      1.359 -4.1538 3.128e-04
## factor(cyl)6:wt    2.867      3.117  0.9197 3.662e-01
## factor(cyl)8:wt    3.455      1.627  2.1229 4.344e-02
anova(fit1, fit2)
## Analysis of Variance Table
## 
## Model 1: mpg ~ factor(cyl) + wt
## Model 2: mpg ~ factor(cyl) * wt
##   Res.Df RSS Df Sum of Sq    F Pr(>F)
## 1     28 183                         
## 2     26 156  2      27.2 2.27   0.12

#--------------------------------------------------
# Generalized Linear Models
#--------------------------------------------------

#Visualizing fitting logistic regression curves
#检查不同的参数对曲线的影响
x = seq(-10, 10, length = 1000)
beta0 = 0; beta1s = seq(.25, 1.5, by = .1)
plot(c(-10, 10), c(0, 1), type = "n", xlab = "X", ylab = "Probability", frame = FALSE)
sapply(beta1s, function(beta1) {
  y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
  lines(x, y, type = "l", lwd = 3)
})

x = seq(-10, 10, length = 1000)
beta0s = seq(-2, 2, by = .5); beta1 = 1
plot(c(-10, 10), c(0, 1), type = "n", xlab = "X", ylab = "Probability", frame = FALSE)
sapply(beta0s, function(beta0) {
  y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
  lines(x, y, type = "l", lwd = 3)
})

x = seq(-10, 10, length = 1000)
beta0 = 0; beta1 = 1
p = 1 / (1 + exp(-1 * (beta0 + beta1 * x)))
y = rbinom(prob = p, size = 1, n = length(p))
plot(x, y, frame = FALSE, xlab = "x", ylab = "y")
lines(lowess(x, y), type = "l", col = "blue", lwd = 3)
fit = glm(y ~ x, family = binomial)
lines(x, predict(fit, type = "response"), lwd = 3, col = "red")

#Poisson distribution
par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE)

#Bonus material
## simulate the data 画一个曲线，模拟线段走势，平滑曲线
n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
## the break points of the spline fit
knots <- seq(0, 8 * pi, length = 20);
## building the regression spline terms
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
## adding an intercept and the linear term
xMat <- cbind(1, x, splineTerms)
## fit the model, notice the intercept is in xMat so we have -1

yhat <- predict(lm(y ~ xMat - 1))
## perform the plot
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

#adding squared terms 曲线更加的平滑
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
xMat <- cbind(1, x, x^2, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

