library(dplyr)
library(ggplot2)
library(grid)

#-----------------------------------------------------
#画出各个分布的图，体会各个参数对图形的影响
#-----------------------------------------------------

library(reshape)
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

showPlot <- function(data) {
  p <- ggplot(data, aes(x, y, group = group, colour=group))
  p + geom_line()
}

showPlots<- function(m, n, params, fun) {
  grid.newpage()  ##新建页面
  pushViewport(viewport(layout = grid.layout(m, n)))  
  len <- nrow(params)/m/n
  g <- lapply(1:m, function(i) {
    lapply(1:n, function(j,i) {
      oneParms <- params[((i-1)*n*len+(j-1)*len+1):((i-1)*n*len+j*len),]
      oneData <- fun(oneParms)
      onePlot <- showPlot(oneData)
      print(onePlot, vp = vplayout(i, j))
    },i=i)
  }) 
}

#正态分布
#从图上可以看出，方差越小，图形越凸
getNormDensity <- function(params) {
  sigmas <- params$sigma
  x <- seq(-max(sigmas)*3, max(sigmas)*3, min(sigmas)/10)
  d <- cbind(x, apply(params, 1, function(param) dnorm(x, param["mu"], param["sigma"])))
  colnames(d) <- c("x", paste(params$mu, params$sigma))
  d <- as.data.frame(d)
  data <- melt(d, id=c('x'))
  names(data) <- c("x", "group", "y")
  data
}

params <- data.frame(mu=rep(0, 12), sigma=c(0.1, 0.2, 0.5, 1, 2, 4, 8, 12, 16, 24, 32, 40))
showPlots(m=2, n=2, params=params, fun=getNormDensity)

#二项分布比较好理解
getBinomDensity <- function(params) {
  d.names <- apply(params, 1, function(x) paste(x["size"], x["prob"]))
  x <- 0:max(params["size"])
  d <- cbind(x, apply(params, 1, function(param) dbinom(x, param["size"], param["prob"])))
  colnames(d) <- c("x", d.names)
  d <- as.data.frame(d)
  data <- melt(d, id=c('x'))
  names(data) <- c("x", "group", "y")
  params.norm <- data.frame(mu=params$size * params$prob, sigma=round(sqrt(params$size * params$prob * (1-params$prob)),2))
  data.norm <- getNormDensity(params.norm)  
  rbind(data, data.norm)
}

getBinomParams <- function() {
  prob <- c(0.01, 0.1, 0.2, 0.5, 0.8)
  n <- c(10, 20, 40, 60, 80, 100)  #, 50, 100, 500, 2000, 5000
  params <- sapply(n, function(p) prob)
  colnames(params)=n
  params <- melt(params)
  params <- params[,c(2,3)]
  colnames(params) <- c("size", "prob")
  params
}

params <- getBinomParams()
showPlots(m=2, n=3, params=params, fun=getBinomDensity)


#二项分布，当n很小，p很大的时候, 近似于正态分布。这又如何理解


#泊松分布可以理解为二项分布的一种应用， Lambda/n = p, p是两项分布的一个概率
getPoisDensity <- function(lambdas) {
  sigmas <- ceiling(sqrt(lambdas))
  x <- seq(0, max(sigmas)*8, 1)
  d <- cbind(x, sapply(lambdas, function(lambda) dpois(x, lambda)))
  colnames(d) <- c("x", lambdas)
  print(lambdas)
  d <- as.data.frame(d)
  data <- melt(d, id=c('x'))
  names(data) <- c("x", "group", "y")
  data
}

params <- data.frame(sigma=c(0.1, 0.2, 0.5, 1, 2, 4, 8, 12, 16, 24, 32, 40))
showPlots(m=2, n=2, params=params, fun=getPoisDensity)
