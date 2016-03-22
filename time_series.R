library(TSA)
setwd("D:/xujian/repository/reading/dm/time_series_r/Datasets")


cor.prev <- function(data){
  len <- length(data)
  cor(data[2:len], zlag(data)[2:len])
}

process1 <- function(data, data.lm){
  #时间序列图，并显示季节标志
  print('Start Time Series Diagram')
  plot(data, type='l', main='Time Series')
  points(y=data,x=time(data),pch=as.vector(season(data)))
  summary(data.lm)
  
  readline(prompt = "Press any key to show Fitted Information ")
  plot(ts(fitted(data.lm), freq=frequency(data),start=start(data)), type='l', ylim=range(c(fitted(data.lm),data))) 
       
  points(data, type='o')
  
  readline(prompt = "Press any key to show Standardized Residuals")
  plot(rstudent(data.lm),x=as.vector(time(data)),ylab='Standardized Residuals',type='l')
  points(y=rstudent(data.lm),x=time(data),pch=as.vector(season(data)))
  
  readline(prompt = "Press any key to show Runs test")
  print(runs(rstudent(data.lm)))
  
  readline(prompt = "Press any key to show sample autocorrelations for the standardized residuals")
  acf(rstudent(data.lm))
    
  readline(prompt = "Press any key to show the distribution of the standardized residuals")
  hist(rstudent(data.lm),xlab='Standardized Residuals')
  
  readline(prompt = "Press any key to show the normality of the standardized residuals(QQ & shapiro.test)")  
  qqnorm(rstudent(data.lm)); qqline(rstudent(data.lm))
  shapiro.test(rstudent(data.lm))
}

# Exhibit 1.1
win.graph(width=4.875, height=2.5,pointsize=8)
data(larain)
plot(larain,ylab='Inches',xlab='Year',type='o')

# Exhibit 1.2
win.graph(width=3, height=3,pointsize=8)
plot(y=larain,x=zlag(larain),ylab='Inches',xlab='Previous Year Inches')

cor.prev(larain)


# Exhibit 1.3
win.graph(width=4.875, height=2.5,pointsize=8)
data(color)
plot(color,ylab='Color Property',xlab='Batch',type='o')

# Exhibit 1.4
win.graph(width=3, height=3,pointsize=8)
plot(y=color,x=zlag(color),ylab='Color Property',
     xlab='Prevous Batch Color Property')
cor.prev(color)

# Exhibit 1.5
win.graph(width=4.875, height=2.5,pointsize=8)
data(hare)
plot(hare,ylab='Abundance',xlab='Year',type='o')

# Exhibit 1.6
win.graph(width=3, height=3,pointsize=8)
plot(y=hare,x=zlag(hare),ylab='Abundance',xlab='Previous Year Abundance')
cor.prev(hare)

# Exhibit 1.7
win.graph(width=4.875, height=2.5,pointsize=8)
data(tempdub)
plot(tempdub,ylab='Temperature',type='o')
cor.prev(tempdub)


# Exhibit 1.8
data(oilfilters)
plot(oilfilters,type='o',ylab='Sales')
cor.prev(oilfilters)

# Exhibit 1.9
plot(oilfilters,type='l',ylab='Sales')
Month=c("J","A","S","O","N","D","J","F","M","A","M","J")
points(oilfilters,pch=Month)

# Alternatively, the exhibit can be reproduced by the following commands
plot(oilfilters,type='l',ylab='Sales')
points(y=oilfilters,x=time(oilfilters),pch=as.vector(season(oilfilters)))



#####################################
#Exercise 1 
#####################################
setwd("D:/xujian/repository/reading/dm/time_series_r/Datasets")
#1.1
larain1 <- read.csv("larain.dat", header=T)
plot(y=larain1$larain,x=c(NA, larain1[2:nrow(larain1)-1,1]),ylab='Inches',xlab='Previous Year Inches')

#1.2
color1 <- scan('color.dat', skip=1)
plot(color1,ylab='Color Property',xlab='Batch',type='o')

#1.3
#布朗运动(正态分布)
brownian_motion <- function(n, mu=0, sigma=1){
  element <- rnorm(n, mean=mu, sd =sigma )
  data.frame(x1 <- element, x2<-cumsum(element))
}

m <- brownian_motion(48)
plot(m$x2,ylab='brownian_motion',xlab='time',type='o')
points(m$x1, type='o',col='blue')

#1.4 卡方分布运动
chisq_motion <- function(n, df){
  element <- rchisq(n, df=df)
  data.frame(x1 <- element, x2<-cumsum(element))
}

m <- chisq_motion(48, 2)
plot(m$x2,ylab='chisq_motion',xlab='time',type='o')
points(m$x1, type='o',col='blue')

#1.5 t分布
t_motion <- function(n, df){
  element <- rt(n, df=df)
  data.frame(x1 <- element, x2<-cumsum(element))
}

m <- t_motion(48, 5)
plot(m$x2,ylab='t_motion',xlab='time',type='o')
points(m$x1, type='o',col='blue')

#1.6 
tempdub1 <- scan('tempdub.dat', skip=1)
tempdub1 <- ts(tempdub1, start=c(1964, 1), frequency=12)
plot(tempdub1,ylab='sales',xlab='month',type='l')
plot.ts(tempdub1)
Month=c("J","A","S","O","N","D","J","F","M","A","M","J")
points(tempdub1,pch=Month)

library(TSA)
# Exhibit 2.1
win.graph(width=4.875, height=2.5,pointsize=8)
# rwalk contains a simulated random walk
data(rwalk)
plot(rwalk,type='o',ylab='Random Walk')

# R code for simulating a random walk with, say 60, iid standard normal errors
n=60
set.seed(12345) # intialize the random number so that the simulation can be 
# reproducible.
sim.random.walk=ts(cumsum(rnorm(n)),freq=1,start=1)
plot(sim.random.walk,type='o',ylab='Another Random Walk')

#####################################
#Exercise 2 
#####################################
setwd("D:/xujian/repository/reading/dm/time_series_r/Datasets")
#2.3

1 -2 1
1 -2 1
1 -3 3 -1
1 -3 3 -1
1  -4 6 -4  1
1 -4 6 -4  1
1  -5 10 -10 5 -1

library(TSA)
# Exhibit 3.1
# time(rwalk) yields a time series of the time epoches when the random walk was sampled.
data(rwalk)
model1=lm(rwalk~time(rwalk))
summary(model1)

# Exhibit 3.2
win.graph(width=4.875, height=2.5,pointsize=8)
# rwalk contains a simulated random walk
plot(rwalk,type='o',ylab='y')
abline(model1) # add the fitted least squares line

# 检查随机游走的自相关函数。

# Exhibit 3.3
# season(tempdub) creates a vector of the month index of the data as a factor 
data(tempdub)
month.=season(tempdub) # the period sign is included to make the printout from
# the commands two line below clearer; ditto below.
model2=lm(tempdub~month.-1) # -1 removes the intercept term 
summary(model2)
sum(abs(tempdub - fitted(model2))/abs(tempdub))/length(tempdub)

# Exhibit 3.4
model3=lm(tempdub~month.) # intercept is automatically included so one month (Jan) is dropped
summary(model3)

win.graph(width=4.875, height=2.5,pointsize=8)
plot(ts(fitted(model3),freq=12,start=c(1964,1)),ylab='Temperature',type='l',
     ylim=range(c(fitted(model3),tempdub))) 
points(tempdub)
sum(abs(tempdub - fitted(model3))/abs(tempdub))/length(tempdub)
fitted(model3)[1:12]
model3$coefficients[1:12] %*% diag(12) + model3$coefficients[1]

# Exhibit 3.5
# first creates the first pair of harmonic functions and then fit the model
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.)
summary(model4)
sum(abs(tempdub - fitted(model4))/abs(tempdub))/length(tempdub)

fitted(model4)[1:12]
tt <- cbind(rep(1, nrow(har.)), har.) %*% model4$coefficients 
tt[1:12]


# Exhibit 3.6
win.graph(width=4.875, height=2.5,pointsize=8)
plot(ts(fitted(model4),freq=12,start=c(1964,1)),ylab='Temperature',type='l',
     ylim=range(c(fitted(model4),tempdub))) # the ylim option ensures that the 
# y axis has a range that fits the raw data and the fitted values
points(tempdub)

# Exhibit 3.7
data(rwalk)
model1=lm(rwalk~time(rwalk))
summary(model1)

# Exhibit 3.8
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',
     ylab='Standardized Residuals',type='o')

# Exhibit 3.9
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',
     ylab='Standardized Residuals',type='l')
points(y=rstudent(model3),x=as.vector(time(tempdub)),
       pch=as.vector(season(tempdub)))

(tempdub[1:12] - fitted(model3)[1:12])/sqrt(sum((tempdub - fitted(model3))^2/(length(tempdub)-12)))*144/132
rstudent(model3)[1:12]
#残差标准差计算方式
df.residual(model3);length(tempdub)-12  #144-12  
deviance(model3); sum((tempdub - fitted(model3))^2)
sqrt(deviance(model)/df.residual(model));sqrt(sum((tempdub - fitted(model3))^2/(length(tempdub)-12)))

#actual-predict
infl = lm.influence(model3, do.coef = FALSE)
infl$wt.res[1:12]
(tempdub - fitted(model3))[1:12]

# Exhibit 3.10
plot(y=rstudent(model3),x=as.vector(fitted(model3)),xlab='Fitted Trend Values',
     ylab='Standardized Residuals',type="n")
points(y=rstudent(model3),x=as.vector(fitted(model3)),
       pch=as.vector(season(tempdub)))

# Exhibit 3.11
hist(rstudent(model3),xlab='Standardized Residuals',main='')

# Exhibit 3.12
win.graph(width=3, height=3,pointsize=8)
qqnorm(rstudent(model3),main='')

# Exhibit 3.13
win.graph(width=4.875, height=3,pointsize=8)
acf(rstudent(model3),main='')

# Exhibit 3.14
plot(y=rstudent(model1),x=as.vector(time(rwalk)),ylab='Standardized Residuals',
     xlab='Time',type='o')

# Exhibit 3.15
win.graph(width=4.875, height=3,pointsize=8)
plot(y=rstudent(model1),x=fitted(model1),ylab='Standardized Residuals',
     xlab='Fitted Trend Values',type='p')

# Exhibit 3.16
acf(rstudent(model1),main='')

# Exhibit 3.17
data(larain)
win.graph(width=4.875, height=3,pointsize=8)
qqnorm(larain);qqline(larain)

#####################################
#Exercise 3 
#####################################
setwd("D:/xujian/repository/reading/dm/time_series_r/Datasets")
#3.4
hours <- read.csv("hours.dat", header=T)
hours <- ts(hours, start=c(1982, 7), frequency=12)
plot(hours,ylab='work hours',xlab='month',type='o')
plot(hours,ylab='work hours',xlab='month',type='l')
points(y=hours,x=time(hours),pch=as.vector(season(hours)))

#3.5
wages <- read.csv("wages.dat", header=T)
wages <- ts(wages, start=c(1981, 7), frequency=12)
plot(wages,ylab='wage',xlab='month',type='l')
points(y=wages,x=time(wages),pch=as.vector(season(wages)))

month.=time(wages) 
wages.lm=lm(wages~month.) # -1 removes the intercept term 
summary(wages.lm)

# plot(ts(fitted(wages.lm),freq=12,start=c(1981,7)),ylab='wage',type='l',
#      ylim=range(c(fitted(wages.lm),wages))) 
plot(wages,ylab='wage',xlab='month',type='o')
abline(wages.lm)

#R-squared is 0.9728. look good, but
y <- rstudent(wages.lm)
plot(y,x=as.vector(time(wages)),ylab='Standardized Residuals',type='o')

#采用二次项拟合
wages.lm2=lm(wages~time(wages)+I(time(wages)^2))
summary(wages.lm2)
y <- rstudent(wages.lm2)
plot(wages,ylab='wage',xlab='month',type='o')
points(ts(fitted(wages.lm2), freq=12,start=c(1981,7)),type='l') 

#从残差图来看，结果还不是非常理想。
y <- rstudent(wages.lm2)
plot(y,x=as.vector(time(wages)),ylab='Standardized Residuals',type='o')

#3.6
sales <- read.csv("beersales.dat", header=T)
sales <- ts(sales, start=c(1975, 1), frequency=12)

#a)
plot(sales,ylab='sales',xlab='month',type='o')

#b)
plot(sales,ylab='sales',xlab='month',type='l')
points(y=sales,x=time(sales),pch=as.vector(season(sales)))

#c)
month.=season(sales) 
sales.lm=lm(sales~month.) # -1 removes the intercept term 
summary(sales.lm)
y <- rstudent(sales.lm)

#查看拟合情况
plot(ts(fitted(sales.lm), freq=12,start=c(1975,1)),type='l') 
points(sales)

#d)
plot(y,x=as.vector(time(sales)),ylab='Standardized Residuals',type='o')

#e)
month.=season(sales) 
sales.lm2=lm(sales~month.+time(sales)+I(time(sales)^2)) # -1 removes the intercept term 
summary(sales.lm2)
y <- rstudent(sales.lm2)

#查看拟合情况
plot(ts(fitted(sales.lm2), freq=12,start=c(1975,1)),type='l') 
points(sales)
plot(y,x=as.vector(time(sales)),ylab='Standardized Residuals',type='l')
points(y=rstudent(sales.lm2),x=as.vector(time(sales)), pch=as.vector(season(sales)))

#3.7
winnebago <- read.csv("winnebago.dat", header=T)
winnebago <- ts(winnebago, start=c(1966, 11), frequency=12)

#a)
win.graph(width=4.875, height=2.5,pointsize=8)
plot(winnebago,ylab='sales',xlab='month',type='o')
plot(winnebago,ylab='sales',xlab='month',type='l')
points(y=winnebago,x=time(winnebago),pch=as.vector(season(winnebago)))

#b)
winnebago.lm=lm(winnebago~time(winnebago)) # -1 removes the intercept term 
summary(winnebago.lm)

#查看拟合情况
plot(ts(fitted(winnebago.lm), freq=12,start=c(1966,11)),type='l') 
points(winnebago)
plot(rstudent(winnebago.lm),x=as.vector(time(winnebago)),ylab='Standardized Residuals',type='o')

#c)
winnebago1 <- log(winnebago)
win.graph(width=4.875, height=2.5,pointsize=8)
plot(winnebago1,ylab='sales',xlab='month',type='l')
points(y=winnebago1,x=time(winnebago1),pch=as.vector(season(winnebago1)))

#d)
winnebago1.lm=lm(winnebago1~time(winnebago1)) # -1 removes the intercept term 
summary(winnebago1.lm)

plot(ts(exp(fitted(winnebago1.lm)), freq=12,start=c(1966,11)),type='l') 
points(winnebago)
plot(rstudent(winnebago1.lm),x=as.vector(time(winnebago1)),ylab='Standardized Residuals',type='o')

#e)
winnebago1.lm2=lm(winnebago1~season(winnebago1)+time(winnebago1)) # -1 removes the intercept term 
summary(winnebago1.lm2)

plot(ts(exp(fitted(winnebago1.lm2)), freq=12,start=c(1966,11)),type='l') 
points(winnebago)
plot(rstudent(winnebago1.lm2),x=as.vector(time(winnebago1)),ylab='Standardized Residuals',type='o')

#3.8
retail <- read.csv("retail.dat", header=T)
retail <- ts(retail, start=c(1986, 1), frequency=12)

#a)
win.graph(width=4.875, height=2.5,pointsize=8)
plot(retail,ylab='sales',xlab='month',type='o')
plot(retail,ylab='sales',xlab='month',type='l')
points(y=retail,x=time(retail),pch=as.vector(season(retail)))

#b)
retail.lm=lm(retail~season(retail)+time(retail)) # -1 removes the intercept term 
summary(retail.lm)

#c)
plot(ts(fitted(retail.lm), freq=12,start=c(1986,1)),type='l') 
points(retail)
plot(rstudent(retail.lm),x=as.vector(time(retail)),ylab='Standardized Residuals',type='l')
points(y=rstudent(retail.lm),x=time(retail),pch=as.vector(season(retail)))

#3.9
prescrip <- read.csv("prescrip.dat", header=T)
prescrip <- ts(prescrip, start=c(1986, 8), frequency=12)

#a)
win.graph(width=4.875, height=2.5,pointsize=8)
plot(prescrip,ylab='cost',xlab='month',type='o')
plot(prescrip,ylab='cost',xlab='month',type='l')
points(y=prescrip,x=time(prescrip),pch=as.vector(season(prescrip)))

#b)
prescrip1 <- ts(zlag(prescrip)[2:length(prescrip)], start=c(1986, 9), frequency=12)
prescrip.per <- (prescrip-prescrip1)/prescrip1*100
#perprescrip=na.omit(100*(prescrip-zlag(prescrip))/zlag(prescrip))
win.graph(width=4.875, height=2.5,pointsize=8)
plot(prescrip.per,ylab='cost',xlab='month',type='o')
plot(prescrip.per,ylab='cost',xlab='month',type='l')
points(y=prescrip.per,x=time(prescrip.per),pch=as.vector(season(prescrip.per)))

#c)
har.=harmonic(prescrip.per,1);har.[1:9,]
prescrip.per.lm=lm(prescrip.per~har.) # -1 removes the intercept term 
summary(prescrip.per.lm)

#d)
plot(ts(fitted(prescrip.per.lm), freq=12,start=c(1986,9)),type='l') 
points(prescrip.per)
plot(rstudent(prescrip.per.lm),x=as.vector(time(prescrip.per)),ylab='Standardized Residuals',type='l')
points(y=rstudent(prescrip.per.lm),x=time(prescrip.per),pch=as.vector(season(prescrip.per)))

#3.10
hours <- read.csv("hours.dat", header=T)
hours <- ts(hours, start=c(1982, 7), frequency=12)
hours.lm <- lm(hours~time(hours)+I(time(hours)^2))
summary(hours.lm)
process1(hours, hours.lm)

#3.11
wages <- read.csv("wages.dat", header=T)
wages <- ts(wages, start=c(1981, 7), frequency=12)
wages.lm=lm(wages~time(wages)+I(time(wages)^2)) # -1 removes the intercept term 
summary(wages.lm)
process1(wages, wages.lm)

#3.12
sales <- read.csv("beersales.dat", header=T)
sales <- ts(sales, start=c(1975, 1), frequency=12)
sales.lm=lm(sales~season(sales)+time(sales)+I(time(sales)^2)) # -1 removes the intercept term 
summary(sales.lm)
process1(sales, sales.lm)

#3.13
winnebago <- read.csv("winnebago.dat", header=T)
winnebago <- ts(winnebago, start=c(1966, 11), frequency=12)
winnebago.lm=lm(log(winnebago)~season(winnebago)+time(winnebago)) # -1 removes the intercept term 
summary(winnebago.lm)
process1(log(winnebago), winnebago.lm)

#3.14
retail <- read.csv("retail.dat", header=T)
retail <- ts(retail, start=c(1986, 1), frequency=12)
retail.lm=lm(retail~season(retail)+time(retail)) # -1 removes the intercept term 
summary(retail.lm)
process1(retail, retail.lm)

#3.15
prescrip <- read.csv("prescrip.dat", header=T)
prescrip <- ts(prescrip, start=c(1986, 8), frequency=12)
perprescrip <- na.omit(100*(prescrip-zlag(prescrip))/zlag(prescrip))
har.=harmonic(perprescrip,1);
perprescrip.lm=lm(perprescrip~har.) # -1 removes the intercept term 
summary(perprescrip.lm)
process1(perprescrip, perprescrip.lm)

#3.16
phi=seq(from=-0.99,to=0.99,by=0.1)
plot(y=(1+phi)/(1-phi),x=phi,type='l',xlab=expression(phi), 
       ylab=expression((1+phi)/(1-phi)))

#chapter 4
library(TSA)
setwd("D:/xujian/repository/reading/dm/time_series_r/Datasets")
# Exhibit 4.2
win.graph(width=4.875, height=3,pointsize=8)
data(ma1.2.s)
plot(ma1.2.s,ylab=expression(Y[t]),type='o')

# An MA(1) series with MA coefficient equal to -0.9 and 
# of length n=100 can be simulated by the following command
set.seed(12345) # initialize the seed of the random number generator so that
# the simulations can be reproduced.
y=arima.sim(model=list(ma=-c(-0.9)),n=100)
# Note that R uses the plus convention in the model formula so the 
# additional minus sign.  

# Exhibit 4.3
win.graph(width=3, height=3,pointsize=8)
plot(y=ma1.2.s,x=zlag(ma1.2.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.4
plot(y=ma1.2.s,x=zlag(ma1.2.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')


# Exhibit 4.5
win.graph(width=4.875, height=3,pointsize=8)
data(ma1.1.s)
plot(ma1.1.s,ylab=expression(Y[t]),type='o')

# An MA(1) series with ma coefficient equal to 0.9 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(MA=-c(0.9)),n=100)
# Note that R uses the plus convention in the MA model formula so the 
# additional minus sign.
start <- length(ma1.1.s)-length(y)+1;
n <- length(ma1.1.s)
points(y=y, x=start:n, col='blue', type='l');


# Exhibit 4.6
win.graph(width=3, height=3,pointsize=8)
plot(y=ma1.1.s,x=zlag(ma1.1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.7
plot(y=ma1.1.s,x=zlag(ma1.1.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.8
win.graph(width=4.875, height=3,pointsize=8)
data(ma2.s)
plot(ma2.s,ylab=expression(Y[t]),type='o')

# An MA(2) series with MA coefficients equal to 1 and -0.6 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ma=-c(1, -0.6)),n=100)
# Note that R uses the plus convention in the MA model formula so the 
# additional minus sign.  

# Exhibit 4.9
win.graph(width=3, height=3,pointsize=8)
plot(y=ma2.s,x=zlag(ma2.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.10
plot(y=ma2.s,x=zlag(ma2.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.11
plot(y=ma2.s,x=zlag(ma2.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')

# Exhibit 4.13
win.graph(width=4.875, height=3,pointsize=8)
data(ar1.s)
plot(ar1.s,ylab=expression(Y[t]),type='o')

# An AR(1) series with AR coefficient equal to 0.9 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ar=c(0.9)),n=100)
# Note that the R convention for the AR model formula is same as the book, so  
# NO additional minus sign.  

# Exhibit 4.14
win.graph(width=3, height=3,pointsize=8)
plot(y=ar1.s,x=zlag(ar1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.15
plot(y=ar1.s,x=zlag(ar1.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.16
plot(y=ar1.s,x=zlag(ar1.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')

# Exhibit 4.19
library(TSA)
win.graph(width=4.875, height=3,pointsize=8)
data(ar2.s)
plot(ar2.s,ylab=expression(Y[t]),type='o')

# An AR(2) series with AR coefficients equal to 1.5 and -0.75 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ar=c(1.5,-0.75)),n=100)
# Note that the R convention for the AR model formula is same as the book, so  
# NO additional minus sign. 

#####################################
#Exercise 4
#####################################
#4.2
#Compute the theoretical autocorrelation function or 
#partial autocorrelation function for an ARMA process.
#计算自相关函数
ARMAacf(ma=list(-.5,-.4))
ARMAacf(ma=list(-1.2,.7))
ARMAacf(ma=list(1,.6))

#4.5
ACF=ARMAacf(ar=.6,lag.max=8)
plot(y=ACF[-1],x=1:8,xlab='Lag',ylab='ACF',type='h'); abline(h=0)
ACF=ARMAacf(ar=-.6,lag.max=8)
plot(y=ACF[-1],x=1:8,xlab='Lag',ylab='ACF',type='h'); abline(h=0)
ACF=ARMAacf(ar=.95,lag.max=20)
plot(y=ACF[-1],x=1:20,xlab='Lag',ylab='ACF',type='h'); abline(h=0)
ACF=ARMAacf(ar=.3,lag.max=8)
plot(y=ACF[-1],x=1:8,xlab='Lag',ylab='ACF',type='h'); abline(h=0)

#4.9
#AR(2)的自相关函数: 两种方法
rho <- function(k, phi1, phi2){
  if (k==0) return(1)
  if (k==1) return(phi1/(1-phi2))
  phi1*rho(k-1, phi1, phi2) + phi2*rho(k-2, phi1, phi2)
}

rhoall <- function(k, phi1, phi2){
  ACF <- sapply(0:k, rho, phi1=phi1, phi2=phi2)
  plot(y=ACF[-1],x=1:k,xlab='Lag',ylab='ACF',type='h'); abline(h=0)
  cat('root:', polyroot(c(1,-phi1,-phi2)), '\n')
  ACF
}

rhoall(20, phi1=0.6, phi2=0.3)
rhoall(20, phi1=-0.4, phi2=0.5)
rhoall(20, phi1=1.2, phi2=-0.7)
rhoall(20, phi1=-1, phi2=-0.6)
rhoall(20, phi1=0.5, phi2=-0.9)
rhoall(20, phi1=-0.5, phi2=0.6)

rho <- function(k, phi1, phi2){
  d <- phi1^2+4*phi2
  if (d>0){
    g1 <- (phi1 - sqrt(d))/2
    g2 <- (phi1 + sqrt(d))/2
    ((1-g2^2)*g1^(k+1)-(1-g1^2)*g2^(k+1))/(g1-g2)/(1+g1*g2)  
  }
  else if (d<0){
    theta <- acos(phi1/2/sqrt(-phi2))
    phi <-  atan((1-phi2)/(1+phi2))
    r <- sqrt(-phi2) 
    cat('r=', r, '\n')
    cat('theta=', theta, '\n')
    cat('phi=', phi, '\n')
    r^k*sin(theta*k + phi)/sin(phi)
  }
  else
  {
    (1+(1+phi2)/(1-phi2)*k)*(phi1/2)^k
  }
}


rhoall(20, phi1=0.6, phi2=0.3)
rhoall(20, phi1=-0.4, phi2=0.5)
rhoall(20, phi1=1.2, phi2=-0.7)
rhoall(20, phi1=-1, phi2=-0.6)
rhoall(20, phi1=0.5, phi2=-0.9)
rhoall(20, phi1=-0.5, phi2=0.6)

#4.10
# Remember that R uses the negative of our theta values.
ACF=ARMAacf(ar=0.7,ma=-0.4,lag.max=20);ACF
plot(y=ACF[-1],x=1:20,xlab='Lag',ylab='ACF',type='h'); abline(h=0)
ACF=ARMAacf(ar=0.7,ma=0.4,lag.max=20);ACF
plot(y=ACF[-1],x=1:20,xlab='Lag',ylab='ACF',type='h'); abline(h=0)

#4.12
ARMAacf(ma=c(1,-6))
ARMAacf(ma=c(-1/6,-1/6))

#
