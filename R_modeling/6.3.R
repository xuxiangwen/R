#(1)
d <- data.frame(
  x = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 11, 12, 12, 12),
  y = c(0.6, 1.6, 0.5, 1.2, 2, 1.3, 2.5, 2.2, 2.4, 1.2, 3.5, 4.1, 5.1, 5.7, 3.4, 9.7, 8.6, 4, 5.5, 10.5, 17.5, 13.4, 4.5, 30.4, 12.4, 13.4, 26.2, 7.4)	
)

x = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 11, 12, 12, 12)
y = c(0.6, 1.6, 0.5, 1.2, 2, 1.3, 2.5, 2.2, 2.4, 1.2, 3.5, 4.1, 5.1, 5.7, 3.4, 9.7, 8.6, 4, 5.5, 10.5, 17.5, 13.4, 4.5, 30.4, 12.4, 13.4, 26.2, 7.4)  
lm.sol <- lm(y~x)
summary(lm.sol)
plot(y~x)
abline(lm.sol)

#(2)
#P-值=0.436>0.05, Intercept没有通过显著性检验(T检验)
#P-值=7.93e-06<0.05, x通过显著性检验(T检验)
#P-值=7.931e-06<0.05, 方程整体通过F检验

#(3)
y.res<-residuals(lm.sol)
y.fit<-predict(lm.sol)
y.rst<-rstandard(lm.sol)
plot(y.res~y.fit)
plot(y.rst~y.fit)

#从图形上看，误差并不呈现等方差,总体呈现喇叭口的样子

#(4)
lm.new<-update(lm.sol, sqrt(.)~.)
summary(lm.new)
plot(y~x)
abline(lm.new)
y.res<-residuals(lm.new)
y.fit<-predict(lm.new)
y.rst<-rstandard(lm.new)
plot(y.res~y.fit)
plot(y.rst~y.fit)
