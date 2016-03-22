X1 = c(1, 1, 1, 1, 0, 0, 0, 0)  #是否用抗生素
X2 = c(1, 1, 0, 0, 1, 1, 0, 0)  #是否有危险因子
X3 = c(1, 0, 1, 0, 1, 0, 1, 0)  #事先有计划
success = c(1, 11, 0, 0, 28, 23, 8, 0)  #有感染
fail = c(17, 87, 2, 0, 30, 3, 32, 9)  #无感染
infection = data.frame(X1, X2, X3, success, fail)
infection$Ymat = cbind(infection$success, infection$fail)

glm.sol = glm(Ymat ~ X1 + X2 + X3, family = binomial, data = infection)
summary(glm.sol)

#感染的回归模型是P=exp(-0.82-3.2544X1+2.0299X2-1.072X3)/1+exp(-0.82-3.2544X1+2.0299X2-1.072X3)
#根据上述模型，我们认为使用抗生素并有计划，将很大可能无感染。而有危险因子将很大可能有感染