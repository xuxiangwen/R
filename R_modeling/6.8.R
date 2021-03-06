X1 = c(70, 60, 70, 40, 40, 70, 70, 80, 60, 30, 80, 40, 60, 40, 20, 50, 50, 40, 
       80, 70, 60, 90, 50, 70, 20, 80, 60, 50, 70, 40, 30, 30, 40, 60, 80, 70, 
       30, 60, 80, 70)  # 生活行为能力
X2 = c(64, 63, 65, 69, 63, 48, 48, 63, 63, 53, 43, 55, 66, 67, 61, 63, 66, 68, 
       41, 53, 37, 54, 52, 50, 65, 52, 70, 40, 36, 44, 54, 59, 69, 50, 62, 68, 
       39, 49, 64, 67)  # 年龄
X3 = c(5, 9, 11, 10, 58, 9, 11, 4, 14, 4, 12, 2, 25, 23, 19, 4, 16, 12, 12, 
       8, 13, 12, 8, 7, 21, 28, 13, 13, 22, 36, 9, 87, 5, 22, 4, 15, 4, 11, 10, 
       18)  # 诊断到直入研究时间
X4 = c(rep(1, 7), rep(2, 7), rep(3, 2), rep(0, 4), rep(1, 8), rep(2, 4), rep(3, 
                                                                             3), rep(0, 5))  # 肿瘤类型
X5 = c(rep(1, 21), rep(0, 19))  # 化疗方法
Y = c(1, rep(0, 11), 1, rep(0, 5), 1, 1, 0, 1, 1, 1, 0, 1, rep(0, 12), 1, 1)
lung.df = data.frame(X1, X2, X3, X4, X5, Y)
lung.df

lung.glm1 = glm(Y ~ X1 + X2 + X3 + X4 + X5, family = binomial, data = lung.df)
summary(lung.glm1)

#肺癌生存时间的模型是P=exp(-7.0114+0.0999X1+0.01415X2+0.01749X3-1.083X4-0.613X5)/1+exp(-7.0114+0.0999X1+0.01415X2+0.01749X3-1.083X4-0.613X5)
#X1~X5对P(Y=1)的综合影响不够显著。X4肿瘤类型是最主要的影响因素，但不够显著

#计算病人的生存概率
lung.pre1 = predict(lung.glm1, lung.df[1:5])
p.lung.pre1 = exp(lung.pre1)/(1 + exp(lung.pre1))
p.lung.pre1

#逐步回归选取自变量并计算病人生存概率
lung.glm2 = step(lung.glm1)
summary(lung.glm2)
lung.pre2 = predict(lung.glm2, lung.df[1:5])
p.lung.pre2 = exp(lung.pre2)/(1 + exp(lung.pre2))
p.lung.pre2

#比较两个模型，从估计病人生存时间角度，使用简化模型更方便，且在仅考虑的X1，X4两个因素更显著