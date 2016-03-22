setwd('C:/xujian/repository/reading/R Training/Week 08/R08')
source('MINE.R')
MINE("WHO.csv","all.pairs", required.common.vals.fraction=0.5)

#查看授权专利（Patents_granted）和其他变量的MIC(>0.7)
result<-read.csv("Who.csv,allpairs,cv=0.5,B=n^0.6,Results.csv",header=T)
result1 <- result[which(result$X.var=='Patents_granted' & result$MIC..strength.>0.7),][1:3]

#授权专利和相关变量的原始数据
who <- read.csv("WHO.csv", header=TRUE)
vars <- c('Patents_granted', as.character(result1$Y.var))
who1 <- who[vars]

#得到MIC和皮尔逊系数的比较
result2 <- data.frame(result1[1:3], 
  pearson = c(
    cor(who1[1], who1[2], use ='na.or.complete'),
    cor(who1[1], who1[3], use ='na.or.complete'),
    cor(who1[1], who1[4], use ='na.or.complete'),
    cor(who1[1], who1[5], use ='na.or.complete'),
    cor(who1[1], who1[6], use ='na.or.complete'),
    cor(who1[1], who1[7], use ='na.or.complete')
  )
)
result2
# X.var                                              Y.var MIC..strength.   pearson
# 438  Patents_granted                                Patent_applications        0.77992 0.6559446
# 722  Patents_granted Colon_and_Rectum_cancer_number_of_new_female_cases        0.73364 0.8143494
# 837  Patents_granted             Lung_cancer_number_of_new_female_cases        0.71791 0.5450172
# 840  Patents_granted               Lung_cancer_number_of_new_male_cases        0.71765 0.4750611
# 879  Patents_granted   Colon_and_Rectum_cancer_number_of_new_male_cases        0.71403 0.7853667
# 1010 Patents_granted                              Broadband_subscribers        0.70099 0.8397472
#看到结果，接下想尝试做几个分析
#1. Patents_granted和Patent_applications之间做一个回归分析
#2. 为何Patents_granted和几个癌症的新增病例有这么高的相关性
plot(who1$Patents_granted~who1$Patent_applications)
ls.sol <- lm(who1$Patents_granted~who1$Patent_application)
summary(ls.sol)
abline(ls.sol)


plot(result2[3:4], xlim=c(0,1), ylim=c(0,1))






