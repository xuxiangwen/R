#

#决策树之三国争霸: http://xccds1977.blogspot.com/2012/11/blog-post_28.html


#决策树基本函数
trans <- function(p){
  if (is.vector(p)) p<-t(p)
  if (is.table(p)) p<-as.matrix(p)
  if (dim(p)[2]==1) p <- t(p)
  p
}

gi <- function(p){
  if (sum(p)==0) return(0)
  1-sum((p/sum(p))^2)
}

gini <- function(p){
  p <- trans(p)
  round(sum(apply(p, 1, gi)*rowSums(p)/sum(p)), 4)
}

gini_each <- function(p){
  sapply(p, function(x) gi(c(x,1-x)))
}

ce_ <- function(p){
  if (sum(p)==0) return(0)
  1-max(p)/sum(p)
}

ce <- function(p){
  p <- trans(p)
  round(sum(apply(p, 1, ce_)*rowSums(p)/sum(p)), 4)  
}

ce_each <- function(p){
  sapply(p, function(x) ce1(c(x,1-x)))  
}

ent <- function(p){
  if (sum(p)==0) return(0)
  p <- p/sum(p)
  sum(sapply(p, function(x) if(x==0) 0 else -log2(x)*x))
}

entropy <- function(p){
  p <- trans(p)
  round(sum(apply(p, 1, ent)*rowSums(p)/sum(p)),4)  
}

entropy_each <- function(p){
  sapply(p, function(x) ent(c(x,1-x)))
}

gain <- function(p, fun=entropy){
  fun(colSums(p))-fun(p)
}

gain_ratio <- function(p, fun=entropy){
  gain(p, fun)/fun(rowSums(p))
}

#=========================================================

#Decision Tree
#Entropy, Gini, Classification Error
#二分类图形
entropy__ <- function(p){
  sapply(p, function(x) ent(c(x, 1-x)))
}
gini__ <- function(p){
  sapply(p, function(x) gi(c(x, 1-x)))
}
ce__ <- function(p){
  sapply(p, function(x) ce_(c(x, 1-x)))
}

x <- seq(0, 1, by=0.01)
curve(entropy__, 0, 1, 100, xlim=c(0,1), ylim=c(0,1)) #x,y轴限制
lines(x, entropy__(x)/2, col = "green")
lines(x, gini__(x), col = "red")
lines(x, ce__(x), col='Blue')

curve(gini__(x), 0, 1, 100, xlim=c(0,1), ylim=c(0,1), col='blue') #x,y轴限制

#用ggplot2来画
library(ggplot2)
x <- seq(0, 1, length=500)
d <- data.frame(x=x, en=entropy__(x),  gn=gini__(x), ce=ce__(x))
dplot <- ggplot(d, aes(x, en)) + geom_line(colour='green')
dplot <- dplot  %+%  geom_line(aes(y = gn), colour='red')
dplot <- dplot  %+%  geom_line(aes(y = ce), colour='blue') 
dplot

dplot <- ggplot(d, aes(x, en)) + geom_line(colour='blue') +  ylim(0, 1)
dplot

dplot <- ggplot(d, aes(x, gn)) + geom_line(colour='blue') +  ylim(0, 1)
dplot

#三分类图形
entropy___ <- function(p){
  apply(p, 1, function(x) ent(c(x[1], x[2], 1-x[1]-x[2])))
}

gini___ <- function(p){
  apply(p, 1, function(x) gi(c(x[1], x[2], 1-x[1]-x[2])))
}

ce___ <- function(p){
  apply(p, 1, function(x) ce_(c(x[1], x[2], 1-x[1]-x[2])))
}

library(scatterplot3d) #三维散点图
#c1 <- 11; c2 <- 0.1
c1 <- 101; c2 <- 0.01
p1<-matrix(rep(seq(0, 1, by=c2), c1), nrow=c1, byrow=T) 
x<-p1[lower.tri(p1, diag=T)]
p2<-matrix(rep(seq(1, 0, by=-c2), c1), nrow=c1) 
y<-p2[lower.tri(p2, diag=T)]

z <- entropy___(cbind(x, y)) 
scatterplot3d(x, y, z, highlight.3d=TRUE, type='p',
              col.axis="blue", col.grid="lightblue", 
              main="entropy", pch=20, angle=100)
z <- gini___(cbind(x, y)) 
scatterplot3d(x, y, z, highlight.3d=TRUE, type='p',
              col.axis="blue", col.grid="lightblue", 
              main="gini", pch=20, angle=100)
z <- ce___(cbind(x, y)) 
scatterplot3d(x, y, z, highlight.3d=TRUE, type='p',
              col.axis="blue", col.grid="lightblue", 
              main="ce", pch=20, angle=100)

#scatterplot3d是一个力图在3D空间展示多维数据的R包
library(scatterplot3d)
z <- seq(-10, 10, 0.01); x <- cos(z); y <- sin(z)
scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", main="scatterplot3d - 1", pch=20)

#=========================================================

#----------------------------------------------------------------
#决策树的练习
#4.2 Consider the training examples shown in Table 4.1 for a binary classification problem.
cust <- scan(what='')
1 M Family Small C0
2 M Sports Medium C0
3 M Sports Medium C0
4 M Sports Large C0
5 M Sports Extra_Large C0
6 M Sports Extra_Large C0
7 F Sports Small C0
8 F Sports Small C0
9 F Sports Medium C0
10 F Luxury Large C0
11 M Family Large C1
12 M Family Extra_Large C1
13 M Family Medium C1
14 M Luxury Extra_Large C1
15 F Luxury Small C1
16 F Luxury Small C1
17 F Luxury Medium C1
18 F Luxury Medium C1
19 F Luxury Medium C1
20 F Luxury Large C1


customer <- data.frame(
  ID = cust[(1:20-1)*5+1],
  Gender = cust[(1:20-1)*5+2], 
  CarType = cust[(1:20-1)*5+3], 
  ShirtSize = cust[(1:20-1)*5+4], 
  Class = cust[(1:20-1)*5+5]
)


#4.2a Compute the Gini index for the overall collection of training examples.
f0 <- table(customer$Class)
g0 <- gini(f0);g0

#4.2b Compute the Gini index for the Customer ID attribute.
f1 <- table(customer$ID, customer$Class)
g1 <- gini(f1);g1

#4.2c Compute the Gini index for the Gender attribute.
f2 <- table(customer$Gender, customer$Class)
g2 <- gini(f2);g2

#4.2d Compute the Gini index for the Car Type attribute using multiway split.
f3 <- table(customer$CarType, customer$Class)
g3 <- gini(f3);g3

#4.2e Compute the Gini index for the Shirt Size attribute using multiwaysplit
f4 <- table(customer$ShirtSize, customer$Class)
g4 <- gini(f4);g4

#4.2f Which attribute is better,Gender, Car Type,orShirt Size?
#CarType because it's gini index is 0.1625 and is the minumum

#4.2g Explain why Customer IDshould not be used as the attribute test condition even though it has the lowest Gini.
(g0-g1)/gini(rowSums(f1));gain_ratio(f1, fun=gini)
(g0-g2)/gini(rowSums(f2));gain_ratio(f2, fun=gini)
(g0-g3)/gini(rowSums(f3));gain_ratio(f3, fun=gini)
(g0-g4)/gini(rowSums(f4));gain_ratio(f4, fun=gini)
#The attribute has no predictive power since new customers are assigned to newCustomer IDs.

#4.2a entropy
f0 <- table(customer$Class)
ent0 <- entropy(f0);ent0

#4.2b entropy
f1 <- table(customer$ID, customer$Class)
ent1 <- entropy(f1);ent1

#4.2c entropy
f2 <- table(customer$Gender, customer$Class)
ent2 <- entropy(f2);ent2

#4.2d entropy
f3 <- table(customer$CarType, customer$Class)
ent3 <- entropy(f3);ent3

#4.2e entropy
f4 <- table(customer$ShirtSize, customer$Class)
ent4 <- entropy(f4);ent4

#4.2f entropy
#CarType because it's entropy index is 0.3797 and is the minumum

#4.2g entropy
(ent0-ent1)/entropy(rowSums(f1));gain_ratio(f1, fun=entropy)
(ent0-ent2)/entropy(rowSums(f2));gain_ratio(f2, fun=entropy)
(ent0-ent3)/entropy(rowSums(f3));gain_ratio(f3, fun=entropy)
(ent0-ent4)/entropy(rowSums(f4));gain_ratio(f4, fun=entropy)

#4.2a classificiation Error
f0 <- table(customer$Class)
ce0 <- ce(f0);ce0

#4.2b classificiation Error
f1 <- table(customer$ID, customer$Class)
ce1 <- ce(f1);ce1

#4.2c classificiation Error
f2 <- table(customer$Gender, customer$Class)
ce2 <- ce(f2);ce2

#4.2d classificiation Error
f3 <- table(customer$CarType, customer$Class)
ce3 <- ce(f3);ce3

#4.2e classificiation Error
f4 <- table(customer$ShirtSize, customer$Class)
ce4 <- ce(f4);ce4

c(ce0, ce1, ce2, ce3, ce4)

#4.2f classificiation Error
#CarType because it's ce index is 0.1 and is the minumum

#4.2g classificiation Error
(ce0-ce1)/ce(rowSums(f1));gain_ratio(f1, fun=ce)
(ce0-ce2)/ce(rowSums(f2));gain_ratio(f2, fun=ce)
(ce0-ce3)/ce(rowSums(f3));gain_ratio(f3, fun=ce)
(ce0-ce4)/ce(rowSums(f4));gain_ratio(f4, fun=ce)

#4.3 Consider the training examples shown in Table 4.2 for a binary classification problem.
e3 <- scan(what='')
1 T T 1.0 1
2 T T 6.0 1
3 T F 5.0 0
4 F F 4.0 1
5 F T 7.0 0
6 F T 3.0 0
7 F F 8.0 0
8 T F 7.0 1
9 F T 5.0 0


e3 <- data.frame(
  Ins = e3[(1:9-1)*5+1],
  a1 = e3[(1:9-1)*5+2], 
  a2 = e3[(1:9-1)*5+3], 
  a3 = as.numeric(e3[(1:9-1)*5+4]), 
  Class = e3[(1:9-1)*5+5]
)

#4.3a What is the entropy of this collection of training examples with respect to the positive class?
f0 <- table(e3$Class)
ent0 <-entropy(f0);ent0
f0; -log2(5/9)*5/9-log2(4/9)*4/9

#4.3b What are the information gains ofa1anda2 relative to these training examples?
f1 <- table(e3$a1, e3$Class)
ent1 <-entropy(f1);ent1
f1;(-log2(1/4)*1/4-log2(3/4)*3/4)*4/9 + (-log2(1/5)*1/5-log2(4/5)*4/5)*5/9

f2 <- table(e3$a2, e3$Class)
ent2 <-entropy(f2);ent2
f2;(-log2(2/4)*2/4-log2(2/4)*2/4)*4/9 + (-log2(3/5)*3/5-log2(2/5)*2/5)*5/9

gain(f1); ent0-ent1
gain(f2); ent0-ent2

#split information 
f1; si1 <- -log2(5/9)*5/9-log2(4/9)*4/9; si1
f2; si2 <- -log2(4/9)*4/9-log2(5/9)*5/9; si2
gain_ratio(f1); (ent0-ent1)/si1
gain_ratio(f2); (ent0-ent2)/si2

#4.3c For a3, which is a continuous attribute, compute the information gain for every possible split.
sp<- function(x, data, class, fun) {
  tf <- table(data<=x, class)
  fun(tf)
}

splits <- sort(unique(e3$a3))
sapply(splits, sp, data=e3$a3, class=e3$Class, fun=entropy)
sapply(splits, sp, data=e3$a3, class=e3$Class, fun=gain)
sapply(splits, sp, data=e3$a3, class=e3$Class, fun=gain_ratio)
#the best split point: 2
(splits[1]+splits[2])/2

#4.3d What is the best split (among a1, a2,and a3)  according to the information gain?
gain(table(e3$a1, e3$Class))
gain(table(e3$a2, e3$Class))
splits <- sort(unique(e3$a3))
max(apply(t(splits), 2, sp, data=e3$a3, class=e3$Class, fun=gain))
#a1 is the best split

#4.3e What is the best split (between a1 and a2) according to the classification error rate?
ce(table(e3$a1, e3$Class))
ce(table(e3$a2, e3$Class))
splits <- sort(unique(e3$a3))
max(apply(t(splits), 2, sp, data=e3$a3, class=e3$Class, fun=ce))
#choose order: a1, a2, a3

#4.3f
gini(table(e3$a1, e3$Class))
gini(table(e3$a2, e3$Class))
splits <- sort(unique(e3$a3))
max(apply(t(splits), 2, sp, data=e3$a3, class=e3$Class, fun=gini))
#choose order: a1, a2, a3

#4.5 Consider the following data set for a binary class problem.
e5 <- scan(what='')
T F +
T T +
T T +
T F -
T T +
F F -
F F -
F F -
T T -
T F -
  
e5 <- data.frame(
  A = e5[(1:10-1)*3+1],
  B = e5[(1:10-1)*3+2],  
  Class = e5[(1:10-1)*3+3]
)

#4.5a Calculate the information gain when splitting on A and B.Whichattribute would the decision tree induction algorithm choose?
f0 <- table(e5$Class); ent0<- entropy(f0); ent0
f1 <- table(e5$A, e5$Class); ent1<- entropy(f1);ent0-ent1;gain(f1)
f2 <- table(e5$B, e5$Class); ent2<- entropy(f2);ent0-ent2;gain(f2)
#A is better

#4.5b Calculate the gain in the Gini index when splitting on A and B.Which attribute would the decision tree induction algorithm choose
f0 <- table(e5$Class); gini0<- gini(f0); gini0
f1 <- table(e5$A, e5$Class); gini1<- gini(f1);gini0-gini1;gain(f1, fun=gini)
f2 <- table(e5$B, e5$Class); gini2<- gini(f2);gini0-gini2;gain(f2, fun=gini)
#B is better

#4.5c Figure 4.13 shows that entropy and the Gini index are both monotonously 
#increasing on the range [0, 0.5] and they are both monotonously decreasing on 
#the range [0.5, 1]. Is it possible that information gain and the gain in the 
#Gini index favor different attributes? Explain.

# Yes, even though these measures have similar range and monotonous
# behavior, their respective gains, ∆, which are scaled differences of the
# measures, do not necessarily behave in the same way, as illustrated by
# the results in parts (a) and (b).

#画出上面的示意
library(ggplot2)
points_dispose <- function(f, fun1, fun2){
  x <- f[,2]/rowSums(f)
  data.frame(x=c(x, sum(f[,2])/sum(f)), y=c(fun2(x), fun1(f)))  
}
x <- seq(0, 1, length=500)
d <- data.frame(x=x, en=entropy__(x),  gi=gini__(x), ce=ce__(x))

f1 <- table(e5$A, e5$Class); 
f2 <- table(e5$B, e5$Class); 
en_p1 <- points_dispose(f1, entropy, entropy__)
en_p2 <- points_dispose(f2, entropy, entropy__)
gi_p1 <- points_dispose(f1, gini, gini__)
gi_p2 <- points_dispose(f2, gini, gini__)

dplot <- ggplot(d, aes(x, en)) 
dplot <- dplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  #去除背景的格线
dplot <- dplot + geom_line(colour='green')
dplot <- dplot + geom_vline(aes(xintercept=x), data=en_p1, linetype=2, alpha = I(1/8))
dplot <- dplot + geom_line(aes(x=x, y=y), data=en_p1, linetype=2, colour='blue', alpha = I(1/2))
dplot <- dplot + geom_point(aes(x=x, y=y), data=en_p1, linetype=2, colour='red')
dplot <- dplot + geom_text(aes(x=x+0.02, y=y), data=en_p1[3,], label='A')
dplot <- dplot + geom_vline(aes(xintercept=x), data=en_p2, linetype=2, alpha = I(1/8))
dplot <- dplot + geom_line(aes(x=x, y=y), data=en_p2, linetype=2, colour='blue', alpha = I(1/2))
dplot <- dplot + geom_point(aes(x=x, y=y), data=en_p2, linetype=2, colour='orange')
dplot <- dplot + geom_text(aes(x=x+0.02, y=y), data=en_p2[3,], label='B')

dplot
dplot <- dplot + geom_line(aes(y=gi),colour='yellow')
#dplot <- dplot + geom_vline(aes(xintercept=x), data=gi_p1, linetype=2, alpha = I(1/4))
dplot <- dplot + geom_line(aes(x=x, y=y), data=gi_p1, linetype=2, colour='blue', alpha = I(1/2))
dplot <- dplot + geom_point(aes(x=x, y=y), data=gi_p1, linetype=2, colour='red')
dplot <- dplot + geom_text(aes(x=x+0.02, y=y), data=gi_p1[3,], label='A')
#dplot <- dplot + geom_vline(aes(xintercept=x), data=gi_p2, linetype=2, alpha = I(1/4))
dplot <- dplot + geom_line(aes(x=x, y=y), data=gi_p2, linetype=2, colour='blue', alpha = I(1/2))
dplot <- dplot + geom_point(aes(x=x, y=y), data=gi_p2, linetype=2, colour='orange')
dplot <- dplot + geom_text(aes(x=x+0.02, y=y), data=gi_p2[3,], label='B')
dplot


#4.6 Consider the following set of training examples.
e6 <- scan(what='')
0 0 0 5 40
0 0 1 0 15
0 1 0 10 5
0 1 1 45 0
1 0 0 10 5
1 0 1 25 0
1 1 0 5 20
1 1 1 0 15

e6 <- data.frame(
  x = e6[(1:8-1)*5+1],
  y = e6[(1:8-1)*5+2],  
  z = e6[(1:8-1)*5+3],
  class1 = e6[(1:8-1)*5+4],
  class2 = e6[(1:8-1)*5+5],
  stringsAsFactors = F
)
e6$class1 <- as.numeric(e6$class1) 
e6$class2 <- as.numeric(e6$class2)  
e6

#4.6a Compute a two-level decision tree using the greedy approach described 
#in this chapter. Use the classification error rate as the criterion for
#splitting. What is the overall error rate of the induced tree?
library(plyr)
f_x <- ddply(.data=e6, .variable=c('x') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_x[,2:3]);entropy(f_x[,2:3])
f_y <- ddply(.data=e6, .variable=c('y') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_y[,2:3]);entropy(f_y[,2:3])
f_z <- ddply(.data=e6, .variable=c('z') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_z[,2:3]);entropy(f_z[,2:3])
com_set <- c(x=ce(f_x[,2:3]), y=ce(f_y[,2:3]), z=ce(f_z[,2:3]))
com_set
#选择z最为第一层split feature.

#z=0
f_x <- ddply(.data=e6[e6$z=='0',], .variable=c('x') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_x[,2:3])
f_y <- ddply(.data=e6[e6$z=='0',], .variable=c('y') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_y[,2:3])
c(ce(f_x[,2:3]), ce(f_y[,2:3]))
#对于第一个子节点，x和y有相同的ce， 都可以作为拆分点

#z=1
f_x <- ddply(.data=e6[e6$z=='1',], .variable=c('x') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_x[,2:3])
f_y <- ddply(.data=e6[e6$z=='1',], .variable=c('y') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_y[,2:3])
c(ce(f_x[,2:3]), ce(f_y[,2:3]))
#对于第二个子节点，x和y有相同的ce， 都可以作为拆分点

#4.6b Repeat part (a) usingXas the first splitting attribute and then choose
#the best remaining attribute for splitting at each of the two successor
#nodes. What is the error rate of the induced tree?
#use x as the first split
#x=0
f_z <- ddply(.data=e6[e6$x=='0',], .variable=c('x') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_z[,2:3])
f_y <- ddply(.data=e6[e6$x=='0',], .variable=c('y') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_y[,2:3])
c(ce(f_z[,2:3]), ce(f_y[,2:3]))
#对于第一个子节点，y以作为拆分点

#x=1
f_z <- ddply(.data=e6[e6$x=='1',], .variable=c('x') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_z[,2:3])
f_y <- ddply(.data=e6[e6$x=='1',], .variable=c('y') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f_y[,2:3])
c(ce(f_z[,2:3]), ce(f_y[,2:3]))
#对于第二个子节点，y以作为拆分点

f <- ddply(.data=e6, .variable=c('x', 'y') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f[,3:4])
#总的ce是0.1

#4.6c Compare the results of parts (a) and (b). Comment on the suitability
#of the greedy heuristic used for splitting attribute selection.
f <- ddply(.data=e6, .variable=c('z', 'y') , .fun=function(x) c(class1=sum(x['class1']), class2=sum(x['class2'])))
ce(f[,3:4])
#看来还是选择x,y好于z,y
#也就是4.6b的树更好
#This examples shows that a greedy heuristic does not always produce an optimal solution.

#4.7 The following table summarizes a data set with three attributesA, B, Cand
#two class labels +,−. Build a two-level decision tree.
e7 <- scan(what='')
T T T 5 0
F T T 0 20
T F T 20 0
F F T 0 5
T T F 0 0
F T F 25 0
T F F 0 0
F F F 0 25

e7 <- data.frame(
  A = e7[(1:8-1)*5+1],
  B = e7[(1:8-1)*5+2],  
  C = e7[(1:8-1)*5+3],
  Positive = e7[(1:8-1)*5+4],
  Negative = e7[(1:8-1)*5+5],
  stringsAsFactors = F
)
e7$Positive <- as.numeric(e7$Positive) 
e7$Negative <- as.numeric(e7$Negative)  
e7

#4.7a According to the classification error rate, which attribute would be
#chosen as the first splitting attribute? For each attribute, show the
#contingency table and the gains in classification error rate.
library(plyr)
f_A <- ddply(.data=e7, .variable=c('A') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_A[,2:3]);gain(f_A[,2:3], fun=gini);gain_ratio(f_A[,2:3]);gini(f_A[,2:3]);entropy(f_A[,2:3])
f_B <- ddply(.data=e7, .variable=c('B') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_B[,2:3]);gain(f_B[,2:3], fun=ce);gain_ratio(f_B[,2:3]);gini(f_B[,2:3]);entropy(f_B[,2:3])
f_C <- ddply(.data=e7, .variable=c('C') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_C[,2:3]);gain(f_C[,2:3], fun=ce);gain_ratio(f_C[,2:3]);gini(f_C[,2:3]);entropy(f_C[,2:3])
#选择A最为第一层split feature.

#4.7b Repeat for the two children of the root node.
#第一个子节点
f_A <- ddply(.data=e7[e7$A=='T',], .variable=c('A') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
f_B <- ddply(.data=e7[e7$A=='T',], .variable=c('B') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_B[,2:3]);gain(f_B[,2:3], fun=ce);gain_ratio(f_B[,2:3]);gini(f_B[,2:3]);entropy(f_B[,2:3])
f_C <- ddply(.data=e7[e7$A=='T',], .variable=c('C') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_C[,2:3]);gain(f_C[,2:3], fun=ce);gain_ratio(f_C[,2:3]);gini(f_C[,2:3]);entropy(f_C[,2:3])
#选择B or C最为第二个子节点split feature.

#第二个子节点
f_B <- ddply(.data=e7[e7$A=='F',], .variable=c('B') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_B[,2:3]);gain(f_B[,2:3], fun=ce);gain_ratio(f_B[,2:3]);gini(f_B[,2:3]);entropy(f_B[,2:3])
f_C <- ddply(.data=e7[e7$A=='F',], .variable=c('C') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_C[,2:3]);gain(f_C[,2:3], fun=ce);gain_ratio(f_C[,2:3]);gini(f_C[,2:3]);entropy(f_C[,2:3])
#选择B最为第二个子节点split feature.

#4.7c How many instances are misclassified by the resulting decision tree?
f <- ddply(.data=e7, .variable=c('A','B') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f[,3:4]);gain(f[,3:4], fun=ce);entropy(f[,3:4])

#4.7d Repeat parts (a), (b), and (c) using C as the splitting attribute.
#第一个子节点
f_B <- ddply(.data=e7[e7$C=='T',], .variable=c('B') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_B[,2:3]);gain(f_B[,2:3], fun=ce);gain_ratio(f_B[,2:3]);gini(f_B[,2:3]);entropy(f_B[,2:3])
f_A <- ddply(.data=e7[e7$C=='T',], .variable=c('A') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_A[,2:3]);gain(f_A[,2:3], fun=ce);gain_ratio(f_A[,2:3]);gini(f_A[,2:3]);entropy(f_A[,2:3])
#选择A最为第二个子节点split feature.

#第二个子节点
f_B <- ddply(.data=e7[e7$C=='F',], .variable=c('B') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_B[,2:3]);gain(f_B[,2:3], fun=ce);gain_ratio(f_B[,2:3]);gini(f_B[,2:3]);entropy(f_B[,2:3])
f_A <- ddply(.data=e7[e7$C=='F',], .variable=c('A') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
ce(f_A[,2:3]);gain(f_A[,2:3], fun=ce);gain_ratio(f_A[,2:3]);gini(f_A[,2:3]);entropy(f_A[,2:3])
#选择B最为第二个子节点split feature.

f1 <- ddply(.data=e7[e7$C=='T',], .variable=c('C','A') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
f2 <- ddply(.data=e7[e7$C=='F',], .variable=c('C','B') , .fun=function(x) c(Positive=sum(x['Positive']), Negative=sum(x['Negative'])))
f <- rbind(f1[,3:4], f1[,3:4])
ce(f);gain(f, fun=ce);entropy(f)
#ce=0
#This examples shows  that a greedy heuristic does not always produce an optimal solution again. 

#4.8


#=============================================================================
#Decision Trees with Package party
str(iris)
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

#try
library(caret)
set.seed(1365)
train <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
train.iris <- iris[train==1,]
valid.iris <- iris[train==2,]
gbm.fit.iris <- gbm(Species ~ ., data=train.iris, n.trees=200, verbose=FALSE)
gbm.pred <- predict(gbm.fit.iris, valid.iris, n.trees=200, type="response")
gbm.pred <- as.factor(colnames(gbm.pred)[max.col(gbm.pred)]) ##!
confusionMatrix(gbm.pred, valid.iris$Species)$overall

library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)
# check the prediction
table( trainData$Species, predict(iris_ctree))
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")

# predict on test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

#titanic 
library(party)
set.seed(1234)
titanic <- read.csv('D:\\xujian\\project\\RR\\data\\titanic3.csv')
titanic$survived <- factor(titanic$survived , levels=c('1','0'), labels=c('survived', 'not-survived') )  
ind <- sample(2, nrow(titanic), replace=TRUE, prob=c(0.7, 0.3))
trainData <- titanic[ind==1,]
testData <- titanic[ind==2,]

myFormula <- survived ~ pclass + sex + age + sibsp
titanic_ctree <- ctree(myFormula, data=trainData)
# check the prediction
table( trainData$survived, predict(titanic_ctree))
print(titanic_ctree)
plot(titanic_ctree)
plot(titanic_ctree, type="simple")
testset <- trainData[trainData$sex=='female' & trainData$pclass<=2,'survived']
table(testset)/length(testset)

# predict on test data
testPred <- predict(titanic_ctree, newdata = testData)
table(testPred, testData$survived)

#Decision Trees with Package rpart
data("bodyfat", package = "mboost")
bodyfat[1:5,]
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]

# train a decision tree
library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)
print(bodyfat_rpart$cptable)
print(bodyfat_rpart)
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)

opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)

DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed",ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)

data()
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              parms = list(prior = c(.65,.35), split = "information"))
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              control = rpart.control(cp = 0.05))
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)

#Random Forest
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

library(randomForest)
rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Species)
print(rf)
attributes(rf)
plot(rf)
importance(rf)
varImpPlot(rf)
irisPred <- predict(rf, newdata=testData)
table(irisPred, testData$Species)
plot(margin(rf, testData$Species))

#SLS OPPTY PROD Decision Tree
sourcedata<-read.csv('D:\\xujian\\project\\AA_SLS_OPP\\SCORED_SLS_OPPTY_PROD.csv')
OPPTY <- sourcedata[, c("sls_oppty_src_oppty_sht_unq_id","WON_LOST", 
                        "sls_oppty_entprs_tot_prc_usd_am", "stage_5_negotiate",
                        "sls_oppty_age", "cust_3yr_recent_won_mth",
                        "chnl_ptnr_3yr_recent_won_mth")]
OPPTY$WON_LOST1<-factor(OPPTY$WON_LOST, levels=c(0,1))
set.seed(1234)
ind <- sample(2, nrow(OPPTY), replace=TRUE, prob=c(0.7, 0.3))
trainData <- OPPTY[ind==1,]
testData <- OPPTY[ind==2,]

library(party)
myFormula <- WON_LOST1 ~ chnl_ptnr_3yr_recent_won_mth + cust_3yr_recent_won_mth + sls_oppty_age + sls_oppty_entprs_tot_prc_usd_am + stage_5_negotiate
oppty_ctree <- ctree(myFormula, data=trainData)
# check the prediction
table(predict(oppty_ctree), trainData$WON_LOST1)
print(oppty_ctree)
plot(oppty_ctree)
plot(oppty_ctree, type="simple")

# predict on test data
testPred <- predict(oppty_ctree, newdata = testData)
table(testPred, testData$WON_LOST)

sourcedata<-read.csv('D:\\xujian\\project\\AA_SLS_OPP\\SCORED_SLS_OPPTY_PROD.csv')
OPPTY <- sourcedata[, c("sls_oppty_src_oppty_sht_unq_id","WON_LOST", 
                        "sls_oppty_entprs_tot_prc_usd_am", "stage_5_negotiate",
                        "sls_oppty_age", "cust_3yr_recent_won_mth",
                        "chnl_ptnr_3yr_recent_won_mth")]
library(party)
OPPTY$WON_LOST1<-factor(OPPTY$WON_LOST)
formula <- WON_LOST1 ~ chnl_ptnr_3yr_recent_won_mth + cust_3yr_recent_won_mth + sls_oppty_age + sls_oppty_entprs_tot_prc_usd_am + stage_5_negotiate
dtree <- ctree(formula, data=OPPTY) 
scoring<-as.numeric(predict(dtree))-1
result <- data.frame(ID=OPPTY$sls_oppty_src_oppty_sht_unq_id, classification=scoring, actual_won_lost=OPPTY$WON_LOST)   
table(predict(dtree), OPPTY$WON_LOST1)
print(dtree)
plot(dtree)

#SLS OPPTY PROD Decision Tree
sourcedata<-read.csv('D:\\xujian\\project\\AA_SLS_OPP\\SCORED_SLS_OPPTY_PROD.csv')
OPPTY <- sourcedata[, c("sls_oppty_src_oppty_sht_unq_id","WON_LOST", 
                        'cust_3yr_recent_won_mth',
                        'REP_sls_chnl_rte_cd',
                        'chnl_ptnr_3yr_recent_won_mth',
                        'sls_oppty_age',
                        'cust_1yr_freq_won_cnt',
                        'sls_oppty_entprs_tot_prc_usd_am',
                        'stage_5_negotiate',
                        'chnl_ptnr_1yr_recent_won_mth',
                        'sls_cmpgn_type_cd',
                        'stage_4a_dev_solution',
                        'stage_4b_propose_solution',
                        'REP_cust_amid_lvl_2_seg_cd',
                        'cust_src_seg_nm',
                        'stage_2_validate'
)]
OPPTY$WON_LOST1<-factor(OPPTY$WON_LOST, levels=c(0,1))
set.seed(1234)
ind <- sample(2, nrow(OPPTY), replace=TRUE, prob=c(0.7, 0.3))
trainData <- OPPTY[ind==1,]
testData <- OPPTY[ind==2,]

library(party)
myFormula <- WON_LOST1 ~ cust_3yr_recent_won_mth+REP_sls_chnl_rte_cd+chnl_ptnr_3yr_recent_won_mth+sls_oppty_age+cust_1yr_freq_won_cnt+sls_oppty_entprs_tot_prc_usd_am+stage_5_negotiate+chnl_ptnr_1yr_recent_won_mth
#+sls_cmpgn_type_cd+stage_4a_dev_solution+stage_4b_propose_solution+REP_cust_amid_lvl_2_seg_cd+cust_src_seg_nm+stage_2_validate
oppty_ctree <- ctree(myFormula, data=trainData)
# check the prediction
table(predict(oppty_ctree), trainData$WON_LOST1)
print(oppty_ctree)
plot(oppty_ctree)
plot(oppty_ctree, type="simple")

#====================================================

#浅谈ROC曲线
#http://xccds1977.blogspot.com/2013/01/roc.html
newdata <- subset(iris, Species=='setosa' | Species=='versicolor')
# 做一个logistic回归，生成概率预测值
model1 <- glm(Species~., data=newdata, family='binomial')
pre <- predict(model1,type='response')
# 将预测概率prob和实际结果y放在一个数据框中
data <- data.frame(prob=pre,obs=newdata$y)
# 按预测概率从低到高排序
data <- data[order(data$prob),]
n <- nrow(data)
tpr <- fpr <- rep(0,n)
# 根据不同的临界值threshold来计算TPR和FPR，之后绘制成图
for (i in 1:n) {
  threshold <- data$prob[i]
  tp <- sum(data$prob > threshold & data$obs == 1)
  fp <- sum(data$prob > threshold & data$obs == 0)
  tn <- sum(data$prob < threshold & data$obs == 0)
  fn <- sum(data$prob < threshold & data$obs == 1)
  tpr[i] <- tp/(tp+fn) # 真正率
  fpr[i] <- fp/(tn+fp) # 假正率
}
plot(fpr,tpr,type='l')
abline(a=0,b=1)
#R中也有专门用来绘制ROC曲线的包，例如常见的ROCR包，它不仅可以用来画图，还能计算ROC曲线下面积AUC，以评价分类器的综合性能，该数值取0-1之间，越大越好。
library(ROCR)
pred <- prediction(pre,newdata$y)
performance(pred,'auc')@y.values #AUC值
perf <- performance(pred,'tpr','fpr')
plot(perf)
#ROCR包画图函数功能比较单一，笔者比较偏好使用功能更强大的pROC包。它可以方便比较两个分类器，还能自动标注出最优的临界点，图看起来也比较漂亮。
library(pROC)
modelroc <- roc(newdata$y,pre)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

#====================================================
#用Diabetes数据集，验证决策树各个分类算法
library(lars)


#====================================================
#====================================================
#====================================================
#====================================================
#====================================================
#====================================================
#====================================================
dt <- function(titanic){
  library(party)
  titanic$survived <- factor(titanic$survived , levels=c('1','0'), labels=c('survived', 'not-survived') )  
  formula <- survived ~ pclass + sex + age + sibsp
  dtree <- ctree(formula, data=titanic)
  serial <- serialize(dtree, NULL)
  dtree1 <- unserialize(serial)
  scoring<-predict(dtree1)
  data.frame(id=titanic$id, prediction=scoring, survived=titanic$survived)
}

dtFactory <- function()
{
  list(name=dt, #function that does the processing
       udxtype=c("transform"), #type of the function
       intype=c("int", "int", "int", "varchar", "numeric","int"), #input types
       outtype=c("int","numeric","numeric") #output types
  )
}

testDT <- function()
{
  set.seed(1234)
  titanic <- read.csv('D:\\xujian\\project\\RR\\data\\titanic.csv')

  result <- dt(titanic)
  # check the prediction
  table( result$survived, result$prediction)
  
}
testDT()
x <- serialize(list(1,2,3), NULL)
unserialize(x)

#train.survive <- glm(survived ~ pclass + sex +age +sibsp, family = binomial(logit), data=titanic3)

#The k-means ploymorphic algorithm

# Input: A dataframe with 5 columns
# Output: A dataframe with one column that tells the cluster to which each data
#         point belongs
mykmeansPoly <- function(x,k)
{
  # get the number of clusters to be formed
#   if(!is.null(y[['k']]))
#     k=as.numeric(y[['k']])
#   else
#     stop("Expected parameter k")
  set.seed(1234)
  # get the number of columns in the input data frame
  cols = ncol(x)
  # run the kmeans algorithm
  cl <- kmeans(x[,c('sl', 'sw','pl','pw')], k)
  # get the cluster information from the result of above
  Result <- cl$cluster
  #return result to vertica
  Result <- data.frame(VCol=Result)
  Result
}

# Function that tells vertica the name of the actual R function, and the 
# polymorphic parameters
kmeansFactoryPoly <- function()
{
  list(name=mykmeansPoly,udxtype=c("transform"), intype=c("any"), outtype=c("int"),
       parametertypecallback=kmeansParameters)
}

# call back function to return parameter types
kmeansParameters <- function()
{
  params <- data.frame(datatype=rep(NA, 1), length=rep(NA,1), scale=rep(NA,1),
                       name=rep(NA,1) )
  params[1,1] = "int"
  params[1,4] = "k"
  params
}

testKMeans <- function()
{
  iris1 <- read.csv('D:\\xujian\\project\\RR\\data\\iris.csv', header=FALSE, col.names=c('sl', 'sw', 'pl', 'pw', 'spec' ))  
  result <- mykmeansPoly(iris1, 3)
  iris1$newspec <- result[,1]
  table(iris1$spec, iris1$newspec)
}