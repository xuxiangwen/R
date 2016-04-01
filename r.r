#--------------------------------
#基础设置
#--------------------------------
#设置当前的工作路径
RR_path <- 'D:/xujian/project/RR/code'
RR_path <- '/home/poc'
RR_code_path <- 'D:/xujian/project/RR'
setwd(RR_path)
setwd(RR_code_path)

#设置简体中文作为本地
Sys.setlocale(,"CHS")
Sys.setlocale(locale='Chinese');

#设置代理
Sys.setenv(http_proxy="http://web-proxy.rose.hp.com:8080")
Sys.setenv(https_proxy="https://web-proxy.rose.hp.com:8080")

#保存已安装的R包
old_packages <- installed.packages()[,1]
save(old_packages, file=paste(RR_path, "old_packages.Rdata", sep="/"))
rm(old_packages)

#更新已安装的R包(电脑重新安装时，特别有用，或者安装了R的新版本)
load(paste(RR_path, "old_packages.Rdata", sep="/"))
#remove.packages(c('vcd', 'pastecs'))
new_pacakges <- installed.packages()[,1]
for (package in setdiff(old_packages, new_pacakges)){
  install.packages(package)
  cat('\n--------------------------------------------------------------------')
  cat('\nHave installed Package:',  package)
  cat('\n--------------------------------------------------------------------')
}

#输入脚本
#source("filename")可在当前会话中执行一个脚本

#文本输出
#sink("filename")将输出重定向到文件filename中

#图形输出
# pdf("filename.pdf")  PDF文件
# win.metafile("filename.wmf")  Windows图元文件
# png("filename.png")  PBG文件
# jpeg("filename.jpg")  JPEG文件
# bmp("filename.bmp")  BMP文件
# postscript("filename.ps")  PostScript文件
#dev.off()  将输出返回到终端

traceback()
options(error = recover)

#--------------------------------
#R的内存管理
#--------------------------------
obj1 <- matrix(0, nrow=4000, ncol=4000)
obj2 <- matrix(0, nrow=4000, ncol=4000)
obj3 <- matrix(0, nrow=4000, ncol=4000)
obj4 <- matrix(0, nrow=4000, ncol=4000)
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存
#remvoe all objects 删除所有对象
rm(list = ls())
gc(T)
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存

obj1 <- matrix(0, nrow=4000, ncol=4000)
object.size(obj1);
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存
storage.mode(obj1) <-  "integer"
object.size(obj1)
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存
gc(T)
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存
gc(T,T)
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存

#帮助我们方便地查看当前的内存占用情况
##  http://adv-r.had.co.nz/memory.html#garbarge-collection
mem <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }  
  node_size <- if (bit == 32L) 28L else 56L  
  usage <- gc()
  sum(usage[, 1] * c(node_size, 8)) / (1024 ^ 2)
}

## http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
# 显示各个对象占用的内存
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
lsos()

#对同一个变量反复赋值,其实占用的内存非常大
memory.size()
system.time(for(i in 1:1e7) x = i^2)
memory.size()
invisible(gc())
memory.size()
lsos()

#当一个函数调用结束后，该函数运行过程中产生的变量, 在函数运行结束后，内存是否会自动消除释放
#Yes and no, depending on whether the evaluation frame is still reachable 
#after the call. 
#Note that it is not true that anything created during a call 
#will not be used anymore after the end of the call. For example,
rm(list=ls()) #删除所有对象
f<-function(n) {
  dat.in.f=runif(n)
  func.in.f=function(y) mean(dat.in.f)+y
  func.in.f
}
g<-f(10L)
g(5)
ls(envir=environment(g))
#Here, anything created in the evaluation frame (e.g., dat.in.f) 
#when calling f cannot be released, 
#because they might be used when calling g. Such spaces will not be gced.

rm(list=ls())
dat=runif(1e4L)
Fn=ecdf(dat)
rm(dat); gc()
ls(envir=environment(Fn))
str(get('x', envir=environment(Fn)))
#You can see here that even if the original data have been removed, 
#there still exists a local copy of it in the evaluation frame 
#during the call to ecdf.

#--------------------------------
#Factor因子的应用
#--------------------------------
status <- c('Poor', 'Imporved', 'Excellent', 'Poor')
status1 <- as.factor(status);status1
status2 <- factor(status);status2
status3 <- factor(status, order=T);status3
status4 <- factor(status, order=T, levels=c('Poor','Imporved','Excellent'));status4
levels(status4) #得到Levels名称
level_Values <- 1:length(levels(status4))  #得到Levels对应的数值
names(level_Values) <- levels(status4);   level_Values                     
labels(status4) #得到数据标签
nlevels(status4)  #得到Leveles的个数
summary(status4)  #得到各个Leveles的item个数
as.numeric(status4)
as.integer(status4)
status4[1]>status4[2]  #Facotr 可以直接比较大小
status4[3]>status4[4]

as.integer(status3)
as.integer(status2)

class(status4)
class(status3)

#小技巧：如果你不想跑那一段代码，可以放到if(FALSE)，解决R没有多行注释的问题
if(FALSE) {
  status <- c('Poor', 'Imporved', 'Excellent', 'Poor')
  status1 <- as.factor(status);status1
  status2 <- factor(status);status2
  status3 <- factor(status, order=T);status3
  status4 <- factor(status, order=T, levels=c('Poor','Imporved','Excellent'));status4  
}

#判断第一个向量中的内容是否在第二个向量中出现过
names(iris) %in% c('Sepal.Length', 'Petal.Width')
iris$Species %in% c('setosa', 'virginica')

#--------------------------------
# 数据变形处理（行转列，列转行）
#--------------------------------
#Reshape的一些应用
library(reshape)

mydata <- scan()
1 1 5  6 
1 2 3  5 
2 1 6  1 
2 2 2  4 


mydata <- data.frame(matrix(mydata, nrow=4, byrow=T))
#, colClasses(mydata) < c('character', 'character', 'numeric', 'numeric') 
colnames(mydata) <- c('id', 'time', 'X1', 'X2') 
mydata

#行转列
md <- melt(mydata, id=c('id', 'time'));md

#列转行
cast(md, id+time~variable)
cast(md, id+variable~time)
cast(md, id~variable+time)

#列转行，并聚合
cast(md, id~variable, mean)
cast(md, time~variable, mean)
cast(md, id~time, mean)

#把array变成data frame
datax <- array(1:8, dim=c(2,2,2)) 
melt(datax) 
melt(datax, varnames=LETTERS[24:26],value.name="Val")

#把list变成data frame
datax <- list(agi="AT1G10000", GO=c("GO:1010","GO:2020"), KEGG=c("0100", "0200", "0300"))
melt(datax)



#--------------------------------
# plyr
#--------------------------------
data(tips,package='reshape2')
library(plyr)
head(tips)
aggregate(x=tips$tip, by=list(tips$sex), FUN=mean)
aggregate(tip~sex, tips, mean)
ddply(.data=tips, .variable='sex', .fun=function(x) {mean(x$tip)})

ratio_fun <- function(x){
  sum(x$tip)/sum(x$total_bill)
}

ddply(tips, .(sex), ratio_fun)

data <- as.matrix(iris[,-5])
result4 <- adply(.data=data, 
                 .margins=2,
                 .fun=function(x){
                   max <- max(x)
                   min <- min(x)
                   median <- median(x)
                   sd <- round(sd(x), 2)
                   return(c(max, min, median, sd))
                 })
result4
apply(data, 
      2,
      function(x){
        max <- max(x)
        min <- min(x)
        median <- median(x)
        sd <- round(sd(x), 2)
        return(c(max, min, median, sd))
      })
summary(iris)

model <- function(x){
  lm(Sepal.Length~Sepal.Width, data=x)
}

models <- dlply(.data=iris,
                .variable='Species',
                .fun=model);models
result5 <- ldply(.data=models, .fun=coef);result5

x <- rnorm(10)
each(max, min, median, sd)(x)
colwise(mean, is.numeric)(iris)

#dplyr
library(dplyr)
set.seed(1)
mydf <- data.frame(matrix(abs(round(rnorm(200), 1)), nrow = 20))
head(mydf)
mydf %>% filter(X1 %in% c(1.5, 0.2))
mydf %>% filter(X2 %in% c(1.5, 0.2))
mydf %>% filter(X1 %in% c(1.5, 0.2) | X2 %in% c(1.5, 0.2) )
mydf %>% filter(X1 > X2)

library(nycflights13)
dim(flights)
head(flights)
a <- filter(flights, month == 1, day == 1); dim(a)
a <- filter(flights, month == 1 & day == 1); dim(a)
a <- filter(flights, month == 1 | day == 1); dim(a)
a <- filter(flights, month == 1 ); dim(a)
a <- filter(flights, month %in% c(1,2)) ; dim(a)

slice(flights, 1:10) #数据切片
arrange(flights, desc(arr_delay))
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, tail_num = tailnum)
rename(flights, tail_num = tailnum)
distinct(select(flights, tailnum))
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60) #dplyr::mutate() works the same way as plyr::mutate() and similarly to base::transform()
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)  #The key difference between mutate() and transform() is that mutate allows you to refer to columns that you just created:
)

#group
planes <- group_by(flights, tailnum)
delay <- summarise(planes,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

filter(select(flights, distance, dep_delay), dep_delay>60)
a <- flights %>%
  select(distance, dep_delay)%>%
  filter(dep_delay>60)


#dplyr 


#整数相除
29%/%2

#求余
29%%2

#连乘
prod(1:5)

#累加
cumsum(1:5)

#等差数列
2.1:6

# NA. NaN， Inf， Null的区别
# 从网上找了一下，整理如下：
# NA：缺失数据
# NaN：无意义的数，比如sqrt(-2)， 0/0。
# Inf：正无穷大
# -Inf：负无穷大
# NULL：不存在
x<-c(0/1, 0/0, 1/0, NA, NULL)
x
# [1]   0 NaN Inf  NA      #NULL意味着不存在
is.na(x)                   #看上去NaN是Na的一个子集
# [1] FALSE  TRUE FALSE  TRUE
is.nan(x)
# [1] FALSE  TRUE FALSE FALSE
is.finite(x)              #NA， NaN既不属于finite, 也不属于infinite
# [1]  TRUE FALSE FALSE FALSE
is.infinite(x)
# [1] FALSE FALSE  TRUE FALSE
is.null(NULL)

#trim in mean function
# 算术平均数有一个缺点就是对outlier非常敏感，
# 如果少数几个值非常大，那么整个算术平均数也随之增加。
# 因此，为了避免这种情况，可以使用trimmed mean——也就是说，
# 先对最大和最小的数剔除了（一般会按照百分比剔除，
# 比如剔除最大和最小的a％的个案剔除出去），
# 然后再计算算术平均值。
# 另外trim的参数值最大只是0.5
x <- c(0:10, 50:51);x
xm <- mean(x)
c(xm, mean(x, trim = 0.10), mean(x[c(-1, -13)]))
c(xm, mean(x, trim = 0.20), mean(x[c(-1,-2, -12,-13)]))

#异常值
library(DMwR)
iris2 <- iris[,1:4]
outlier.scores <- lofactor(iris2, k=5)
plot(outlier.scores)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]
print(outliers)

n <- nrow(iris2)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(iris2), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(iris2, pch=pch, col=col)

library(Rlof)
data(iris)
df<-data.frame(iris[-5])
df.lof<-lof(df,c(5:10))

#http://statistical-research.com/outlier-detection/
library(DMwR);
library(outliers);
set.seed(1234)
gen.xyz <- function(n, mean, sd) {
  cbind(rnorm(n, mean[1], sd[1]),
        rnorm(n, mean[2],sd[2]),
        rnorm(n, mean[3],sd[3])
  );
}
xyz <- rbind(gen.xyz(150, c(0,0,0), c(.2,.2,.2)),
             gen.xyz(150, c(2.5,0,1), c(.4,.2,.6)),
             gen.xyz(150, c(1.25,.5, .1), c(.3,.2, .5)));
xyz[1,] <- c(0,2,1.5);
km.3 <- kmeans(xyz, 3);
outlier.scores <- lofactor(xyz, k=5)
plot(density(outlier.scores));
outliers <- order(outlier.scores, decreasing=T)[1:5]
print(outliers);
grubbs.test(xyz[,1], type = 10, opposite = FALSE, two.sided = FALSE)
grubbs.test(xyz[,2], type = 10, opposite = FALSE, two.sided = FALSE)
grubbs.test(xyz[,3], type = 10, opposite = FALSE, two.sided = FALSE)
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(xyz, pch=pch, col=col)
my.cols = km.3$cluster;
plot(xyz[,c(1,2)], col=my.cols);
plot(xyz[,c(1,3)], col=my.cols);
plot(xyz[,c(2,3)], col=my.cols);


#异常检测
library(outliers)
set.seed(1234)
x = rnorm(10)
dixon.test(x)
dixon.test(x,opposite=TRUE)
dixon.test(x,type=10)

set.seed(1234)
x = rnorm(10)
grubbs.test(x)
grubbs.test(x,type=20)
grubbs.test(x,type=11)

set.seed(1234)
y=rnorm(100)
outlier(y)
outlier(y,opposite=TRUE)
dim(y) <- c(20,5)
outlier(y)
outlier(y,opposite=TRUE)

set.seed(1234)
y=rnorm(100)
outlier(y)
outlier(y,opposite=TRUE)
rm.outlier(y)
rm.outlier(y,opposite=TRUE)
dim(y) <- c(20,5)
outlier(y)
outlier(y,logical=TRUE)
outlier(y,logical=TRUE,opposite=TRUE)
rm.outlier(y)
rm.outlier(y,opposite=TRUE)

set.seed(1234)
x=rnorm(100)
d=data.frame(x=x,group=rep(1:10,10))
cochran.test(x~group,d)
cochran.test(x~group,d,inlying=TRUE)
x=runif(5)
cochran.test(x,rep(5,5))
cochran.test(x,rep(100,5))

#时间序列异常值outlier
library(tsoutliers)
data("hicp")
y <- log(hicp[["011600"]])
fit <- arima(y, order = c(1, 1, 0))
resid <- residuals(fit)
pars <- coefs2poly(fit)
outliers <- locate.outliers(resid, pars,, cval = 3.5, types = c("AO", "LS", "TC"), 
                            delta = 0.7, n.start = 50)
outliers
tso(y = log(hicp[[1]]))
tso(y = d)

resNile2 <- tso(y = ts(d, frequency=1), types = c("AO", "LS", "TC"),
                 maxit = 1, remove.method = "bottom-up", tsmethod = "auto.arima",
                args.tsmethod = list(allowdrift = FALSE, ic = "bic"))

#字符串连接
paste('xu', 'jian')
paste('x', 1:6, sep='')
paste(1:10) #same as as.character(1:10)

#对象属性
mode(c(1,3,5)>5)
length(c(1,3,5)>5)
x<-c(apple=2.5, orange=2.1);x
attributes(x)  #对象的各个特殊属性组成的列表，但不包括mode和length
attr(x, 'names') #对象的具体某个属性
attr(x, 'type') <- 'fruit';x
attr(x, 'type')

##tapply（进行分组统计）
sex <- c('m', 'f', 'm', 'm', 'f')
height <- c(174, 165, 180, 171, 160)
tapply(height, sex, mean)

require(stats)
groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
tapply(groups, groups, length) #- is almost the same as
table(groups)

## contingency table from data.frame : array with named dimnames
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
tapply(warpbreaks$breaks, warpbreaks[, 3, drop = F], sum)

n <- 17; fac <- factor(rep(1:3, length = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
tapply(1:n, fac, sum, simplify = FALSE)
tapply(1:n, fac, range)
tapply(1:n, fac, quantile)

###################################################
#R数据分析当中的化整为零（Split-Apply-Combine）策略
#http://pgfe.umassmed.edu/ou/archives/2564
###################################################
library(plyr)
library(MASS)
month <- ordered(rep(1:12, length=72))
one <- ozone[1,1,]
model <- rlm(one ~ month - 1); model
deseas <- resid(model)

deseasf <- function(value) rlm(value ~ month -1) #the function
models <- as.list(rep(NA, 24*24)) #prepare the variable
dim(models) <- c(24, 24)
deseas <- array(NA, c(24,24,72)) #prepare the variable
dimnames(deseas) <- dimnames(ozone)
for (i in seq_len(24)) { #for loop for first dimension
  for(j in seq_len(24)) { #for loop for second dimension
    mod <- deseasf(ozone[i, j, ]) #apply function
    models[[i, j]] <- mod #save data
    deseas[i, j, ] <- resid(mod) #get residure
  }
}

#apply(array, margin, function, …)
#处理对象是是矩阵array或者matrix
x<-cbind(x1=3,x2=c(4:1,2:5))
dimnames(x)[[1]]<-letters[1:8]
x
apply(x,2,mean) 
col.sums <- apply(x, 2, sum);col.sums
colSums(x)
row.sums <- apply(x, 1, sum);colSums(x)
rowSums(x)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

sum.plus.y <- function(x,y){
  sum(x) + y
}
apply(x, 1, sum.plus.y, 3) #使用自定义函数,其中最后一个参数是y的值
apply(x, 1, sum.plus.y, y=3)

#理解了apply,就可比较容易地理解lapply, sapply, vapply了。
#这三者针对的对象是list或者Vector。
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
x
lapply(x,mean)
lapply(x, quantile)
sapply(x,mean)
sapply(x, quantile, simplify=FALSE, use.names=FALSE)
sapply(x, quantile)

#多维
x<-cbind(x1=3,x2=c(4:1,2:5))
dimnames(x)[[1]]<-letters[1:8]
x
x<-as.data.frame(x);x
as.list(x)
lapply(x,mean)
sapply(x,mean)
vapply(x,mean,1)

# 从它们的说明文件我们知道，无论你传入的x是什么，
# 它首先做的一步说是使用as.list来将其转换成一个一维的list。
# 所以，一个data.frame传入lapply之后，
# 它的colnames将会转换成list的names,而rownames可能会丢失。
# 比较可知，lapply和sapply的差别在于，
# lapply的返回值是一个list，而sapply的返回值是一个矩阵。
# sapply的返回值其实就是在lapply的基础上
# 再使用了simplify2array(x, higher=TRUE)函数，
# 使用其结果变成一个array。
i39 <- sapply(3:9, seq);i39
sapply(i39, fivenum)
vapply(i39, fivenum, 1:5+0.1)
vapply(i39, fivenum, c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))


# tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)
# mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
# sweep(x, MARGIN, STATS, FUN="-", check.margin=TRUE, ...)
models <- apply(ozone, 1:2, deseasf) #这里相当于for loop当中的for(i in seq_len(24)){for(j in seq_len(24)){mod<-deseasf(ozone[i,j,]); models[[i,j]]<-mod;}}, 但是运算却是并行处理的。
resids_list <- lapply(models, resid)
resids <- unlist(resids_list)
dim(resids) <- c(72, 24, 24)
deseas <- aperm(resids, c(2, 3, 1))
dimnames(deseas) <- dimnames(ozone)

ozonedf <- as.data.frame(ozone)
deseasf_df <- function(df) {
  rlm(value ~ month - 1, data = df)
}
pieces <- split(ozone, list(ozonedf$lat, ozonedf$long))
models <- lapply(pieces, deseasf_df)
results <- mapply(function(model, df) {
  cbind(df[rep(1, 72), c("lat", "long")], resid(model))
}, models, pieces)
deseasdf <- do.call("rbind", results)

p <- c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)
x<-1:10
sapply(1:5,function(i) sample(x,4,prob=p))
#apply（对一个数组按行或者按列进行计算）
apply(t(1:5),2,function(i) sample(x,4,prob=p))

#split
require(stats); require(graphics)
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)
### Calculate 'z-scores' by group (standardize to mean zero, variance one)
z <- unsplit(lapply(split(x, g), scale), g)

# or
zz <- x
split(zz, g) <- lapply(split(x, g), scale)

# and check that the within-group std dev is indeed one
tapply(z, g, sd)
tapply(zz, g, sd)

### data frame variation
## Notice that assignment form is not used since a variable is being added
g <- airquality$Month
l <- split(airquality, g)
l <- lapply(l, transform, Oz.Z = scale(Ozone))
aq2 <- unsplit(l, g)
head(aq2)
with(aq2, tapply(Oz.Z,  Month, sd, na.rm = TRUE))

### Split a matrix into a list by columns
ma <- cbind(x = 1:10, y = (-4:5)^2)
split(ma, col(ma))
split(1:10, 1:2)

#table（求因子出现的频数）
d <- factor(rep(c("A","B","C"), 10), levels=c("A","B","C","D","E"))
table(d)
table(d, exclude="B")

#产生因子
gl(3, 5)
gl(3, 1, 15)

#Matrix and Array
a<-matrix(1:24, nrow=4); a
is.matrix(a); is.array(a)
a1<-array(1:24, dim=c(4,6)); a1
is.matrix(a1); is.array(a1)
a2<-array(1:24, dim=c(4,3,2)); a2
is.matrix(a2); is.array(a2)

#方阵的行列式
det(matrix(1:4, ncol=2))

#内积
x <- c(1, 2); y <- c(3, 4)
x %*% y
t(x) %*% y
crossprod(x, y)
#x与x做内积
crossprod(x)

#外积（叉积）
tcrossprod(x, y)
x %*% t(y)
x %o% y
outer(x, y)
#x与x做外积（叉积）
tcrossprod(x)

#生成对角阵
v <- c(1,4,5)
diag(v)
v <- matrix(1:9, ncol=3)
diag(v)
diag(diag(v))

#解线性方程组
A <- t(array(c(1:5, c(8, 6, 10, 9)), dim=c(3,3)))
b <- c(1, 1, 1)
x <- solve(A, b);x
#逆矩阵
B <- solve(A); B
B%*%b     



#数组的维名字
X <- matrix(1:6, ncol=2,
            dimnames=list(c("one","two","three"), c("First","Second")),
            byrow=T); X
X<-matrix(1:6, ncol=2, byrow=T)
dimnames(X) <- list(
  c("one", "two", "three"), c("First", "Second"))

X<-matrix(1:6, ncol=2, byrow=T)
X<-matrix(1:6, ncol=2, byrow=T)
colnames(X) <- c("First", "Second")
rownames(X) <- c("one", "two", "three")

#数组的广义转置
A<-array(1:24, dim = c(2,3,4))
B<-aperm(A, c(2,3,1))

A<-matrix(1:24, nrow = 6)
B<-aperm(A, c(2, 1));B
t(A)


#读写数据文件
#read.table
setwd('C:/xujian/project/AA/code')
rt <- read.table("houses.data", header=T);rt
rt1 <- read.table("houses1.data", header=T);rt1

#scan
w <- scan("weight.data");
w <- scan()
172.4 75.0 169.3 54.8 169.3 64.0 171.4 64.8 166.5 47.4
171.4 62.2 168.2 66.9 165.1 52.0 168.8 62.2 167.8 65.0
165.8 62.2 167.8 65.0 164.4 58.7 169.9 57.5 164.9 63.0

w
inp <- scan("weight.data", list(height=0, weight=0))
inp

#写数据文件
df <- data.frame(
  Name=c("Alice", "Becka", "James", "Jeffrey", "John"),
  Sex=c("F", "F", "M", "M", "M"),
  Age=c(13, 13, 12, 13, 12),
  Height=c(56.5, 65.3, 57.3, 62.5, 59.0),
  Weight=c(84.0, 98.0, 83.0, 84.0, 99.5)
)
?
write.csv(df, file="foo.csv", quote = F, row.names = F)

#链接嵌入的数据库
data()
data(infert)
data(package="plyr")
data(ozone, package="plyr")

#if
# if ( cond_1 )
#   statement_1
# else if ( cond_2 )
#   statement_2
# else if ( cond_3 )
#   statement_3
# else
#   statement_4
x <- exp(2)
if( any(x <= 0) ) y <- log(1+x) else y <- log(x)
y

#switch
x<-3
switch(x, 2+2, mean(1:10), rnorm(4))
y <- "fruit"
switch(y,fruit="banana",vegetable="broccoli",meat="beef")

#for
n<-4; x<-array(0, dim=c(n,n));x
for (i in 1:n){
  for (j in 1:n){
    x[i,j]<-1/(i+j-1)
  }
}
x

#while
f<-1; f[2]<-1; i<-1
while (f[i]+f[i+1]<1000) {
  f[i+2]<-f[i]+f[i+1]
  i<-i+1;
}
f

#repeat
f<-1; f[2]<-1; i<-1
repeat {
  f[i+2]<-f[i]+f[i+1]
  i<-i+1
  if (f[i]+f[i+1]>=1000) break
}
f

#二分法求解线性方程根y=x^3-x-1
fun <- function(x){
  x^3-x-1
}

fun1 <- function(x){
  3^x
}

bisection <- function(x1, x2, f, precision=1e-6){
  i <- 0;
  repeat{
    i <- i + 1
    if (f(x1)*f(x2)>0) return(NA) 
    x <- (x1+x2)/2
    y <- f(x)
    if (y1*y<=0) x2 <- x else x1 <- x   
    if (abs(x1-x2)<=precision) return(list(x=(x1+x2)/2, y=f((x1+x2)/2), iter=i)) 
  }
}


#书上的二分法
fzero <- function(f, a, b, eps=1e-6){
  if (f(a)*f(b)>0)
    list(fail="finding root is fail!")
  else{
    repeat {
      if (abs(b-a)<eps) break
      x <- (a+b)/2
      if (f(a)*f(x)<0) b<-x else a<-x
    }
    list(root=(a+b)/2, fun=f(x))
  }
}

#R提供的求解一元方程根的函数（求根）
# uniroot(f, interval,
#         lower = min(interval), upper = max(interval),
#         tol = .Machine$double.eps^0.25, maxiter = 1000, ...)

bisection(1, 2, fun)
fzero(fun, 1, 2, 1e-6)
uniroot(fun, c(1,2))

#切线法
fun <- function(x){
  x^3-x-1
}

fun1 <- function(x){
  3^x
}

tangent <- function(x0, f, f1, precision=1e-6){
  i <- 0
  repeat{
    i <- i+1
    x <- x0 - f(x0)/f1(x0)
    if (abs(x-x0)<=precision) return(list(x=x, y=f(x), iter=i))  
    x0 <- x
  }
}
tangent(1, fun, fun1)
tangent(2, fun, fun1)

#求导
fun<-expression(15*(log(x)/exp(x)))
D(fun,"x")

x=2;y=3
eval(D(expression(x^2+y^2+x*y),"x"))

#定义新的二元运算: %anything%
"%!%" <- function(x, y) {exp(-0.5*(x-y) %*% (x-y))}
3%!%5

#打印出一个函数的图形
fun <- function(x){
  x^2-54/x
}
curve(f, -5, -1, 1000)

fun <- function(x){
  (exp(1/x)-1)/(exp(1/x)+1)
}
curve(fun, -1.5, 1.5, 10000)

fun <- function(x){
  exp(-x)/(exp(-x)+1)
}
curve(fun, -20, 20, 10000)

fun1 <- function(x){
  2*x+54/x^2
}

#牛顿法求解一元函数极值
uniNewton <- function(x0, f, f1, rate=0.1, precision=1e-6){
  i <- 0
  y <- 0
  repeat{
    i <- i+1
    y0 <- f(x0)
    x <- x0 - rate*abs(y0-y)/f1(x0)
    y <- f(x)   
    if (abs(x-x0)<=precision) return(list(x=x, y=f(x), iter=i))  
    x0 <- x
  }
}
uniNewton(-5, fun, fun1, 0.1)
#x0 < -5
x <- x0 - rate*abs(f(x0))/f1(x0)
x0;x 
x0 <- x   


#牛顿(Newton)法求解方程的根
Newtons<-function (fun, x, ep=1e-5, it_max=100){
  index<-0; k<-1
  while (k<=it_max){
    x1 <- x; obj <- fun(x);
    x <- x - solve(obj$J, obj$f);
    norm <- sqrt((x-x1) %*% (x-x1))
    if (norm<ep){
      index<-1; break
    }
    k<-k+1
  }
  obj <- fun(x);
  list(root=x, it=k, index=index, FunVal= obj$f)
}

funs<-function(x){
  f<-c(x[1]^2+x[2]^2-5, (x[1]+1)*x[2]-(3*x[1]+1))
  J<-matrix(c(2*x[1], 2*x[2], x[2]-3, x[1]+1),
            nrow=2, byrow=T)
  list(f=f, J=J)
}
Newtons(funs, c(0,1))

#递归函数
f <- function(x) 1/x
curve(f, 1, 5, 500)

area <- function(f, a, b, eps = 1.0e-06, lim = 10) {
  fun1 <- function(f, a, b, fa, fb, a0, eps, lim, fun) {
    d <- (a + b)/2; h <- (b - a)/4; fd <- f(d)
    a1 <- h * (fa + fd); a2 <- h * (fd + fb)
    if(abs(a0 - a1 - a2) < eps || lim == 0)
      return(a1 + a2)
    else {
      return(fun(f, a, d, fa, fd, a1, eps, lim - 1, fun)
             + fun(f, d, b, fd, fb, a2, eps, lim - 1, fun))
    }
  }
  fa <- f(a); fb <- f(b); a0 <- ((fa + fb) * (b - a))/2
  fun1(f, a, b, fa, fb, a0, eps, lim, fun1)
}
quad<-area(f,1,5); quad

#Hilbert矩阵
hil <-function(n){
  h <- matrix(rep(0, n^2), nrow=n)
  for(i in 1:n){
    for(j in 1:n){
      h[i,j] <- 1/(i+j-1)
    }
  }
  h
}

a <- hil(5)

#行列式
det(hil(5))  
#逆矩阵
solve(hil(5))
solve(hil(5)) %*% hil(5)
#特征值和特征向量
eigen(hil(5))

#描述性统计分析包
summary(iris)   #五分位数+平均值
fivenum(iris$Sepal.Length)   #五分位数(minimum, lower-hinge, median, upper-hinge, maximum)

library(Hmisc)
help(package='Hmisc')   #查看包的帮助
describe(iris)  #显示各个变量的数量，缺失值数量，不重复值，平均值，七分位数

library(pastecs)
help(package='pastecs') 
#计算中位数、平均数、平均数的标准误、平均数置信度为95%的置信区间、方差、标准差以及变异系数。
stat.desc(iris) 
stat.desc(iris， norm=T) #还显示偏度和峰度，正态检验

library(psych)
help(package='psych') 
#非缺失值的数量、平均数、标准差、中位数、截尾均值、绝对中位差、最小值、最大值、
#值域、偏度、峰度和平均值的标准误
describe(iris)
Hmisc::describe(iris)
detach("package:psych", unload=TRUE);describe(iris)  #因为describe函数重名，所以需要卸载Hmisc包 或者 

#常用统计量
data_outline <- function(x){
  n <- length(x)                    #长度
  Max <- max(x)                     #Max
  Min <- min(x)                     #Min
  m <- mean(x)                      #样本均值
  v <- var(x)                       #样本方差
  s <- sd(x)                        #样本标准方差 
  me <- median(x)                   #中位数
  cv <- 100*s/m                     #变异系数
  css <- sum((x-m)^2)               #样本校正平方和
  uss <- sum(x^2)                   #样本未校正平方和
  R <- max(x)-min(x)                #样本极差
  R1 <- quantile(x,3/4)-quantile(x,1/4)    #样本四分差（半极差）
  sm <- s/sqrt(n)                          #样本标准误
  u3  <- sum((x-m)^3)/n                    #样本三阶中心距
  g1 <- n^2/((n-1)*(n-2))*u3/s^3           #偏度系数
  u4 <- sum((x-m)^4)/n                     #样本四阶中心距
  g2 <- ((n^2*(n+1))/((n-1)*(n-2)*(n-3))*u4/s^4 - (3*(n-1)^2)/((n-2)*(n-3)))     #峰度系数
  qd <- numeric(0)
  for(i in 1:9){
    qd[i]<- quantile(x, 1/8*(i-1))
    names(qd)[i]<- paste(100/8*(i-1), '%', sep='')
  }
  list(
    information = data.frame(N=n, Min=Min, Max=Max, Mean=m, Var=v, std_dev=s,
                             Median=me, 
                             std_mean=sm, CV=cv, CSS=css, USS=uss,
                             R=R, R1=R1, Skewness=g1, Kurtosis=g2,  row.names=1),
    quantile.distribution = qd
  )
} 
data_outline(1:10)

#频数和列联表Frequency and contingency tables
library(vcd)
library(psych)
head(Arthritis)
describe(Arthritis)

#ONE-WAY TABLES 一维列联表
mytable <- table(Arthritis$Improved);mytable
prop.table(mytable)

#TWO-WAY TABLES 二维列联表
mytable <- xtabs(~ Treatment+Improved, data=Arthritis);mytable

margin.table(mytable, 1)
margin.table(mytable, 2)

prop.table(mytable, 1)
prop.table(mytable, 2)

addmargins(mytable)
addmargins(prop.table(mytable))
addmargins(prop.table(mytable,1),2)
addmargins(prop.table(mytable,2),1)

#把二维列联表转化成平面结构
table2flat <- function(mytable){
  df <- as.data.frame(mytable)
  rows <- dim(df)[1]
  cols <- dim(df)[2]
  x <- NULL
  for(i in 1:rows){
    for(j in 1:df$Freq[i]){
      row <- df[i, 1:(cols-1)]
      x <- rbind(x, row)
    }
  }
  row.names(x)<- 1:dim(x)[1]
  x
}
table2flat(mytable)
#更加简洁的方法把二维列联表转化成平面结构
row2flat <- function(x) {
  len <- length(x)
  row <- as.numeric(x[len])
  rep(x[1:(len-1)],row)
}
table2flat1 <- function(mytable){
  df <- as.data.frame(mytable)
  f <- unlist(apply(df, 1, row2flat))
  df1 <- as.data.frame(matrix(f, ncol=dim(df)[2]-1, byrow=T))
  colnames(df1) <- colnames(df)[1:(dim(df)[2]-1)]
  df1
}
#tt <- apply(as.data.frame(mytable), 1, function(x) x);tt
table2flat1(mytable)

#另外一种创建列联表的方法CrossTable() function produces two-way table
#SAS风格
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

#多维列联表
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable 
margin.table(mytable, c(1, 3))
ftable(mytable)
ftable(addmargins(prop.table(mytable, c(1, 2)), 3))

#均值，方差，协方差，相关系数
ore<-data.frame(
  x=c(67, 54, 72, 64, 39, 22, 58, 43, 46, 34),
  y=c(24, 15, 23, 19, 16, 11, 20, 16, 17, 13)
)
ore.m<-colMeans(ore);
ore.s<-cov(ore); ore.r<-cor(ore)


plot(ore, xlim=c(0, 80), ylim=c(0, 35));
text(ore$x+1, ore$y, rownames(ore))
text(mean(ore$x), mean(ore$y), 'o', col='green')
lines(c(0, ore[1,1]), c(0, ore[1,2]), col='blue')
lines(c(0, ore[2,1]), c(0, ore[2,2]), col='blue')
lines(c(mean(ore$x), ore[1,1]), c(mean(ore$y), ore[1,2]), col='green')
lines(c(mean(ore$x), ore[2,1]), c(mean(ore$y), ore[2,2]), col='green')

cor(ore_m);cov(ore_m);
ore_m <- as.matrix(ore);

#皮尔逊相关系数，其实基本上是中心化后的夹角余铉
#得到余铉夹角
mycor <- function(m, center=T) {
  m1 <- m
  if (center==T) m1 <- sweep(m1, 2, colMeans(m1))
  #m.len <- apply(m, 1, function(x) sqrt(sum(x^2)))
  #(m)%*%t(m)/(m.len %*% t(m.len))
  m1 <- apply(m1, 2, function(x) x/sqrt(sum(x^2)))
  t(m1)%*%(m1)
}

mycov <- function(m, center=T) {
  if (center==T) m <- sweep(m, 2, colMeans(m)) 
  (m)%*%t(m)
}

mycor1 <- function(m, center=T) {
  y <- scale(m, center = center, scale = T)/sqrt(nrow(m)-1)
  t(y) %*% (y)
}

mycor2 <- function(m, center=T) {
  y <- scale(m, center = center, scale = F)
  y <- apply(y, 2, function(x) x/sqrt(sum(x^2)))
  t(y) %*% (y)
}

mycov1 <- function(m, center=T) {
  y <- scale(m, center = center, scale = F)
  t(y) %*% (y)
}


#下面有三个点正立方体的三个顶点
#用这个例子来显示
m <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow=3, byrow=T)
cov(t(m));cor(t(m))
mycov(m, T);mycor(m, T);

#夹角余弦
mycor1(m)
mycor(m, F);
cor(t(m))

#平面上的三个点
m <- matrix(c(sqrt(3), 1, sqrt(3), 3, 0, 2), nrow=3, byrow=T)
plot(m, xlim=c(0, 3), ylim=c(0, 3));
text(m[,1]+0.2, m[,2])
text(colMeans(m)[1], colMeans(m)[2], 'o', col='green')

mycor(t(m), F);mycor1(t(m), F);mycor2(t(m), F);cor(t(m))
mycor(t(m), T);mycor1(t(m), T);mycor2(t(m), T);cor(t(m))

mycor(m, F);mycor1(m, F);mycor2(m, F);cor(m)
mycor(m, T);mycor1(m, T);mycor2(m, T);cor(m)

cov(t(m));cor(t(m))
mycov(m, T);mycor(m, T);
mycov1(m, T);mycor1(m, T);

cov(m);cor(m)
mycov(t(m), T);mycor(t(m), T);
mycov1(t(m), T);mycor1(t(m), T);

#夹角余弦
mycov1(m, F);mycor1(m, F)
mycov(m, F);mycor(m, F)


#相关系数的区间估计
#Ruben法
ruben.test <- function(n, r, alpha=0.05){
  #r：相关系数
  u <- qnorm(1-alpha/2)
  r_star <- r/sqrt(1-r^2)
  a <- 2*n-3-u^2
  b <- r_star*sqrt((2*n-3)*(2*n-5))
  c <- (2*n-5-u^2)*r_star^2-2*u^2
  y1 <- (b-sqrt(b^2-a*c))/a
  y2 <- (b+sqrt(b^2-a*c))/a
  data.frame(n = n, r = r, conf = 1-alpha,
             L = y1/sqrt(1+y1^2), U = y2/sqrt(1+y2^2))
}

ruben.test(6, 0.8)
ruben.test(25, 0.7)
ruben.test(50, 0.7)
ruben.test(100, 0.7)
ruben.test(2000, 0.7)

#相关系数检验
ore<-data.frame(
  x=c(67, 54, 72, 64, 39, 22, 58, 43, 46, 34),
  y=c(24, 15, 23, 19, 16, 11, 20, 16, 17, 13)
)
cor(ore)
cor.test(ore$x, ore$y)
cor.test(ore$x, ore$y, method="spearman")
cor.test(ore$x, ore$y, method="kendall")

#相关系数矩阵
rubber<-read.table("rubber.data");rubber
colMeans(rubber)
cov(rubber)
cor(rubber)

#计算一下结果，不拒绝X1, X2, X3两两之间是不相关的
#（或者说认为X1, X2, X3两两之间是不相关的，当然这种说法有争议）
cor.test(~X1+X2, data=rubber)
cor.test(~X1+X3, data=rubber)
cor.test(~X2+X3, data=rubber)

#基于相关系数的变量分类
#48位面试者面试职位。有15项指标，需要挑选6位最优秀的
rt <- read.table("applicant.data")
me <- apply(rt, 1, mean)
sort(me, decreasing = TRUE)[1:6]
#取平均成绩是一般的方法，但是这种方法有些不足，因为很多指标之间相关度非常高，
#所以需要剔除某些相关度非常高的指标
co <- cor(rt)
varcount <- dim(co)[1]
varnames <- colnames(co)
low_tri <- which(co>0.5 & lower.tri(co))   #得到下半矩阵中相关系数大于0.5
#得到行列编号
low_tri_ij <- t(apply(t(low_tri), 2, function(x) c((x-1)%%varcount+1, (x-1)%/%varcount+1)))
low_tri_name <- t(apply(low_tri_ij, 1, function(x) c(varnames[x[1]], varnames[x[2]])))
low_tri_name;low_tri_ij
#经过分析分成6组,这个结果和书上有微小出入
#第1组:5,6,8,10,11,12,13
#第2组:1,9,15
#第3组:4,14
#第4组:2
#第5组:3
#第6组:7
varnames[c(5,6,8,10,11,12,13)]
varnames[c(1,9,15)]
varnames[c(4,14)]
varnames[c(2)]
varnames[c(3)]
varnames[c(7)]
newscore <- function(x) {
  (
    mean(x[c(5,6,8,10,11,12,13)]) + 
      mean(x[c(1,9,15)]) +
      mean(x[c(4,14)]) +
      x[2] + 
      x[3] + 
      x[7]
  )/5
}  
newme <- apply(rt, 1, newscore)
sort(newme, decreasing = TRUE)[1:6]
sort(me, decreasing = TRUE)[1:6]

#R Modeling
#3.1 
students <- scan()
74.3 78.8 68.8 78.0 70.4 80.5 80.5 69.7 71.2 73.5
79.5 75.6 75.0 78.8 72.0 72.0 72.0 74.3 71.2 72.0
75.0 73.5 78.8 74.3 75.8 65.0 74.3 71.2 69.7 68.0
73.5 75.0 72.0 64.3 75.8 80.3 69.7 74.3 73.5 73.5
75.8 75.8 68.8 76.5 70.4 71.2 81.2 75.0 70.4 68.0
70.4 72.0 76.5 74.3 76.5 77.6 67.3 72.0 75.0 74.3
73.5 79.5 73.5 74.7 65.0 76.5 81.6 75.4 72.7 72.7
67.2 76.5 72.7 70.4 77.2 68.8 67.3 67.3 67.3 72.7
75.8 73.5 75.0 73.5 73.5 73.5 72.7 81.6 70.3 74.3
73.5 79.5 70.4 76.5 72.7 77.2 84.3 75.0 76.5 70.4

students 
data_outline(students)

#3.2
hist(students, freq=F)
lines(density(students), col = "blue")
x <- min(students):max(students)
lines(x, dnorm(x, mean(students), sd(students)), col = "red")
#经验分布
plot(ecdf(students),verticals = TRUE, do.p = FALSE)
lines(x, pnorm(x, mean(students), sd(students)))
#QQ图
qqnorm(students);qqline(students)

#3.3
sort(students)
stem(students) 
stem(students, scale=0.25) 
boxplot(students)
fivenum(students)

#3.4
shapiro.test(students)
#这里的警告信息，是因为数据中有重复的数值，ks检验要求待检数据时连续的，不允许重复值。
ks.test(students,"pnorm",mean(students),sd(students) )
ks.test(unique(students),"pnorm",mean(students),sd(students) )


#3.5
x1 <- c(2, 4, 3, 2, 4, 7, 7, 2, 2, 5, 4)
x2 <- c(5, 6, 8, 5, 10, 7, 12, 12, 6, 6)
x3 <- c(7, 11, 6, 6, 7, 9, 5, 5, 10, 6, 3, 10)
boxplot(x1, x2, x3) 
f <- factor(c(rep(1,length(x1)), rep(2,length(x1)), rep(3,length(x1))))
plot(c(x1, x2, x3), f)

#3.6
rubber<-read.table("rubber.data");rubber
colMeans(rubber)
cov(rubber)
cor(rubber)
plot(rubber$X1, rubber$X2)
plot(rubber$X1, rubber$X3)
plot(rubber$X2, rubber$X3)
plot(rubber)
pairs(rubber)
#从散点图看来，x1, x2, x3相互之间都呈现均匀的随机分布，所以认为它们之间是相互独立的

#3.7
childs <- read.table('childs.data')
attach(childs)
plot(childs)
plot(~Height+Weight)
coplot(Weight~Height|Gender)
coplot(Weight~Height|Age)
coplot(Weight~Height|Age+Gender)
detach(childs)

#3.8
Z <- function(x, y){
  
  x^4-2*x^2*y+x^2-2*x*y+2*y^2+9/2*x-4*y+4
}
x <- seq(-2, 3, by=0.05)
y <- seq(-1, 7, by=0.05)
#outer这个函数能够计算xy之间的外积，并且制定计算的函数，非常不错。
z<-outer(x,y,Z) 
#三维图形等值线
contour(x, y, z, levels = c(0, 1, 2, 3, 4, 5, 10, 15, 20, 30, 40, 50, 60, 80, 100))
#三维图形表面曲线
persp(x, y, z, theta = 30, phi = 45, expand = 0.7, col='lightblue')

#3.9
childs <- read.table('childs.data')
cor(childs[c('Weight','Height')])
cor.test(childs$Weight, childs$Height)

#3.10
rt <- read.table("applicant.data")
stars(rt)
G1<- apply(rt[,c(5,6,8,10,11,12,13)], 1, mean)  
G2<- apply(rt[,c(1,9,15)], 1, mean) 
G3<- apply(rt[,c(4,14,7)], 1, mean) 
G4<- rt[,2]  
G5<- rt[,3] 
sort(G1+G2+G3+G4+G5, decreasing = T)[1:6] #8 40 39 7 23 9 
#通过图形我们可以轻易选出 7,8,23, 39, 40, 24
stars(data.frame(G1, G2, G3, G4, G5))

#3.11
unison(data.frame(G1, G2, G3, G4, G5))

#层次聚类
x<-c(1,2,6,8,11); dim(x)<-c(5,1); d<-dist(x)
hc1<-hclust(d, "single"); hc2<-hclust(d, "complete")
hc3<-hclust(d, "median"); hc4<-hclust(d, "mcquitty")
opar <- par(mfrow = c(2, 2)) #分屏
plot(hc1,hang=-1); plot(hc2,hang=-1)
plot(hc3,hang=-1); plot(hc4,hang=-1)
par(opar)

dend1<-as.dendrogram(hc1)
opar <- par(mfrow = c(2, 2),mar = c(4,3,1,2))
plot(dend1)
plot(dend1, nodePar=list(pch = c(1,NA), cex=0.8, lab.cex=0.8),
     type = "t", center=TRUE)
plot(dend1, edgePar=list(col = 1:2, lty = 2:3),
     dLeaf=1, edge.root = TRUE)
plot(dend1, nodePar=list(pch = 2:1, cex=.4*2:1, col=2:3),
     horiz=TRUE)
par(opar)




#优化问题
#设定线性规划问题：
#用Rglpk包解决线性规划和整数规划
# max z = 2x1 + 4x2 + 3x3
# subject to :
#   3x1 + 4x2 + 2x3 <= 60
#   2x1 +  x2 +  x3 <= 40
#    x1 + 3x2 + 2x3 <= 80
#    x1, x2, x3 >= 0
library(Rglpk)
obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
Rglpk_solve_LP(obj, mat, dir, rhs, max = TRUE)

# max z = 3x1 + x2+ 3x3
# subject to:
# −x1 + 2x2 +  x3 <= 4
#       4x2 − 3x3 <= 2 
#  x1 − 3x2 + 2x3 <= 3
# x1, x3 为正整数
library(Rglpk) #GLPK (GNU Linear Programming Kit)
obj <- c(3, 1, 3)
mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3)
dir <- rep("<=", 3)
rhs <- c(4, 2, 3)
types <- c("I", "C", "I") 
Rglpk_solve_LP(obj, mat, dir, rhs, types, max = TRUE)

#lpSolve包和运输问题
library(lpSolve)
costs <- matrix(c(4,2,8,12,10,5,4,3,11,11,9,6),nrow=3) 
row.signs <- rep("=", 3) 
row.rhs <- c(16, 10, 22) 
col.signs <- rep("=", 4)
col.rhs <- c(8,14,12,14) 
res <- lp.transport(costs,"min",row.signs,row.rhs,col.signs,col.rhs)
res 
res$solution 

# Set up problem: maximize
# x1 + 9 * x2 + x3
# subject to
# x1 + 2 * x2 + 3 * x3 <= 9
# 3 * x1 + 2 * x2 + 2 * x3 <= 15
library(lpSolve)
f.obj <- c (1, 9, 1)
f.con <- matrix ( c (1, 2, 3, 3, 2, 2), nrow = 2, byrow = TRUE )
f.dir <- c ( "<=" , "<=" )
f.rhs <- c (9, 15)
#使用lp()函数求解：
lp.result <- lp ( "max" , f.obj, f.con, f.dir, f.rhs)
lp.result$solution

# target: max C = 5*x1 + 8*x2
# subject to:
#   x1 + x2 <= 2
# x1 + 2*x2 = 3
# x1,x2 >=0：
library(lpSolve)
eg.lp <- lp(objective.in=c(5, 8),
            const.mat=matrix(c(1, 1, 1, 2), nrow=2),
            const.rhs=c(2, 3),
            const.dir=c("<=", "="), direction="max")
eg.lp$solution;eg.lp

#see Page 27 in Applying Optimization Techniques to Long-Term Workforce Planning.pdf
#1. Data Preparation
s.len <- 3  #3 Skills
p.len <- 4  #4 Positions
t.len <- 6  #6 Periods
s.names <- paste('s', 1:s.len, sep='')
p.names <- paste('p', 1:p.len, sep='')
t.names <- paste('t', 1:t.len, sep='')

#S[k] : number of employees available for position k at time 0
S <- scan()
16 12 10 8

names(S) <- p.names
S

#R[j, t] : number of employees of skill j needed in time period t 
R <- scan()
18 13 14
16 17 15
10 15 10
18 19 13
12 14 10
16 16 14

R <- matrix(R, nrow=3, byrow=F, dimnames=list(s.names, t.names))
R

#alpha : natural attrition rate. the gradual reduction of employees due to retirement and resignation, is yet another challenge.
alpha <- 0.99

#a[j, k]: 1 if position kcan perform skill j, 0 otherwise (position-skill matrix) 
a <- scan()
1 0 0
0 1 0
0 0 1
0 0 1

a <- matrix(a, nrow=3, byrow=F, dimnames=list(s.names, p.names))
a

#h[k, t] : fixed cost of hiring someone with position k
h <- scan()
1000 1500 2000 2500
1000 1500 2000 2500
1000 1500 2000 2500
1000 1500 2000 2500
1000 1500 2000 2500
1000 1500 2000 2500

h <- matrix(h, nrow=4, byrow=F, dimnames=list(p.names, t.names))
h


#f[k, t] : fixed cost of releasing someone with position k
f <- scan()
600 1100 1600 2100
600 1100 1600 2100
600 1100 1600 2100
600 1100 1600 2100
600 1100 1600 2100
600 1100 1600 2100

f <- matrix(f, nrow=4, byrow=F, dimnames=list(p.names, t.names))
f

#g[k, t] : fixed cost of releasing someone with position k
g <- scan()
5000 7000 9000 10000
5000 7000 9000 10000
5000 7000 9000 10000
5000 7000 9000 10000
5000 7000 9000 10000
5000 7000 9000 10000

g <- matrix(g, nrow=4, byrow=F,dimnames=list(p.names, t.names))
g

#2. Define the objective and constraints
#defin initial value for variables.
x.base <- array(0, c(s.len, p.len, t.len), dimnames=list(s.names, p.names, t.names))
y.base <- matrix(0, nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names))
z.base <- matrix(0, nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names))

#define the objective
wp.obj.x <- x.base
wp.obj.y <- h + t(apply(g[,t.len:1], 1, cumsum))[,t.len:1]
wp.obj.z <- f - t(apply(g[,t.len:1], 1, function(x) cumsum(x)-x))[,t.len:1]
wp.obj <- c(wp.obj.x, wp.obj.y, wp.obj.z)
wp.obj.dir <- 'min'

#initial con, dir and rhs
#wp.con.mat.base <- c(x.base, y.base, z.base)
wp.con.mat <- numeric(0)
wp.con.dir <- numeric(0)
wp.con.rhs <- numeric(0)

#contraint 1
x.test<-list()
for(i in 1:s.len){
  for(j in 1:t.len){
    x.temp <- x.base
    x.temp[i,,j] <- a[i,]
    x.test[[paste(paste('s',i, sep=''), paste('t',j, sep=''), sep=',')]] <- x.temp
    wp.con.mat <- rbind(wp.con.mat, c(x.temp, y.base, z.base))
    wp.con.dir <- c(wp.con.dir, '>=')
    wp.con.rhs <- c(wp.con.rhs, R[i,j])
  }
}

#test
a[2,]
x.test[['s2,t1']][2,,1]
x.test[['s2,t2']][2,,2]
x.test[['s2,t3']][2,,3]
wp.con.rhs

#contraint 2
x.test<-list()
y.test<-list()
z.test<-list()
for(i in 1:p.len){
  for(j in 1:t.len){
    x.temp <- x.base
    x.temp[,i,j] <- rep(1, s.len)
    x.test[[paste(paste('p',i, sep=''), paste('t',j, sep=''), sep=',')]] <- x.temp
    y.temp <- y.base
    y.temp[i,1:j] <- -apply(t(j:1), 2, function(x) alpha^(x-1))
    y.test[[paste(paste('p',i, sep=''), paste('t',j, sep=''), sep=',')]] <- y.temp
    z.temp <- z.base        
    z.temp[i,1:j] <- apply(t(j:1), 2, function(x) alpha^(x-1))
    z.test[[paste(paste('p',i, sep=''), paste('t',j, sep=''), sep=',')]] <- z.temp
    wp.con.mat <- rbind(wp.con.mat, c(x.temp, y.temp, z.temp))
    wp.con.dir <- c(wp.con.dir, '<=')
    wp.con.rhs <- c(wp.con.rhs, alpha^j*S[i])
  }
}

#test
#a[2,]
x.test[['p2,t1']][,,1]
x.test[['p2,t2']][,,2]
x.test[['p2,t3']][,,3]
y.test[['p2,t1']]
y.test[['p2,t2']]
y.test[['p2,t3']]
y.test[['p2,t4']]
y.test[['p2,t5']]
y.test[['p2,t6']]
z.test[['p2,t1']]
z.test[['p2,t2']]
z.test[['p2,t3']]
z.test[['p2,t4']]
z.test[['p2,t5']]
z.test[['p2,t6']]
wp.con.rhs 
length(wp.obj)
dim(wp.con.mat)
length(wp.con.dir)
length(wp.con.rhs )

#3. Execute linear optimiaztion and get the solutions
#get the result
xyz_result <- function(solution){
  x <- array(solution[1:(s.len*p.len*t.len)], c(s.len, p.len, t.len), dimnames=list(s.names, p.names, t.names))
  y <- matrix(solution[1:(p.len*t.len)+(s.len*p.len*t.len)], nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names))
  z <- matrix(solution[1:(p.len*t.len)+(s.len*p.len*t.len)+(p.len*t.len)], nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names)) 
  list(x=x, y=y, z=z)  
}

library(lpSolve)
wp.result <- lp ( wp.obj.dir, wp.obj, wp.con.mat, wp.con.dir, wp.con.rhs)
wp.result$solution

xyz <- xyz_result(wp.result$solution)
xyz$x
xyz$y
xyz$z
wp.result$objval + sum(rowSums(g)*S)

library(lpSolve)
wp.result <- lp ( wp.obj.dir, wp.obj, wp.con.mat, wp.con.dir, wp.con.rhs, all.int=T)
wp.result$solution

xyz <- xyz_result(wp.result$solution)
xyz$x
xyz$y
xyz$z
wp.result$objval + sum(rowSums(g)*S)


# library(Rglpk)
# wp.result1 <- Rglpk_solve_LP(wp.obj, wp.con.mat, wp.con.dir, wp.con.rhs,  max = F)
# wp.result1 
# 
# xyz1 <- xyz_result(wp.result1$solution)
# xyz1$x
# xyz1$y
# xyz1$z
# wp.result1$optimum + sum(rowSums(g)*S)
# 
# sum(xyz$x-xyz1$x);round(xyz$x-xyz1$x,4)
# sum(xyz$y-xyz1$y);round(xyz$y-xyz1$y,4)
# sum(xyz$z-xyz1$z);round(xyz$z-xyz1$z,4)

# library(Rglpk)
# wp.result1 <- Rglpk_solve_LP(wp.obj, wp.con.mat, wp.con.dir, wp.con.rhs, types=rep('I',s.len*p.len*t.len), max = F)
# wp.result1 
# 
# xyz1 <- xyz_result(wp.result1$solution)
# xyz1$x
# xyz1$y
# xyz1$z
# wp.result1$optimum + sum(rowSums(g)*S)
# 
# sum(xyz$x-xyz1$x);round(xyz$x-xyz1$x,4)
# sum(xyz$y-xyz1$y);round(xyz$y-xyz1$y,4)
# sum(xyz$z-xyz1$z);round(xyz$z-xyz1$z,4)

#Version 0.5 : use execl as the inputs
#see Page 27 in Applying Optimization Techniques to Long-Term Workforce Planning.pdf
#1. Data Preparation
library(XLConnect)
#In order to use XLConnect, you need to ensure that you have installed Java Run Time 64 bit.
#If not, install Java Run Time 64 bit from http://java.com/en/download/manual.jsp
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre7')

input_file <- "D:/xujian/project/AA_ES/Workforce_Planning/data/wp_inputs.xlsx"
Skill <- readWorksheetFromFile(input_file, sheet="Skill")
Position <- readWorksheetFromFile(input_file, sheet="Position")
Time_Period <- readWorksheetFromFile(input_file, sheet="Time_Period")
Attrition_Rate <- readWorksheetFromFile(input_file, sheet="Attrition_Rate")
Skill_Position <- readWorksheetFromFile(input_file, sheet="Skill_Position")
Position_T0 <- readWorksheetFromFile(input_file, sheet="Position_T0")
Skill_Time_Period <- readWorksheetFromFile(input_file, sheet="Skill_Time_Period")
Hire_Cost <- readWorksheetFromFile(input_file, sheet="Hire_Cost")
Fire_Cost <- readWorksheetFromFile(input_file, sheet="Fire_Cost")
Position_Cost <- readWorksheetFromFile(input_file, sheet="Position_Cost")

s.len <- dim(Skill)[1]  
p.len <- dim(Position)[1]  
t.len <- dim(Time_Period)[1]  
s.names <- Skill$'Skill_ID'
p.names <- Position$'Position_ID'
t.names <- Time_Period$'Time_Period_ID'

#S[k] : number of employees available for position k at time 0
S <- Position_T0$T0
names(S) <- p.names
S

#R[j, t] : number of employees of skill j needed in time period t 
R <- as.matrix(Skill_Time_Period[,-1])
dimnames(R) <- list(s.names, t.names)
R

#alpha : natural attrition rate. the gradual reduction of employees due to retirement and resignation, is yet another challenge.
alpha <- Attrition_Rate$Attrition_Rate

#a[j, k]: 1 if position kcan perform skill j, 0 otherwise (position-skill matrix) 
a <- as.matrix(Skill_Position[,-1])
dimnames(a) <- list(s.names, p.names)
a

#h[k, t] : fixed cost of hiring someone with position k
h <- as.matrix(Hire_Cost[,-1])
dimnames(h) <- list(p.names, t.names)
h

#f[k, t] : fixed cost of releasing someone with position k
f <- as.matrix(Fire_Cost[,-1])
dimnames(f) <- list(p.names, t.names)
f

#g[k, t] : cost of position k per time period (months)
g <- as.matrix(Position_Cost[,-1])
dimnames(g) <- list(p.names, t.names)
g

#2. Define the objective and constraints
#defin initial value for variables.
x.base <- array(0, c(s.len, p.len, t.len), dimnames=list(s.names, p.names, t.names))
y.base <- matrix(0, nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names))
z.base <- matrix(0, nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names))

#define the objective
wp.obj.x <- x.base
wp.obj.y <- h + t(apply(g[,t.len:1], 1, cumsum))[,t.len:1] #累加函数
wp.obj.z <- f - t(apply(g[,t.len:1], 1, function(x) cumsum(x)-x))[,t.len:1]
wp.obj <- c(wp.obj.x, wp.obj.y, wp.obj.z)
wp.obj.dir <- 'min'

#initial con, dir and rhs
#wp.con.mat.base <- c(x.base, y.base, z.base)
wp.con.mat <- numeric(0)
wp.con.dir <- numeric(0)
wp.con.rhs <- numeric(0)

#contraint 1
x.test<-list()
for(i in 1:s.len){
  for(j in 1:t.len){
    x.temp <- x.base
    x.temp[i,,j] <- a[i,]
    x.test[[paste(paste('s',i, sep=''), paste('t',j, sep=''), sep=',')]] <- x.temp
    wp.con.mat <- rbind(wp.con.mat, c(x.temp, y.base, z.base))
    wp.con.dir <- c(wp.con.dir, '>=')
    wp.con.rhs <- c(wp.con.rhs, R[i,j])
  }
}

#test
a[2,]
x.test[['s2,t1']][2,,1]
x.test[['s2,t2']][2,,2]
x.test[['s2,t3']][2,,3]
wp.con.rhs

#contraint 2
x.test<-list()
y.test<-list()
z.test<-list()
for(i in 1:p.len){
  for(j in 1:t.len){
    x.temp <- x.base
    x.temp[,i,j] <- rep(1, s.len)
    x.test[[paste(paste('p',i, sep=''), paste('t',j, sep=''), sep=',')]] <- x.temp
    y.temp <- y.base
    y.temp[i,1:j] <- -apply(t(j:1), 2, function(x) alpha^(x-1))
    y.test[[paste(paste('p',i, sep=''), paste('t',j, sep=''), sep=',')]] <- y.temp
    z.temp <- z.base        
    z.temp[i,1:j] <- apply(t(j:1), 2, function(x) alpha^(x-1))
    z.test[[paste(paste('p',i, sep=''), paste('t',j, sep=''), sep=',')]] <- z.temp
    wp.con.mat <- rbind(wp.con.mat, c(x.temp, y.temp, z.temp))
    wp.con.dir <- c(wp.con.dir, '<=')
    wp.con.rhs <- c(wp.con.rhs, alpha^j*S[i])
  }
}

#test
#a[2,]
x.test[['p2,t1']][,,1]
x.test[['p2,t2']][,,2]
x.test[['p2,t3']][,,3]
y.test[['p2,t1']]
y.test[['p2,t2']]
y.test[['p2,t3']]
y.test[['p2,t4']]
y.test[['p2,t5']]
y.test[['p2,t6']]
z.test[['p2,t1']]
z.test[['p2,t2']]
z.test[['p2,t3']]
z.test[['p2,t4']]
z.test[['p2,t5']]
z.test[['p2,t6']]
wp.con.rhs 
length(wp.obj)
dim(wp.con.mat)
length(wp.con.dir)
length(wp.con.rhs )

#3. Execute linear optimiaztion and get the solutions
#get the result
xyz_result <- function(solution){
  x <- array(solution[1:(s.len*p.len*t.len)], c(s.len, p.len, t.len), dimnames=list(s.names, p.names, t.names))
  y <- matrix(solution[1:(p.len*t.len)+(s.len*p.len*t.len)], nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names))
  z <- matrix(solution[1:(p.len*t.len)+(s.len*p.len*t.len)+(p.len*t.len)], nrow=p.len, ncol=t.len, dimnames=list(p.names, t.names)) 
  list(x=x, y=y, z=z)  
}

library(lpSolve)
wp.result <- lp ( wp.obj.dir, wp.obj, wp.con.mat, wp.con.dir, wp.con.rhs)
wp.result$solution

xyz <- xyz_result(wp.result$solution)
xyz$x
xyz$y
xyz$z
wp.result$objval + sum(rowSums(g)*S)

#----------------------------------------------
#Text Mining
#----------------------------------------------
# library(Snowball)
# SnowballStemmer(c('functions', 'stemming', 'liked', 'doing'))
# 
# library(Rwordseg)
# segmentCN(' 花儿为什么这样红')

#1 tm包
#1.1  创建语料库
library(tm)
#文本文件数据读入，创建语料库
txt <- system.file("texts", "txt", package = "tm")
(ovid <- Corpus(DirSource(txt), readerControl = list(language = "lat")))


#字符向量读入，创建语料库
docs <- c("This is a text.", "This another one.")
Corpus(VectorSource(docs))

#读入路透社文档，创建语料库
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML))
reuters

#1.2 语料库输出到文件
writeCorpus(ovid, 'D:/xujian/tmp')

#1.3 语料库的提取
inspect(ovid[1:2])
summary(ovid)
print(ovid)
identical(ovid[[2]], ovid[["ovid_2.txt"]])

#1.4 信息转化
reuters <- tm_map(reuters, as.PlainTextDocument) #转化为纯文本
inspect(reuters[1])
reuters <- tm_map(reuters, stripWhitespace) #去除多余空格
inspect(reuters[1])
reuters <- tm_map(reuters, tolower) #小写变换
inspect(reuters[1])
reuters <- tm_map(reuters, removeWords, stopwords("english")) #去除停止词
#比如 the, is, at, which 和 on 等不表意的词汇，tm 包提供了近五百个英文停止词，除英文外还支持法语、荷兰
#语、西班牙语、意大利语等十几种语言。关于停止词的定义参见 http://en.wikipedia.org/wiki/Stopwords
inspect(reuters[1])
tm_map(reuters, stemDocument) #填充 but Snowball is not available for R version 3.1.0

#1.5 过滤
query <- "id == '237' & heading == 'INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE'"
tf <- tm_filter(reuters, FUN = sFilter, query)
inspect(tf)

# #全文检索 run the following 2 lines unsuccessfully
# tm_filter(reuters, pattern = "company")
# tm_filter(reuters, FUN = searchFullText, "company")

#1.6 源数据管理
DublinCore(reuters[[1]], tag = "creator") <- "Ano Nymous"
DublinCore(reuters[[1]], tag = "Subject") <- "Nothing"
DublinCore(reuters[[1]])
meta(reuters[[1]])
meta(reuters[[1]], tag = "Topics")
meta(reuters[[1]], tag = "Topics")
reuters[[1]]

meta(reuters, type = "corpus")
meta(reuters, tag = "test", type = "corpus") <- "test meta"
meta(reuters, type = "corpus")
meta(reuters)
meta(reuters, "foo") <- letters[1:20]
meta(reuters)

#1.7 创建词条-文档关系矩阵
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[1:5,100:105])

#1.8 操作词条-文档关系矩阵
# 查找出发生5次以上的条目
findFreqTerms(dtm, 8)
#找出相关系数在0.8以上的条目
findAssocs(dtm, "opec", 0.8)
#删减低于40%的稀疏条目
inspect(removeSparseTerms(dtm, 0.4))

#1.9 字典
#(d <- Dictionary(c("prices", "crude", "oil"))) why can't find Dictionary?
inspect(DocumentTermMatrix(reuters, list(dictionary = c("prices", "crude", "oil"))))

#2 网页解析的利器 –XML 包     
library(XML)
fileName <- system.file("exampleData", "include.xml", package="XML")
root <- xmlParse(fileName)
test <- getNodeSet(root, '//para')
vdata <- sapply(test, xmlValue)
vdata

doc <- xmlParse(system.file("exampleData", "tagnames.xml", package = "XML"))
els <- getNodeSet(doc, "/doc//a[@status]")
sapply(els, function(el) xmlGetAttr(el, "status"))

#如果遇到中文字符，编码转换问题
iconv(x, from = "", to = "", sub = NA, mark = TRUE, toRaw = FALSE)

#3 一些文本挖掘方面的应用
#基本分析技术
#1) 频数提取（ findFreqTerms ）
#2) 相关性提取（ findAssocs ）
#3) 计算 cosine 距离的 dissimilarity 函数
#4) wordcloud 包提供了绘制文本云的技术，甚至还可以对两个文本云进行比较。

#3.1 文本聚类
library(tm)
library(proxy)
data(acq)
data(crude)
m <- c(acq, crude)
dtm <- DocumentTermMatrix(m)
dtm <- removeSparseTerms(dtm, 0.8)
inspect(dtm[1:5, 1:5])
dist_dtm <- dissimilarity(dtm, method = 'cosine')
hc <- hclust(dist_dtm, method = 'ave')
plot(hc, xlab = '')



#3.2 文本分类
#3.3 潜变量语义分析
#3.4 主题模型（Topic model）
commentPath <- "D:/xujian/project/R/text_mining/jd_comment_0606/segment/Test"
simpleCommentFileNames <- dir(commentPath, '/d+.01.txt')
remove_star <- function(sourcefile, targetfile){
  comments <- read.delim(sourcefile, header=F, sep="|", quote="", stringsAsFactors=F) 
  write.table(comments[,2], targetfile, row.names=F, col.names=F, sep="", quote=FALSE)
}
remove_star1 <- function(filename, path){
  remove_star(
    paste(path, '/', filename, sep=''),
    paste(path, '/', substr(filename, 1, 6), sep='')
  )
}

dummy <- lapply(simpleCommentFileNames, function(x) remove_star1(x, commentPath))


doc_CN <- list()
for(i in 1:length(simpleCommentFileNames)){
  filename <-paste(commentPath, '/', substr(simpleCommentFileNames[i], 1, 6), sep='')  
  doc_CN[[i]]=c(segmentCN(filename), returnType='vector')
}
library(tm)
doc.corpus=Corpus(VectorSource(doc_CN))
inspect(doc.corpus)
control=list(removePunctuation=T,minDocFreq=5,wordLengths = c(1, Inf),weighting = weightTfIdf)
doc.tdm=TermDocumentMatrix(doc.corpus)
inspect(doc.tdm[10:20, 1:7])
inspect(doc.tdm)

# R通过ODBC连接数据库
library(RODBC)
library(tm)
library(wordcloud)
library(ggplot2)
# Conn to Hopper DB and Import to Dataframe
conn <- odbcConnect('aa_vertica_server', uid='dbadmin', pwd='dbadmin')
Hopper.Data<-sqlQuery(conn,"select Company_ID, _System, _Date from online_sales.hopper1")
names(Hopper.Data) <- sub(" ", ".", names(Hopper.Data))

# or load the file from flat file
Hopper.Data<-read.csv('D:/xujian/project/AA_ES/TextMining/Data/hopper1.txt', header=F, sep="|")
#Hopper.Data<-read.csv('D:/xujian/project/AA_ES/TextMining/Data/hopper2.txt', header=F, sep="|")
names(Hopper.Data) <- c('Company_ID', '_System', '_Date', 'Message_Text')

memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存
Hopper.Data<-Hopper.Data[1:10000,]
gc()
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存

# build a corpus using the text mining (tm) package
hopper_corpus <- Corpus(VectorSource(Hopper.Data$Message_Text))

# clean up the corpus using tm_map()
hopper_corpus_clean <- tm_map(hopper_corpus, tolower)
hopper_corpus_clean <- tm_map(hopper_corpus_clean, removeNumbers)
hopper_corpus_clean <- tm_map(hopper_corpus_clean, removeWords, stopwords())
hopper_corpus_clean <- tm_map(hopper_corpus_clean, removePunctuation)
hopper_corpus_clean <- tm_map(hopper_corpus_clean, stripWhitespace)

# create a document-term sparse matrix
hopper_corpus_dtm <- DocumentTermMatrix(hopper_corpus_clean)

termFrequency <- rowSums(as.matrix(hopper_corpus_dtm))
termFrequency <- subset(termFrequency, termFrequency>=15)
qplot(names(termFrequency), termFrequency, geom="bar", xlab="Terms") + coord_flip()

# word cloud visualization
wordcloud(hopper_corpus_clean, min.freq = 30, random.order = FALSE)

# indicator features for frequent words
hopper_corpus_dict <- findFreqTerms(hopper_corpus_dtm, 50)
Hopper <- DocumentTermMatrix(hopper_corpus_clean, list(dictionary = hopper_corpus_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of Hopper data
Hopper <- apply(Hopper, MARGIN = 2, convert_counts)

#---------------------------------------
#中文分词包： Rwordseg
#http://jliblog.com/app/rwordseg
# Rwordseg 是一个R环境下的中文分词工具，使用rJava调用Java分词工具Ansj。
# Ansj 也是一个开源的 Java 中文分词工具，基于中科院的 ictclas 中文分词算法，
# 采用隐马尔科夫模型（Hidden Markov Model, HMM）。作者孙健重写了一个Java版本，
# 并且全部开源，使得 Ansi 可用于人名识别、地名识别、组织机构名识别、多级词性标注、
# 关键词提取、指纹提取等领域，支持行业词典、 用户自定义词典。
# 详细信息可以参考作者孙健的专访以及项目的Github地址。
# 当前版本的Rwordseg包完全引用了 Ansj 包，只是简单提供了R的接口
# ，并根据R中处理文本的习惯进行了调整，在此对原作者孙健表示强烈的敬意！
# 之所以没有命名为ransj，是因为以后可能还会纳入其他的 分词工具或者自己开发一些新的功能。
#---------------------------------------
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre7')
library(Rwordseg)
teststring1 <- "小都督促织女将R语言学习到底"
teststring2 <- "小都是统计之都的昵称"

#2 和其他分词包的对比
#2.1 rmmseg4j
require(rmmseg4j)
mmseg4j(teststring1)
mmseg4j(teststring2)
#速度很慢，要3分多钟
system.time(for(i in 1:10000) mmseg4j(teststring1))

#2.2 imdict-chinese-analyzer
require(RsegWord)
segWord(teststring1)
segWord(teststring2)

#2.3 ansj
# segmentCN(strwords,      
#           analyzer = get("Analyzer", envir = .RwordsegEnv),     
#           nature = FALSE, nosymbol = TRUE,      
#           returnType = c("vector", "tm"), 
#           isfast = FALSE,     
#           outfile = "", 
#           blocklines = 1000)
require(Rwordseg)
segmentCN(teststring1)
segmentCN(teststring2)

#不受干扰词影响
insertWords(c("促织","都督"))
segmentCN(teststring1)

#性能大大提高
system.time(for(i in 1:10000) segmentCN(teststring1))

#3 分词操作
#3.1 默认分词
segmentCN("结合成分子时")
segmentCN(c("说的实在在理", "一次性交多少钱", "非常好用，惠普的产品，代表了品质，一直喜欢。"))
segmentCN("想渊明，《停运》诗就，此时风味。")
#输出标点
segmentCN("想渊明，《停运》诗就，此时风味。",nosymbol = FALSE)

#3.2 词性识别
dd <- segmentCN(c("花一元钱买了一朵美丽的花", "说的实在在理", 
                  "一次性交多少钱", "非常好用，惠普的产品，代表了品质，一直喜欢。")
                , nature = TRUE)
dd[[1]]
ddd<-unlist(dd);
names(ddd)
ddd[20]

#3.3 人名识别
getOption("isNameRecognition")
segmentCN("梅野石是东昆仑盟主")

segment.options(isNameRecognition = TRUE)
segmentCN("梅野石是东昆仑盟主")

3.4 tm格式的支持
3.5 对文件的分词
segmentCN("D:/xujian/project/R/text_mining/data/1105059.txt")
system.time(segmentCN("D:/xujian/project/R/text_mining/data/1105059.txt"))

#4 词典管理
4.1 安装和卸载词典
listDict()
segmentCN("湖北大鼓真是不错啊")
installDict("D:/xujian/project/R/text_mining/dic/default.dic", "default")
segmentCN("湖北大鼓真是不错啊")
segmentCN("帮同事买的，性价比超高，才2999元，就是要自己装系统~~~")
segmentCN("D:/xujian/project/R/text_mining/jd_comment_0528/1105059.txt")

#http://pinyin.sogou.com/dict/中有大量词库
segmentCN("")

uninstallDict()

#4.2 自定义文本词典
#在%R_HOME%\library\Rwordseg\dict下添加你的词典

#4.3 手动添加和删除词汇
deleteWords(c("价比高","比高"))
segmentCN("D:/xujian/project/R/text_mining/jd_comment_0528/1105059.txt")

#-----------------------------------------------------
#JD Text Mining
#-----------------------------------------------------
library(tm)
library(wordcloud)
library(ggplot2)
library(rJava)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre7')
library(Rwordseg)
library(plyr)
library(XLConnect)
Sys.setlocale(locale='Chinese');

commentPath <- 'D:/xujian/project/R/text_mining/jd_comment_0606'
commentFileNames <- dir(commentPath, '/d+.txt')
product_ids <- sapply(strsplit(commentFileNames, '/.'), function(x) x[1])
comments <- list()
comments.good <- list()
comments.bad <- list()
comments.mediate <- list()
fileNames <- paste(commentPath, commentFileNames, sep='/')
productinfoFile <- paste(commentPath, 'product_info.txt', sep='/')


for (i in 1:length(product_ids)){
  comment <- read.delim(fileNames[i], header=F, sep="|", quote="", stringsAsFactors=F)   
  names(comment) <- c("Product_ID", "Username", "Buy_date", "Comment_date", "Star", "Text", "Location", "Label")
  comments[[product_ids[i]]] <- comment
  
  comment.good <- subset(comment, Star==4|Star==5)
  comment.bad <- subset(comment, Star==1|Star==0)
  comment.mediate <- subset(comment, Star==2|Star==3)
  comments.good[[product_ids[i]]] <- comment.good
  comments.bad[[product_ids[i]]] <- comment.bad
  comments.mediate[[product_ids[i]]] <- comment.mediate
}

good.count <- sum(sapply(comments.good, function(x) dim(x)[1]))  #78466
bad.count <- sum(sapply(comments.bad, function(x) dim(x)[1]))  #1866
mediate.count <- sum(sapply(comments.mediate, function(x) dim(x)[1])) #1866
total.count <- sum(sapply(comments, function(x) dim(x)[1]))  #85342
c(good = good.count,bad = bad.count,meidate = mediate.count)
c(good = good.count,bad = bad.count,meidate = mediate.count)/total.count

memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存
gc()
memory.size(T)   #查看已分配的内存
memory.size(F)   #当前R实际使用的内存

#检查分词效果
segment_to_file <- function(i, comments, fileNames){
  words <- segmentCN(as.character(comments[[i]]$Text))
  words <- sapply(words, paste, collapse=" ")
  words <- cbind(comments[[i]]$Star, words)
  write.table(words, fileNames[i], col.names=F, row.names=F, quote = F)
  print(paste('Write the segement results to', fileNames[i]))
  return(T)
}
# uninstallDict()
# listDict()
# segmentFileNames <- gsub('/.txt', '.00.txt', paste(commentPath, 'segment', commentFileNames, sep='/'))
# proc <- lapply(1:length(product_ids), segment_to_file , comments=comments, fileNames=segmentFileNames) #输出所有的分词结果到segment folder
# 
# installDict("D:/xujian/project/R/text_mining/dic/default.dic", "default")
listDict()
deleteWords(c("价比高","比高", "段时间", "hellip", "hellip;", "打游戏", "玩游戏", "电脑配置","一般般"，
              "个人感觉", "重装系统", "小键盘", "笔记本电脑"))
deleteWords(c("惠普", "DELL", "戴尔", "HP"，"京东", "康柏"))
deleteWords(c("价格低", "价格便宜"，"价格合理", "价格比", "价格下降"))
deleteWords(c("速度快", "速度慢", "反映速度", "大屏幕", "屏幕显示"))
deleteWords(c("挺不错", "不错呀", "真不错"))
insertWords(c("给力", "好用", "3D", "吐槽", "翻新机"))
insertWords(c("win8","win7"))
segmentFileNames <- gsub('/.txt', '.02.txt', paste(commentPath, 'segment', commentFileNames, sep='/'))
proc <- lapply(1:length(product_ids), segment_to_file , comments=comments, fileNames=segmentFileNames)

#处理stopwords
stopwordfile <- "D:/xujian/project/R/text_mining/dic/ChineseStopWord1208.txt"
mystopwords<- unlist (read.table(stopwordfile, quote="", stringsAsFactors=F)) 

word_frequence <- function(comment, stopwordsCN){ 
  word_text <- comment$Text
  words <- unlist(segmentCN(word_text)) 
  word_freq <- table(words)
  #remove stopwords
  remainwords <- setdiff(names(word_freq), stopwordsCN) #集合相减
  word_freq <- word_freq[remainwords]
  
  word_freq <- sort(word_freq,decreasing=TRUE)
  word_freq <- data.frame(word = names(word_freq), freq = word_freq, row.names=paste(1:length(word_freq)), stringsAsFactors=F)
  word_freq$len<- nchar(word_freq$word)
  word_freq <- subset(word_freq, len>1)  
  return(word_freq[1:1000,])   
}

gc()
system.time(comments.freq <- lapply(comments, word_frequence, stopwords=mystopwords)) #elapsed = 51.77
comments.good.freq <- lapply(comments.good, word_frequence, stopwords=mystopwords)
comments.bad.freq <- lapply(comments.bad, word_frequence, stopwords=mystopwords)
comments.mediate.freq <- lapply(comments.mediate, word_frequence, stopwords=mystopwords)

#读入产品信息
products <- read.delim(productinfoFile, header=F, sep="|", quote="", stringsAsFactors=F)  
names(products) <- c("Product_ID", "Product_Name", "Price", "Brand")
brands <- unique(products$Brand)

brand_freq_to_excel <- function(brand, currentTime, products, comments,
                                comments.freq, comments.good.freq, comments.mediate.freq, comments.bad.freq){
  brand_product_ids <- as.character(products[products$Brand==brand,'Product_ID'])
  names(brand_product_ids) <- brand_product_ids
  product_freq <- sapply(brand_product_ids, function(product_id) dim(comments[[product_id]])[1])
  ord <- order(product_freq, decreasing = TRUE)
  brand_product_ids <- brand_product_ids[ord]
  product_freq <- product_freq[ord]
  
  word_freq_file <- paste(commentPath,'/freq/freq_', brand, '_',currentTime, '.xlsx', sep='')
  word_freq_xls <- loadWorkbook(word_freq_file, create=TRUE) 
  
  for (product_id in brand_product_ids){
    sheet_name <- paste(brand, product_id, product_freq[product_id],sep='_')
    createSheet(word_freq_xls,name=sheet_name) # 创建工作表
    writeWorksheet(word_freq_xls, data.frame(x1='all', x2='freq'), sheet_name, 
                   startRow=1,startCol=1, header=FALSE)
    writeWorksheet(word_freq_xls, data.frame(x1='good', x2='freq'), sheet_name, 
                   startRow=1,startCol=4, header=FALSE)
    writeWorksheet(word_freq_xls, data.frame(x1='mediate', x2='freq'), sheet_name, 
                   startRow=1,startCol=7, header=FALSE)
    writeWorksheet(word_freq_xls, data.frame(x1='bad', x2='freq'), sheet_name, 
                   startRow=1,startCol=10, header=FALSE)    
    writeWorksheet(word_freq_xls, comments.freq[[product_id]][,1:2], sheet_name, 
                   startRow=2,startCol=1, header=FALSE) # 写入数据    
    writeWorksheet(word_freq_xls, comments.good.freq[[product_id]][,1:2], sheet_name, 
                   startRow=2,startCol=4, header=FALSE) # 写入数据
    writeWorksheet(word_freq_xls, comments.mediate.freq[[product_id]][,1:2], sheet_name, 
                   startRow=2,startCol=7, header=FALSE) # 写入数据    
    writeWorksheet(word_freq_xls, comments.bad.freq[[product_id]][,1:2], sheet_name, 
                   startRow=2,startCol=10, header=FALSE) # 写入数据      
  }    
  saveWorkbook(word_freq_xls)       # 存入硬盘，直到此步方才有文档生成
  print(paste('Creat Work Frequence File:', word_freq_file))
}

currentTime <- as.character(Sys.time())
currentTime <- gsub('-', '', currentTime)
currentTime <- gsub(':', '', currentTime)
currentTime <- gsub(' ', '_', currentTime)
gc()
lapply(brands, brand_freq_to_excel, currentTime, products, comments,
       comments.freq, comments.good.freq, comments.mediate.freq, comments.bad.freq)

#卸载包
detach("package:Rwordseg", unload=TRUE)

#-------------------------------------------
#RSQLite
#http://www.inside-r.org/questions/append-column-sqlite-table-r-using-rsqlite
#-------------------------------------------
library(RSQLite)
library(sqldf)
con <- dbConnect(dbDriver("SQLite"), dbname="D:/xujian/repository/software/tool/sqliteadmin/db/test.db")
left <- data.frame(let = letters[rep(1:4, each = 5)], num = 1:20)
right <- data.frame(let = letters[rep(1:4, each = 5)], num = 21:40)
dbWriteTable(con, "left_table", left, row.names = F)
dbWriteTable(con, "right_table", right, row.names = F)
dbGetQuery(con, "CREATE TABLE merged_table (letters TEXT, left_num INTEGER, right_num INTEGER)")
dbGetQuery(con, "INSERT INTO merged_table SELECT * FROM left_table LEFT OUTER JOIN right_table USING (let)")

fun <- function(x) rowSums(x)
temp <- dbReadTable(con, "merged_table")
dbWriteTable(con, "merged_table_new", cbind(temp, fun(temp[, 2:3])))

#show the tables in db
dbListTables(con)
dbDisconnect(con)


library(RSQLite)
left <- data.frame(let = letters[rep(1:4, each = 5)], num = 1:20)
right <- data.frame(let = letters[rep(1:4, each = 5)], num = 21:40)
my.tempfile <- tempfile()
con.write <- dbConnect(dbDriver("SQLite"), dbname = my.tempfile)
con.read <- dbConnect(dbDriver("SQLite"), dbname = my.tempfile)
dbWriteTable(con.write, "left_table", left, row.names = F)
dbWriteTable(con.write, "right_table", right, row.names = F)
dbGetQuery(con.write, "CREATE TABLE merged_table (letters TEXT, left_num INTEGER, right_num INTEGER)")
dbGetQuery(con.write, "INSERT INTO merged_table SELECT * FROM left_table LEFT OUTER JOIN right_table USING (let)")
dbGetQuery(con.write, "ALTER TABLE merged_table ADD COLUMN sum INTEGER")
dbGetQuery(con.write, "ALTER TABLE merged_table ADD COLUMN mean INTEGER")

res <- dbSendQuery(con.read, "SELECT left_num, right_num FROM merged_table")
while (!dbHasCompleted(res)) {
  data.1 <- fetch(res)
  data.2 <- data.frame(rowSums(data.1), rowMeans(data.1))
  dbGetPreparedQuery(con.write, "INSERT INTO merged_table (sum, mean) VALUES (?, ?)", bind.data = data.2)
}
dbClearResult(res)

dbGetQuery(con.read, "SELECT * FROM merged_table LIMIT 5")
dbDisconnect(con.write)
dbDisconnect(con.read)

#-------------------------------------------
#sqldf
#http://www.inside-r.org/questions/append-column-sqlite-table-r-using-rsqlite
#-------------------------------------------
library(sqldf)
str(iris)
sqldf("select Sepal_Length,Sepal_Width, Species from iris where Species='virginica'")

df<-data.frame(a=Sys.Date() +1:5,b=1:5);df
s<-sprintf("select * from df where a>=%d",Sys.Date()+2)
sqldf(s)

#join
Abbr <- data.frame(Species = levels(iris$Species), Abbr = c("S", "Ve", "Vi"))
sqldf("select Abbr, avg(Sepal_Length) from iris natural join Abbr group by Species")
sqldf("select Abbr, avg(Sepal_Length) from iris join Abbr using(Species) group by Species")
sqldf("select Abbr, avg(Sepal_Length) from iris, Abbr where iris.Species = Abbr.Species group by iris.Species")

#dbfile <- paste(commentPath, '/db/comment.s3db',sep='')
#con <- dbConnect(dbDriver("SQLite"), dbname=dbfile)
# sql <- "CREATE TABLE word_freq_total (product_id Nvarchar(20), word NVARCHAR(20), freq INTEGER, word_length INTEGER)"
# dbGetQuery(con, sql)
# sql <- "CREATE TABLE word_freq_good (product_id Nvarchar(20), word NVARCHAR(20), freq INTEGER, word_length INTEGER)"
# dbGetQuery(con, sql)
# sql <- "CREATE TABLE word_freq_mediate (product_id Nvarchar(20), word NVARCHAR(20), freq INTEGER, word_length INTEGER)"
# dbGetQuery(con, sql)
# sql <- "CREATE TABLE word_freq_bad (product_id Nvarchar(20), word NVARCHAR(20), freq INTEGER, word_length INTEGER)"
# dbGetQuery(con, sql)
# 因为乱码问题，暂时不用SQLite
#dbDisconnect(con)

#================================================
#报纸销量服从Normal(100, 10)，问题：应该进货多少。
sellfun <- function(purchase=50, cost, price, return_price){
  sell <- round(rnorm(10000, 100, 10), 0)
  profit <- ifelse(sell>purchase, 
                   purchase*(price-cost), 
                   sell*(price-cost)+(purchase-sell)*(return_price-cost))
  return(mean(profit))
}

cost <- 0.5
price <- 0.7
return_price <- 0.1
x <- seq(50, 100, by=1)
y <- sapply(x, sellfun, cost=0.5,price=0.7, return_price=0.1)
plot(x, y, type='l')
max(y)
which.max(y)
x[which.max(y)]

#================================================
#用Shiny包快速搭建基于R的交互网页应用
#http://shiny.rstudio.com/tutorial/lesson1/#Go Further
#http://xccds1977.blogspot.com/2012/11/shinyr.html#more
library(shiny)
setwd("D:/xujian/project/RR/code/shiny/")
runExample("01_hello")

#一个是负责前端的ui.R，另一个是负责后端的server.R。
#将这两个代码文件存放到同一个文件夹下，例如我是放在在"d:/rspace/shinyapp"。
#最后在控制台下运行：
library(shiny)
runApp("D:/xujian/project/RR/code/shiny/first")
runApp("D:/xujian/project/RR/code/shiny/first", display.mode='showcase')

#more example
system.file("examples", package="shiny")
#runExample("01_hello") # a histogram
runExample("02_text" , display.mode='showcase') # tables and data frames
runExample("03_reactivity", display.mode='showcase') # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer

#Lessions
runApp("D:/xujian/project/RR/code/shiny/lesson1", display.mode='showcase')
runApp("D:/xujian/project/RR/code/shiny/lesson2", display.mode='showcase')
runApp("D:/xujian/project/RR/code/shiny/lesson3", display.mode='showcase')
runApp("D:/xujian/project/RR/code/shiny/lesson4", display.mode='showcase')

#lesson 5
library(maps)
source("lesson5/helpers.R")
counties <- readRDS("lesson5/data/counties.rds")
head(counties)
percent_map(counties$white, "darkgreen", "% white")

runApp("D:/xujian/project/RR/code/shiny/lesson5")
runApp("D:/xujian/project/RR/code/shiny/lesson6")
runApp("D:/xujian/project/RR/code/shiny/lesson7", display.mode='showcase')

#================================================

#电影爱好者的R函数:从豆瓣抓数据
library(RCurl)
library(XML)
movieScore <- function(x) {
  stopifnot(is.character(x))
  # 提交搜索豆瓣表单
  search <- getForm("http://movie.douban.com/subject_search", search_text = x)
  searchweb <- htmlParse(search)
  # 解析搜索结果页面
  resnodes <- getNodeSet(searchweb, "//div[@id='wrapper']//table[1]//a")
  if (is.null(resnodes)) 
    return(NULL) else resurl <- xmlGetAttr(resnodes[[1]], name = "href")
  # 得到影片页面后第二次解析
  resweb <- getURL(resurl, .encoding = "UTF-8")
  content <- htmlParse(resweb, encoding = "UTF-8")
  resnodes <- getNodeSet(content, "//div[@id='interest_sectl']//p[@class='rating_self clearfix']//strong")
  namenodes <- getNodeSet(content, "//div[@id='content']//h1//span")
  # 得到影片评分
  score <- xmlValue(resnodes[[1]])
  name <- xmlValue(namenodes[[1]])
  return(list(name = name, score = score))
}

#看看天机这部大烂片多少分
movieScore("天机")

#抓网页比较慢，豆瓣为人民群众着想提供了API，我们也可以使用API来调取分数，函数也比较简单。
library(RCurl)
library(XML)
library(RJSONIO)
movieScoreapi <- function(x) {
  api <- "https://api.douban.com/v2/movie/search?q={"
  url <- paste(api, x, "}", sep = "")
  res <- getURL(url)
  reslist <- fromJSON(res)
  name <- reslist$subjects[[1]]$title
  score <- reslist$subjects[[1]]$rating$average
  return(list(name = name, score = score))
}
movieScoreapi("僵尸世界大战")

#====================================================
#关联分析
#http://cos.name/2013/02/association-rules-with-r-and-sas/
##1）下载泰坦尼克数据

# setInternet2(TRUE)
# con <- url("http://www.rdatamining.com/data/titanic.raw.rdata")
# load(con)
# close(con) # url() always opens the connection
load("D:/xujian/project/RR/data/titanic.raw.rdata")

str(titanic.raw)

##2）关联分析
library(arules)
# find association rules with default settings
rules.all <- apriori(titanic.raw)
inspect(rules.all)

#3）只保留结果中包含生存变量的关联规则
# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8), 
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), 
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#4）去除冗余的规则
# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#5）结果的解释
#对于结果的解释，一定要慎重，千万不要盲目下结论。从下面的四条规则看，好像确实像电影中描述的那样：妇女和儿童优先。

#1 {Class=2nd, Age=Child}              => {Survived=Yes} 0.010904134  1.0000000 3.095640
#2 {Class=1st, Sex=Female}           => {Survived=Yes} 0.064061790  0.9724138 3.010243
#3 {Class=2nd, Sex=Female}          => {Survived=Yes} 0.042253521  0.8773585 2.715986
#4 {Class=Crew, Sex=Female}       => {Survived=Yes} 0.009086779  0.8695652 2.691861
#如果我们减小最小支持率和置信度的阈值，则能看到更多的真相。

rules <- apriori(titanic.raw, parameter = list(minlen=3, supp=0.002, conf=0.2), 
                 appearance = 
                   list(rhs=c("Survived=Yes"),                                    
                        lhs=c("Class=1st", "Class=2nd", "Class=3rd", 
                              "Age=Child", "Age=Adult"), 
                        default="none"), control = list(verbose=F)
)
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

# 据统计，头等舱男乘客的生还率比三等舱中儿童的生还率还稍高一点。
#美国新泽西州州立大学教授、著名社会学家戴维·波普诺研究后毫不客气地修改了曾使英国人颇感“安慰”的
#“社会规范”(妇女和儿童优先)：“在泰坦尼克号上实践的社会规范这样表述可能更准确一些：‘头等舱和二等舱的妇女和儿童优先’。”

#6）可视化
# visualize rules
library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules.all, method="grouped")
plot(rules, method="paracoord", control=list(reorder=TRUE))

#====================================================
#可视化的其他长效果
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
rules

## scatterplot
plot(rules)
## try: sel <- plot(rules, interactive=TRUE)

## Two-key plot is a scatterplot with shading = "order"
plot(rules, shading="order", control=list(main = "Two-key plot"))

## the following techniques work better with fewer rules
subrules <- rules[quality(rules)$confidence > 0.8]

## 2D matrix with shading
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))

## 3D matrix
plot(subrules, method="matrix3D", measure="lift")
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))

## matrix with two measures
plot(subrules, method="matrix", measure=c("lift", "confidence"))
plot(subrules, method="matrix", measure=c("lift", "confidence"), 
     control=list(reorder=TRUE))

## try: plot(subrules, method="matrix", measure="lift", interactive=TRUE, 
##    control=list(reorder=TRUE))

## grouped matrix plot
plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=30))
## try: sel <- plot(rules, method="grouped", interactive=TRUE)

## graphs only work with very few rules
subrules2 <- sample(rules, 10)
plot(subrules2, method="graph")
plot(subrules2, method="graph", 
     control=list(type="items"))
## try: plot(subrules2, method="graph", interactive=TRUE)
## try: plot(subrules2, method="graph", control=list(engine="graphviz", 
##		type="items"))


## parallel coordinates plot
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

## Doubledecker plot only works for a single rule
oneRule <- sample(rules, 1)
plot(oneRule, method="doubledecker", data = Groceries)

## use iplots (experimental)
## try: sel <- plot(rules, method="iplots", interactive=TRUE)


## for itemsets
itemsets <- eclat(Groceries, parameter = list(support = 0.02))
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))
plot(itemsets, method="graph")

#arules中的itemMatrix
set.seed(1234)

## Generate random data and coerce data to itemMatrix.
m <- matrix(as.integer(runif(100000)>0.8), ncol=20)
dimnames(m) <- list(NULL, paste("item", c(1:20), sep=""))
i <- as(m, "itemMatrix")

## Get the number of elements (rows) in the itemMatrix.
length(i)

## Get first 5 elements (rows) of the itemMatrix as list.
as(i[1:5], "list")

## Get first 5 elements (rows) of the itemMatrix as matrix.
as(i[1:5], "matrix")

## Get first 5 elements (rows) of the itemMatrix as sparse ngCMatrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed!
as(i[1:5], "ngCMatrix")

## create itemsets from itemMatrix  
is <- new("itemsets", items = i[1:3])
inspect(is)

## create rules (rhs and lhs cannot share items so I use 
## itemSetdiff here)
rules <- new("rules", lhs=itemSetdiff(i[4:6],i[1:3]), rhs=i[1:3])
inspect(rules)

#====================================================

#漏斗中水全部流出的时间
#
flow_t <- function(R, H, S, K=0.62, g=9.8){
  (2/5)*pi*(R^2)*sqrt(H)/(K*S*sqrt(2*g))
}
flow_t(0.1/sqrt(3), 0.1, 0.00005)

#====================================================
#R绘制中国地图 http://cos.name/2014/08/r-maps-for-china/#more-10181
library(maptools)
setwd("D:/xujian/project/RR/code/maps_data/")

#1 地图GIS数据的来源与R绘制软件包
#全国范围的区域
mydat = readShapePoly("maps/bou1/bou1_4p.shp")
plot(mydat)

#可以看出这样绘制的地图的形状有些扁平。这是因为，在绘图的过程中，默认把经度和纬度作为普通数据，
#均匀平等对待，绘制在笛卡尔坐标系上造成的。其实，地球的球面图形如何映射到平面图上，在地理学上
#是有一系列不同的专业算法的。地图不应该画在普通的笛卡尔坐标系上，而是要画在地理学专业的坐标系
#上。在这一点上，R的ggplot2包提供了专门的coord_map()函数。所以推荐R的ggplot2包来绘制地图。
library(ggplot2)
mymap = ggplot(data = fortify(mydat)) +
  geom_polygon(aes(x = long, y = lat, group = id), colour = "black", fill = NA) +
  theme_grey()
print(mymap + coord_map())

#ggplot2包的coord_map函数默认的映射类型是mercator。如果有其他需要，可以使用其他的映射类型来绘制地图，如：
mymap + coord_map(projection = "azequidistant")

#coord_map函数的映射类型及其含义可以通过下列代码查询帮助，一般我们用默认的就可以。
library(mapproj)
?mapproject

#2 GIS地图的数据结构及省市地图的绘制
library(maptools)
mydat = readShapePoly("maps/bou2/bou2_4p.shp")
length(mydat)
table(iconv(mydat$NAME))  #table(iconv(mydat$NAME, from = "GBK"))

Shanghai = mydat[mydat$ADCODE99 == 310000,]
plot(Shanghai)

head(fortify(Shanghai))

#3 在地图上展示流行病学数据
Sys.setlocale(locale='Chinese');
mydat = readShapePoly("maps/bou4/BOUNT_poly.shp")
tmp = iconv(mydat$NAME99)
grep("长沙", tmp, value = TRUE)
grep("长沙", tmp)
mydat$ADCODE99[grep("长沙", tmp)]

#长沙地图
Changsha = mydat[substr(as.character(mydat$ADCODE99), 1, 4) == "4301",]
mysh = fortify(Changsha, region = 'NAME99')
mysh = transform(mysh, id = iconv(id, from = 'GBK'), group = iconv(group, from = 'GBK'))
head(mysh, n = 2)


#http://mindhacks.cn/2008/09/21/the-magical-bayesian-method/ 
#朴素贝叶斯分类算法是一种常用的分类方法，应用非常广泛，譬如垃圾邮件判断，电子商务反作弊（作弊卖家等等）。
# 数据集来自Tom Mitchell's book "Machine Learning".
#定义数据矩阵matrix，matrix(vector, nrow=r, ncol=c, byrow=logical_value,  dimnames=list(char_vector_rownames, char_vector_colnames))
#nrow表示行数
#ncol表示列数
#byrow表示矩阵组织方式，按行或者按列
#dimnames表示行标识，列标识
data <- matrix(c("sunny","hot","high","weak","no",
                 "sunny","hot","high","strong","no",
                 "overcast","hot","high","weak","yes",
                 "rain","mild","high","weak","yes",
                 "rain","cool","normal","weak","yes",
                 "rain","cool","normal","strong","no",
                 "overcast","cool","normal","strong","yes",
                 "sunny","mild","high","weak","no",
                 "sunny","cool","normal","weak","yes",
                 "rain","mild","normal","weak","yes",
                 "sunny","mild","normal","strong","yes",
                 "overcast","mild","high","strong","yes",
                 "overcast","hot","normal","weak","yes",
                 "rain","mild","high","strong","no"), byrow = TRUE,
               dimnames = list(day = c(),
                               condition = c("outlook","temperature",
                                             "humidity","wind","playtennis")), nrow=14, ncol=5);

#统计yes，no出现的概率                 
prior.yes = sum(data[,5] == "yes") / length(data[,5]);
prior.no = sum(data[,5] == "no") / length(data[,5]);
#输入条件向量
###################################################
naive.bayes.prediction <- function(condition.vec) {
  ###################################################
  # Calculate unnormlized posterior probability for playtennis = yes.
  playtennis.yes <-
    sum((data[,1] == condition.vec[1]) & (data[,5] == "yes")) / sum(data[,5] == "yes") * # P(outlook = f_1 | playtennis = yes)
    sum((data[,2] == condition.vec[2]) & (data[,5] == "yes")) / sum(data[,5] == "yes") * # P(temperature = f_2 | playtennis = yes)
    sum((data[,3] == condition.vec[3]) & (data[,5] == "yes")) / sum(data[,5] == "yes") * # P(humidity = f_3 | playtennis = yes)
    sum((data[,4] == condition.vec[4]) & (data[,5] == "yes")) / sum(data[,5] == "yes") * # P(wind = f_4 | playtennis = yes)
    prior.yes; # P(playtennis = yes)
  # Calculate unnormlized posterior probability for playtennis = no.
  playtennis.no <-
    sum((data[,1] == condition.vec[1]) & (data[,5] == "no")) / sum(data[,5] == "no") * # P(outlook = f_1 | playtennis = no)
    sum((data[,2] == condition.vec[2]) & (data[,5] == "no")) / sum(data[,5] == "no") * # P(temperature = f_2 | playtennis = no)
    sum((data[,3] == condition.vec[3]) & (data[,5] == "no")) / sum(data[,5] == "no") * # P(humidity = f_3 | playtennis = no)
    sum((data[,4] == condition.vec[4]) & (data[,5] == "no")) / sum(data[,5] == "no") * # P(wind = f_4 | playtennis = no)
    prior.no; # P(playtennis = no)
  return(list(post.pr.yes = playtennis.yes,
              post.pr.no = playtennis.no,
              prediction = ifelse(playtennis.yes >=
                                    playtennis.no, "yes", "no")));
}
naive.bayes.prediction(c("rain","hot","high","strong"));
naive.bayes.prediction(c("sunny","mild","normal","weak"));
naive.bayes.prediction(c("overcast","mild","normal","weak"));

执行后，输出结果：
> naive.bayes.prediction(c("rain","hot","high","strong"));

#R Training Exercise 1
#朴素naiveBayes
set.seed(1234)
newsdf <- read.delim("D:/xiazai/newsdf.txt", header=TRUE, encoding="utf-8")
newsdf[,2] <- as.factor(newsdf[,2])
testindices <- sample(c(1:length(newsdf[,2])),500)
trainindices <- c(1:length(newsdf[,2]))
trainindices <- trainindices[-testindices]

discretiztion <- function(newsdf)
{
  #Discretization
  for(i in c(3:length(newsdf))){
    tempvec <- newsdf[[i]]
    tempvec[tempvec>=1] <- 1
    tempvec[tempvec<1] <- 0
    newsdf[[i]] <- tempvec
  }
  #transform to Factor type
  for(i in c(3:length(newsdf))){
    newsdf[[i]] <- as.factor(newsdf[[i]])
  }  
  newsdf
}

convert_factor <- function(newsdf)
{
  #transform to Factor type
  for(i in c(3:length(newsdf))){
    newsdf[[i]] <- as.factor(newsdf[[i]])
  }  
  newsdf
}

#evaluation
accCal <- function(res1, res2){
  evares <- table(res1, res2)
  sum <- 0
  for(i in c(1:5)){
    sum <- sum + evares[i,i]
  }
  return(sum/5)
}

#Adaboosting
library(adabag)
newsdf_1 <- discretiztion(newsdf)
trainindices1 <- sample(trainindices, 500)
newsnbm <- boosting(Label~.,data=newsdf_1[-testindices,-1], boos=FALSE, mfinal=5)
newsnbres <- predict(newsnbm, newsdf_1[testindices,-1])
table(newsnbres$class, newsdf_1[testindices,2])
accCal(newsnbres$class, newsdf_1[testindices,2])


#NB
library(e1071)
newsdf_1 <- discretiztion(newsdf)
newsnbm <- naiveBayes(Label~.,data=newsdf_1[-testindices,-1])
summary(newsnbm)
newsnbm$apriori
head(newsnbm$tables)

newsnbres <- predict(newsnbm, newsdf_1[testindices,-1])
table(newsnbres, newsdf_1[testindices,2])
accCal(newsnbres, newsdf_1[testindices,2])

#svm
newssvmm <- svm(Label~.,data=newsdf_1[-testindices,-1], type="nu-classification")
summary(newssvmm)
newssvmm$apriori
head(newssvmm$tables)

newssvmres <- predict(newssvmm, newsdf_1[testindices,-1])
table(newssvmres, newsdf_1[testindices,2])
accCal(newssvmres, newsdf_1[testindices,2])

newssvmm <- svm(Label~.,data=newsdf[-testindices,-1], type="nu-classification")
summary(newssvmm)
newssvmm$apriori
head(newssvmm$tables)

newssvmres <- predict(newssvmm, newsdf[testindices,-1])
table(newssvmres, newsdf[testindices,2])
accCal(newssvmres, newsdf[testindices,2])

#使用KNN
library(class)
knnres <- knn(newsdf_1[-testindices,c(-1,-2)], newsdf_1[testindices,c(-1,-2)], newsdf_1[-testindices,2], k=5)
table(knnres, newsdf_1[testindices,2])
accCal(knnres, newsdf_1[testindices,2])

#Random Forest
library(randomForest)
rf <- randomForest(Label ~ ., data=newsdf[-testindices,c(-1)], ntree=100, proximity=TRUE)
table(predict(rf), newsdf[-testindices,c(-1)]$Label)

print(rf)
attributes(rf)
plot(rf)
importance(rf)
varImpPlot(rf)


#协同过滤
#movie recommendation
library(recommenderlab)
data(MovieLense)
attributes(MovieLense)
evaScheme <- evaluationScheme(MovieLense, method="split", train=0.8, given = 15, goodRating=4)


algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "item-based CF" = list(name="IBCF", param=list(method="cosine")),
  "item-based CF" = list(name="IBCF", param=list(method="jaccard")),
  "item-based CF" = list(name="IBCF", param=list(method="pearson")),
  "user-based CF" = list(name="UBCF", param=list(method="cosine")),
  "user-based CF" = list(name="UBCF", param=list(method="jaccard")),
  "user-based CF" = list(name="UBCF", param=list(method="pearson"))
)
results <- evaluate(evaScheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
plot(results, annotate=TRUE)


#Regression
#
setwd('D:/xiazai/HPTraining')
#user interest prediction
ticDF <- read.delim("ticdata2000.txt", header = FALSE, sep = '\t')
length(ticDF)
#head(ticDF)
ticTestT <- read.delim("tictgts2000.txt", header = FALSE, sep = '\t')
ticTestFV <- read.delim("ticeval2000.txt", header = FALSE, sep = '\t')

evalTopN <- function(sysPred, goldAnsw, topN){
  tempresult <- sysPred
  temp <- sort(tempresult)
  tempresult[tempresult >= temp[4000-topN+1]] <- 1
  tempresult[tempresult < temp[4000-topN+1]] <- 0
  table(tempresult, goldAnsw)
}

lmmodel <- lm(V86~., data=ticDF)
summary(lmmodel)
lmres <- predict(lmmodel, ticTestFV)
evalTopN(lmres, ticTestT[,1], 800)

lmmodel <- glm(V86~., family=binomial(link=logit), data=ticDF)
summary(lmmodel)
lmres <- predict(lmmodel, ticTestFV)
evalTopN(lmres, ticTestT[,1], 800)

library(e1071)
ticDF[,86] <- as.factor(ticDF[,86])
nbmodel <- naiveBayes(V86~.,data=ticDF)
result <- predict(nbmodel, ticTestFV, type = c("raw") )
evalTopN(result[,2], ticTestT[,1], 800)


#马尔科夫
library(msm)
setwd('D:/xiazai/HPTraining')
namecharsdf <- read.delim("nameCharlist.txt", header=FALSE)
charIDVec <- namecharsdf[,2]
names(charIDVec) <- namecharsdf[,1]
pnDF <- read.delim("nameSeq.id.txt", header=TRUE, sep='\t')
length(charIDVec)

spnDF <- pnDF[1:5000,]
nstate <- length(unique(spnDF[,2]))
initprobs <- rep(0.01, nstate*nstate)
pnqmatrix <- matrix(initprobs, nrow=nstate,ncol=nstate)
sttable <- statetable.msm(Char, PNum, data=spnDF)
initqm <- crudeinits.msm(Char ~ Position, PNum, data=spnDF, qmatrix=pnqmatrix)

#initprobs <- rep(0.01,2535*2535)
#pnqmatrix <- matrix(initprobs, nrow=2535,ncol=2535)
#pn.msm <- msm(Char~Position, subject=PNum, data=pnDF, gen.inits =TRUE, qmatrix=pnqmatrix)

computeProb <- function(newName){
  newNameIDs <- c(1:length(newName))
  for(i in c(1:length(newName))){
    newNameIDs[i] <- charIDVec[newName[i]]
  }
  probs <- c(2:length(newName))
  for(i in c(2:length(newName))){
    probs[i-1] <- initqm[newNameIDs[i-1],newNameIDs[i]]
  }
  mean(probs)
}
newName <- c('S', '王', '化', '强', 'E')
newName <- c('S', '化', '强', 'E')
newName <- c('S', '强', 'E')
newName <- c('S', '喻', '刚', 'E')
newName <- c('S', '徐', '湘', '雯', 'E')
computeProb(newName)

#从最长公共子串开始
#http://mephisto.unige.ch/pub/TraMineR/doc/1.4/TraMineR-1.4-Users-Guide.pdf

library(TraMineR)
#2.1 State sequence analysis
data(mvad)
dim(mvad)
mvad[1:3,17:86]

mvad.labels <- c("employment", "further education", "higher education", "joblessness", "school", "training")
mvad.scode <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, states = mvad.scode, labels = mvad.labels)
mvad.seq[1:3,]
alphabet(mvad.seq)
seqiplot(mvad.seq, withlegend = F, title = "Index plot (10 first sequences)", border = NA)
seqfplot(mvad.seq, withlegend = F, border = NA, title = "Sequence frequency plot")
seqdplot(mvad.seq, withlegend = F, border = NA, title = "State distribution plot")
seqlegend(mvad.seq, fontsize = 1.3)
seqHtplot(mvad.seq, title = "Entropy index")
Turbulence <- seqST(mvad.seq)
summary(Turbulence)
hist(Turbulence, col = "cyan", main = "Sequence turbulence")

#Compute the optimal matching distances using substitution costs based on transition rates
observed in the data and a 1 indel cost
submat <- seqsubm(mvad.seq, method = "TRATE")
dist.om1 <- seqdist(mvad.seq, method = "OM", indel = 1,sm = submat)
dist.om1[1:6, 1:6] 

library(cluster)
clusterward1 <- agnes(dist.om1, diss = TRUE, method = "ward")
plot(clusterward1)
cl1.4 <- cutree(clusterward1, k = 4)
cl1.4fac <- factor(cl1.4, labels = paste("Type", 1:4))
#Plot the state distribution at each time point within each cluster
seqdplot(mvad.seq, group = cl1.4fac, border = NA)
#Plot the sequence frequencies within each cluster
seqfplot(mvad.seq, group = cl1.4fac, border = NA)

#2.2 Event sequence analysis
mvad.seqe <- seqecreate(mvad.seq)
fsubseq <- seqefsub(mvad.seqe, pMinSupport = 0.05)
plot(fsubseq[1:15], col = "cyan")
discr <- seqecmpgroup(fsubseq, group = cl1.4fac)
plot(discr[1:6])

#3.2.1 The actcal data set
data(actcal)
names(actcal)
actcal[1:3,]

#3.2.2 The biofam data set
data(biofam)
names(biofam)
biofam[1:3,]

#3.2.3 The mvad data set
data(mvad)
names(mvad)
mvad[1:3,]

#3.2.4 Other data sets borrowed from the literature
data(famform)
names(famform)
famform[1:3,]
famform

#5.2.1 Converting between compressed and extended formats
actcal.comp <- seqconc(actcal, 13:24)
actcal.ext <- seqdecomp(actcal.comp)
head(actcal.ext)

#6.1 Creating a state sequence object
data(actcal)
actcal.seq <- seqdef(actcal, var = 13:24)
is.data.frame(actcal.seq)
dim(actcal.seq)
names(actcal)
names(actcal.seq)

actcal.seq <- seqdef(actcal, var = c("jan00", "feb00", "mar00",
                                     "apr00", "may00", "jun00", "jul00", "aug00", "sep00", "oct00",
                                     "nov00", "dec00"))

#SPS formatted sequences
seq1 <- "(000,12)-(0W0,9)-(0WU,5)-(1WU,2)"
seq2 <- "(000,12)-(0W0,14)-(1WU,2)"
seq.ex1 <- rbind(seq1, seq2)
seq.ex1 <- seqdef(seq.ex1, informat = "SPS")
seq.ex1[, 10:25]
print(seq.ex1, format = "SPS")

#6.2.2 Alphabet
actcal.s1 <- seqdef(actcal[1:3, 13:24])
alphabet(actcal.s1)

#Transition rates
data(actcal)
actcal.seq <- seqdef(actcal, var = 13:24)
tr <- seqtrate(actcal.seq)
round(tr, 2)

#6.2.3 Color palette
actcal.seq <- seqdef(actcal, 13:24, cpal = c("red", "blue", "green", "yellow"))
attr(actcal.seq, "cpal") <- c("pink", "purple", "cyan", "yellow")
attr(actcal.seq, "cpal") <- brewer.pal(4, "Dark2")

#6.4 Indexing and printing sequence objects
actcal.seq[1:5, 3:8]
print(actcal.seq[1:5, 3:8], ext = TRUE)
print(actcal.seq[1:5, 3:8], format = "SPS")
actcal.summer <- actcal.seq[, 6:9]
attr(actcal.summer, "cpal")
attr(actcal.summer, "labels")
attr(actcal.summer, "start")
names(actcal.summer)

#6.5 Truncations, gaps and missing values
data(famform)
famform
seqdecomp(famform)

#7.1.2 Plotting the legend separately
par(mfrow = c(2, 2))
seqiplot(biofam.seq, title = "Index plot (first 10 sequences)", withlegend = FALSE)
seqdplot(biofam.seq, title = "State distribution plot", withlegend = FALSE)
seqfplot(biofam.seq, title = "Sequence frequency plot", withlegend = FALSE, pbarw = TRUE)
seqlegend(biofam.seq)

data(actcal)
actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
actcal.seq <- seqdef(actcal, 13:24, labels = actcal.lab)
alphabet(actcal.seq)
seqdplot(actcal.seq)

data(mvad)
mvad.labels <- c("employment", "further education", "higher education", "joblessness", "school", "training")
mvad.seq <- seqdef(mvad, 15:86, labels = mvad.labels)
seqdplot(mvad.seq)

seqstatd(biofam.seq)
sd <- seqstatd(biofam.seq)
seqHtplot(biofam.seq)

#7.2.4 Transition rates
tr <- seqtrate(actcal.seq)
round(tr, 2)

s.def <- seqdef(c('1-2-1-1', '2-3-2-3', '3-2-2-1', '1-2-3-1'))
seqiplot(s.def)
s.tr <- seqtrate(s.def);s.tr

#Number of matching positions
data(famform)
famform.seq <- seqdef(famform)
famform.seq
seqmpos(famform.seq[1, ], famform.seq[2, ])
seqmpos(famform.seq[2, ], famform.seq[4, ])
seqmpos(famform.seq[5, ], famform.seq[3, ])
seqmpos(famform.seq[3, ], famform.seq[4, ])

#Longest Common Prex (LCP) distances
famform.seq
seqLLCP(famform.seq[1, ], famform.seq[2, ])
seqLLCP(famform.seq[3, ], famform.seq[4, ])
seqLLCP(famform.seq[3, ], famform.seq[5, ])

#Computing LCP distances
seqdist(famform.seq, method = "LCP")
seqdist(famform.seq, method = "LCP", norm = TRUE)

#Longest Common Subsequence (LCS) distances
LCS.ex <- seqdef(c("S-U-S-M-S-U", "U-S-SC-MC", "S-U-M-S-SC-UC-MC"))
x <- LCS.ex[1, ]
y <- LCS.ex[2, ]
z <- LCS.ex[3, ]
seqLLCS(x, y)
seqLLCS(x, z)
LCS.lcs <- seqdist(LCS.ex, method = "LCS")
LCS.lcs

#Missing Value
ex2.seq <- seqdef(c("A-B-C-D", "A-B-*-C-D", "A-B-*-C-D-A"))
seqLLCS(ex2.seq[1,], ex2.seq[2,])
seqLLCS(ex2.seq[2,], ex2.seq[3,])
ex2.seq.lcs <- seqdist(ex2.seq, method = "LCS", with.missing = TRUE)  #it doesn't work
ex2.seq.lcs    

#Optimal matching (OM) distances
data(biofam)
biofam.seq <- seqdef(biofam, var=paste('a', 15:30, sep=''))
alphabet(biofam.seq )
couts <- seqsubm(biofam.seq, method = "TRATE")
biofam.seq[1:2,]
round(couts, 2)

s.def <- seqdef(c('1-2-1-1', '2-3-2-3', '3-2-2-1', '1-2-3-1'))
seqiplot(s.def)
s.tr <- seqtrate(s.def);s.tr
s.couts <- seqsubm(s.def , method = "TRATE")
round(s.couts , 2)
range(s.couts)

s.ccost <- seqsubm(s.def, method = "CONSTANT", cval = 2);s.ccost
s.OM <- seqdist(s.def, method = "OM", sm = s.couts);s.OM
s.LCS <- seqdist(s.def, method = "LCS");s.LCS


#understand om
ex3.seq <- seqdef(c('A-B-C-D', 'A-B-B-D', 'A-B-C-D-D', 'A-B-C-D'))
seqiplot(ex3.seq)
ex3.tr <- seqtrate(ex3.seq);ex3.tr
ex3.couts <- seqsubm(ex3.seq , method = "TRATE")
round(ex3.couts , 2)
range(ex3.couts)

ex3.ccost <- seqsubm(ex3.seq, method = "CONSTANT", cval = 2); ex3.ccost
ex3.OM <- seqdist(ex3.seq, method = "OM", sm = ex3.ccost); ex3.OM
ex3.OM <- seqdist(ex3.seq, method = "OM", sm = ex3.couts); ex3.OM
ex3.LCS <- seqdist(ex3.seq, method = "LCS")
all.equal(ex3.OM, ex3.LCS)

biofam.ccost <- seqsubm(biofam.seq, method = "CONSTANT", cval = 2);
biofam.OM <- seqdist(biofam.seq, method = "OM", sm = biofam.ccost); biofam.OM[1:10,1:10]
biofam.LCS <- seqdist(biofam.seq, method = "LCS"); biofam.LCS[1:10,1:10]
all.equal(biofam.OM, biofam.LCS)

library(qualV)


#线性回归

#蒲丰投针问题
# p(a) <- 2*l/pi/a
#l为针的长度，a为线的间距, line_num是平行线个数
pBuffon <- function(n, l, a, line_num=101){  
  x <- runif(n, 0, line_num-1) 
  theta <- runif(n, 0, pi)
  dis <- abs(round(x)-x) * a
  m <- sum(l*sin(theta)/2 >= dis)
  m/n  
}

l <- 8; a <- 10
pBuffon(1000000, l, a)
2*l/pi/a

#################################
#BreakoutDetection is an open-source R package that makes breakout detection simple and fast.
#install.packages("devtools")
#devtools::install_github("twitter/BreakoutDetection")
#################################
library(BreakoutDetection) 
data(Scribe)
res = breakout(Scribe, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot

#层次抽样
#Stratified sampling
library(sampling)
############
## Example 1
############
# Example from An and Watts (New SAS procedures for Analysis of Sample Survey Data)
# generates artificial data (a 235X3 matrix with 3 columns: state, region, income).
# the variable "state" has 2 categories ('nc' and 'sc'). 
# the variable "region" has 3 categories (1, 2 and 3).
# the sampling frame is stratified by region within state.
# the income variable is randomly generated
data=rbind(matrix(rep("nc",165),165,1,byrow=TRUE),matrix(rep("sc",70),70,1,byrow=TRUE))
data=cbind.data.frame(data,c(rep(1,100), rep(2,50), rep(3,15), rep(1,30),rep(2,40)),
                      1000*runif(235))
names(data)=c("state","region","income")
# computes the population stratum sizes
table(data$region,data$state)
# not run
#     nc  sc
#  1 100  30
#  2  50  40
#  3  15   0
# there are 5 cells with non-zero values
# one draws 5 samples (1 sample in each stratum)
# the sample stratum sizes are 10,5,10,4,6, respectively
# the method is 'srswor' (equal probability, without replacement)
s=strata(data,c("region","state"),size=c(10,5,10,4,6), method="srswor")
# extracts the observed data
getdata(data,s)
# see the result using a contigency table
table(s$region,s$state)
############
## Example 2
############
# The same data as in Example 1
# the method is 'systematic' (unequal probability, without replacement)
# the selection probabilities are computed using the variable 'income'
s=strata(data,c("region","state"),size=c(10,5,10,4,6), method="systematic",pik=data$income)
# extracts the observed data
getdata(data,s)
# see the result using a contigency table
table(s$region,s$state)
############
## Example 3
############
# Uses the 'swissmunicipalities' data as population for drawing a sample of units
data(swissmunicipalities)
# the variable 'REG' has 7 categories in the population
# it is used as stratification variable
# Computes the population stratum sizes
table(swissmunicipalities$REG)
# do not run
#  1   2   3   4   5   6   7 
# 589 913 321 171 471 186 245 
# sort the data to obtain the same order of the regions in the sample
data=swissmunicipalities
data=data[order(data$REG),]
# the sample stratum sizes are given by size=c(30,20,45,15,20,11,44)
# 30 units are drawn in the first stratum, 20 in the second one, etc.
# the method is simple random sampling without replacement 
# (equal probability, without replacement)
st=strata(data,stratanames=c("REG"),size=c(30,20,45,15,20,11,44), method="srswor")
# extracts the observed data
getdata(data, st)
# see the result using a contingency table
table(st$REG)



#
# View data:
#   elecj_test_PAD_Fail_flag_OCT
# 
# R code:
#   ##1.load data 
setwd("C:/pm/Analysis")
data_oct <- read.csv("elecj_test_PAD_Fail_flag_OCT.csv",header=T)
#

##2.summary of data
summary(data_oct)
plot(density(data_oct$Final_Flag_Fail))
table(data_oct$Final_Flag_Fail)

##4.sampling of data by Final_Flag_Fail
install.packages("sampling")
library(sampling)
install.packages("stratification")
library(stratification)

seed(1234)
m <- dim(data_oct)[1]  #获取数据集记录条数  
val <- sample(m, size =round(m/3), replace = FALSE, prob= rep(1/m, m))  #抽样，选取三分之二的数据作为训练集。 
data_oct$Final_Flag_Fail <- as.factor(data_oct$Final_Flag_Fail)
model.learn <- data_oct[-val,]  #选取训练集    
model.valid <- data_oct[val,]   #选取验证集


#5.install logistic regression package
library(AER)

fit.full <- glm(Final_Flag_Fail ~ CSIO+
                  DATA_A_minus+
                  DATA_A_plus+
                  MCLK_plus+
                  MCLK_minus+
                  nRESET+
                  VDD_N+
                  VPP_LOGIC+
                  VPP_N+
                  VPP_S,
                data =  model.learn,
                family = binomial(link = 'logit'))

summary(fit.full)
table(model.valid$Final_Flag_Fail, predict(fit.full, data=model.valid))

#####################################
#googleVis
#####################################
library(googleVis)
library(dplyr)
setwd('D:/xujian/project/RR/data')
gdp <- read.delim2('GDP.txt', header=T)
gdp$GDP <- as.numeric(as.character(gdp$GDP))
gdp <- select(gdp, EconomyCode, Ranking, Economy, GDP)
colnames(gdp) <- c("EconomyCode", "Ranking", "country", "value")
gdp$value2=runif(192,min=2000000,max=6000000)
gdp$per=runif(192,min=.4,max=.9)
names(gdp)
dim(gdp)
str(gdp)

bar=gvisBarChart(gdp[1:5,],'country','value')
plot(bar)

combo=gvisComboChart(gdp[1:5,], xvar="country",
                     yvar=c("value2", "value"),
                     options=list(seriesType="bars",
                                  title="ComboChart",
                                  series='{0: {type:"line"}}'))
plot(combo)

pie=gvisPieChart(gdp[1:6,c('country','value')])
plot(pie)

geo=gvisGeoChart(gdp, locationvar="country", colorvar="value")
plot(geo)
geo2=gvisGeoChart(gdp, locationvar="country", sizevar="value",options = list(displayMode="markers"))
plot(geo2)

#sampling 分层抽样
library(sampling)
#0     1 
#89599   774
#stratification , 70% as Training, 30% as Validation
#size: sample data volumn of every classification
data(iris)
sample1<-strata(iris,stratanames="Species",size=c(10,2,3),method="srswor",description=TRUE)
training<-getdata(z2_z3,sample1)

#################################problem is here?????????????????????????
v<-getdata(z2_z3,-sample1)

data=rbind(matrix(rep("nc",165),165,1,byrow=TRUE),matrix(rep("sc",70),70,1,byrow=TRUE))
data=cbind.data.frame(data,c(rep(1,100), rep(2,50), rep(3,15), rep(1,30),rep(2,40)),
                      1000*runif(235))
names(data)=c("state","region","income")
# computes the population stratum sizes
table(data$region,data$state)
s=strata(data,c("region","state"),size=c(10,5,10,4,6), method="srswor")

table(data$region)
s=strata(data,c("region"),size=c(10,10,10), method="srswor")
d <-getdata(data,s);dim(d)
s=strata(data,c("region"),size=c(15,20,15), method="srswor")
d <-getdata(data,s);dim(d)
d <- data[-s$ID_unit,]

data(swissmunicipalities)
table(swissmunicipalities$REG)
data=swissmunicipalities
data=data[order(data$REG),]
st=strata(data,stratanames=c("REG"),size=c(30,20,45,15,20,11,44), method="srswor")
a<-getdata(data, st)

#######################################
# bigmemory
#######################################
A <- matrix(data = 0, 5000, 5000); format(object.size(A), units = "MB");
x <- sample(1:5000, size = 5000, replace = TRUE); 
y <- sample(1:5000, size = 5000, replace = TRUE); 
for (i in 1:5000) {
  A[x[i], y[i]] <- 1
}

library(bigmemory)
library(biganalytics)
options(bigmemory.typecast.warning = FALSE)
B <- big.matrix(5000, 5000, init = 0); format(object.size(B), units = "MB");
for (i in 1:5000) {
  B[x[i], y[i]] <- 1
}
B
typeof(B)
class(B)
lsos()

#内存共享
desc <- describe(B)
dput(desc, file = "B.big.matrix")
colsum(B, 1:10)

desc <-dget(file = "B.big.matrix")
C <- attach.big.matrix(desc)
colsum(C, 1:10)
desc

#文件
library(bigmemory)
library(biganalytics)
D <- filebacked.big.matrix(5000, 5000, init = 0,
                           backingfile = "matrix.example",
                           descriptorfile = "matrix.example.desc")
D
dim(D)
list.files()

#######################################
# ff
#######################################
library(ff)
ff.obj1 <- ff(vmode = "double", length = 100)
## "ff2dde53acb8c9.ff" in "/tmp/RtmpZ5qpC2"
R.obj1 <- double(100)
ff.obj2 <- ff(vmode = "double", length = 10000)
R.obj2 <- double(10000)
lsos()

ffVector <- ff(0:1, length=36e6) # 0,1,0,… 4 byte integers
ffVector
ffMatrix <- ff(vmode="logical", dim=c(6e3,6e3)) # 2 bit logical
ffMatrix
ffPOSIXct <- ff(Sys.time(), length=36e6) # 8 byte double
ffPOSIXct
bases <- c("A","T","G","C")
ffFactor <- ff("A", levels=bases, length=400e6 # 2 bit quad
               , vmode="quad", filename="QuadFactorDemo.ff", overwrite=TRUE)
# 95 MB with quad instead of 1.5 GB with integer
ffFactor
# accessing parts based on memory mapping and OS file caching
ffFactor[3:400e6] <- c("A","T") # quick recycling at no RAM
ffFactor[1:12]
close(ffFactor)

#FF CLASS STRUCTURE WITH HYBRID COPYING SEMANTICS
x <- ff(1:12, dim=c(3,4))
str(x)

#SPECIAL COPY SEMANTICS: PARTIAL SHARING
a <- ff(1:12)
b <- a
dim(b) <- c(3,4)
a[] <- a[] + 1
a;b
b[3,4]='99'
a;b

x <- ff(1:26, names=letters)
y <- x
vw(x) <- c(0, 13, 13)
vw(y) <- c(13, 13, 0)
x
y
x[1] <- -1
y[1] <- -2
vw(x) <- NULL
x[]

z <- ff(1:24, dim=c(4,6), dimnames=list(letters[1:4], LETTERS[1:6]))
z
vw(z) <- rbind(c(1,1), c(2,4), c(1,1))
z
rm(x,y,z); gc()
#
#WHILE R‘s RAM STORAGE IS ALWAYS IN COLUMN-MAJOR ORDER,
FF ARRAYS CAN BE STORED IN ARBITRARY DIMORDER …
x <- ff(1:12
        , dim=c(3,4)
        , dimorder=c(1,2)
)
x[]
x[1:12]
read.ff(x, 1, 12)
x <- ff(1:12
        , dim=c(3,4)
        , dimorder=c(2,1)
)
x[]
x[1:12]
read.ff(x, 1, 12)

#WHILE R‘s RAM STORAGE IS ALWAYS IN COLUMN-MAJOR ORDER,
#WHICH SOMETIMES CAN HELP SPEEDING UP
n <- 100
m <- 100000
a <- ff(1L,dim=c(n,m))
b <- ff(1L,dim=c(n,m), dimorder=2:1)
system.time(lapply(1:n, function(i)sum(a[i,])))
system.time(lapply(1:n, function(i)sum(b[i,])))
system.time(lapply(1:n, function(i){i<-(i-1)*(m/n)+1;
                                    sum(a[,i:(i+m/n-1)])}))
system.time(lapply(1:n, function(i){i<-(i-1)*(m/n)+1;
                                    sum(b[,i:(i+m/n-1)])}))


########################################
#广义线性模型
#Logistic回归
########################################
#--------------------------------------------------#
# R in Action: Chapter 13                          #
# requires that the AER, robust, and qcc packages  #
#    have been installed                           #
# install.packages(c('AER', 'robust', 'qcc'))      #
#--------------------------------------------------#

# --Logistic Regression--
library(AER)
# get summary statistics
data(Affairs, package = "AER")
summary(Affairs)
table(Affairs$affairs)

# create binary outcome variable
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, levels = c(0,1), labels = c("No", "Yes"))
table(Affairs$ynaffair)

# fit full model
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + 
                  children + religiousness + education + occupation + rating, 
                data = Affairs, family = binomial())
summary(fit.full)

# fit reduced model
fit.reduced <- glm(ynaffair ~ age + yearsmarried + 
                     religiousness + rating, data = Affairs, family = binomial())
summary(fit.reduced)

# compare models
anova(fit.reduced, fit.full, test = "Chisq")

# interpret coefficients
coef(fit.reduced)
exp(coef(fit.reduced))
exp(confint(fit.reduced))  #获得系数的置信区间

# calculate probability of extramariatal affair by marital ratings
testdata <- data.frame(rating = c(1, 2, 3, 4, 5), 
                       age = mean(Affairs$age), yearsmarried = mean(Affairs$yearsmarried), 
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata = testdata, 
                         type = "response")
testdata

# calculate probabilites of extramariatal affair by age
testdata <- data.frame(rating = mean(Affairs$rating), 
                       age = seq(17, 57, 10), yearsmarried = mean(Affairs$yearsmarried), 
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata = testdata, 
                         type = "response")
testdata

# evaluate overdispersion
fit <- glm(ynaffair ~ age + yearsmarried + religiousness + 
             rating, family = binomial(), data = Affairs)
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                rating, family = quasibinomial(), data = Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual, 
       fit$df.residual, lower = F)

# --Poisson Regression--
# look at dataset
data(breslow.dat, package = "robust")
names(breslow.dat)
summary(breslow.dat[c(6, 7, 8, 10)])

# plot distribution of post-treatment seizure counts
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
attach(breslow.dat)
hist(sumY, breaks = 20, xlab = "Seizure Count", main = "Distribution of Seizures")
boxplot(sumY ~ Trt, xlab = "Treatment", main = "Group Comparisons")
par(opar)

# fit regression
fit <- glm(sumY ~ Base + Age + Trt, data = breslow.dat, 
           family = poisson())
summary(fit)

# interpret model parameters
coef(fit)
exp(coef(fit))

# evaluate overdispersion
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type = "poisson")

# fit model with quasipoisson
fit.od <- glm(sumY ~ Base + Age + Trt, data = breslow.dat, 
              family = quasipoisson())
summary(fit.od)



#层次抽样
#Stratified sampling
library(sampling)
library(dplyr)
data=rbind(matrix(rep("nc",165),165,1,byrow=TRUE),matrix(rep("sc",70),70,1,byrow=TRUE))
data=cbind.data.frame(data, c(rep(1,100), rep(2,50), rep(3,15), rep(1,30),rep(2,40)),
                      1000*runif(235))
names(data)=c("state", "region", "income")
# computes the population stratum sizes
table(data$region,data$state)

cvsample <- function(data, k){
  set.seed(1234)
  cols <- colnames(data)
  data$no <-  1:nrow(data)
  data_sam <- data[, c(cols, 'no')]
  ksam <- data.frame()
  s <- data.frame()
  for(i in 1:k){
    if (i>1)  data_sam <- data_sam[-s$ID_unit, c(cols, 'no')]
    size <- round(table(data_sam[,cols])/(k-i+1))
    cat('No.', i, ' sampling\n', sep = "") 
    print(table(data_sam[,cols]))
    print(size)
    s <- sampling::strata(data_sam, cols, size=t(size), method="srswor") 
    s$i <- i
    s$no <- data_sam[s$ID_unit, 'no']
    ksam <- rbind(ksam, s)
  }
  select(ksam, -c(ID_unit, Prob, Stratum))
}

t(table(data$state))
data1 <- data %>% select(state) %>% arrange(state)
d <- cvsample(data1, 3)
table(d$i, d$state)

t(table(data$region))
data1 <- data %>% select(region) %>% arrange(region)
d <- cvsample(data1, 3)
table(d$i, d$region)

table(data$state, data$region)
data1 <- data %>% select(state, region) %>% arrange(state, region)
d <- cvsample(data=data1, k=10)
table(d$i);table(d$i, d$state);table(d$i, d$region)
table(d$state, d$region, d$i)

table(data$region, data$state)
data1 <- data %>% select(region, state) %>% arrange(region, state)
d <- cvsample(data=data1,  k=2)
table(d$i);table(d$i, d$region);table(d$i, d$state)
table(d$region, d$state, d$i)


load("D:/xujian/project/RR/data/titanic.raw.rdata")
head(titanic.raw)

table(titanic.raw$Class)
data1 <- titanic.raw %>% select(Class) %>% arrange(Class)
d <- cvsample(data=data1, k=2)
table(d$i);table(d$i, d$Class);
table(d$Class,  d$i)
d<-strata(titanic.raw, c('Class'), size=c(50,25,15,20), method="srswor")
table(d$Class)
table(titanic.raw$Class)

table(titanic.raw$Class, titanic.raw$Sex)
data1 <- titanic.raw %>% select(Class, Sex) %>% arrange(Class, Sex)
d <- cvsample(data1,  20)
table(d$i);table(d$i, d$Class);table(d$i, d$Sex)
table(d$Class, d$Sex, d$i)

s=strata(data,c("state"),size=c(165,70), method="srswor")
table(data$state)
d<-strata(data, c('state','region'), size=c(50,15,15,20,8), method="srswor")
d<-strata(data, c('state','region'), size=c(10,10,15,10,6,0), method="srswor");
table(d$state, d$region)
table(data$state, data$region)
d<-strata(data, c('region','state'), size=c(5,6,7,9,6,0), method="srswor");
table(d$region, d$state)
table(data$region, data$state)

#heatmap
library(dplyr)
#mn.hm <- mn %>% group_by(DIE_SITE_NR, NOZRESLK_KY) %>% summarise(cnt = n())

library(ggplot2)
library(sqldf)
library(dplyr)
mn <- read.csv('D:/xujian/project/pm/data/missing_nozzle.csv', header=T, sep=",", stringsAsFactors=T, strip.white=T)
mn.hm <- sqldf("select DIE_SITE_NR,  NOZRESLK_KY, count(1) cnt from mn group by  DIE_SITE_NR,  NOZRESLK_KY")
dim(mn.hm)
mn.hm <- sqldf("select DIE_SITE_NR, PATTERNCOLORLK_KY, NOZRESLK_KY, count(1) cnt from mn group by  DIE_SITE_NR, PATTERNCOLORLK_KY, NOZRESLK_KY")
mn.hm$DIE_SITE_NR <- as.character(mn.hm$DIE_SITE_NR)
dim(mn.hm)

drawHM <- function(data){
  g <- ggplot(data, aes(x=DIE_SITE_NR, y=NOZRESLK_KY, fill=cnt))+xlab('X-labels')+ylab("Y-labels")
  g +geom_tile() + scale_fill_gradient(low='green', high='red')
}
drawHM(mn.hm %>% filter(PATTERNCOLORLK_KY==1))
drawHM(mn.hm %>% filter(PATTERNCOLORLK_KY==5))
drawHM(mn.hm %>% filter(PATTERNCOLORLK_KY==7))
drawHM(mn.hm %>% filter(PATTERNCOLORLK_KY==9))

summary(mn.hm %>% filter(PATTERNCOLORLK_KY==1) %>% select(NOZRESLK_KY))
summary(mn.hm %>% filter(PATTERNCOLORLK_KY==5) %>% select(NOZRESLK_KY))
summary(mn.hm %>% filter(PATTERNCOLORLK_KY==7) %>% select(NOZRESLK_KY))
summary(mn.hm %>% filter(PATTERNCOLORLK_KY==9) %>% select(NOZRESLK_KY))

summary(mn.hm$NOZRESLK_KY)
length(unique(mn.hm$NOZRESLK_KY))

###########################################################
#比较不同聚类的效果
library(seriation)
library(cluster)
library(fpc)
iris.dist <- dist(iris[,1:4])
#GMM 高斯混合模型 聚类
library(mclust)
mc <-  Mclust(iris[,1:4], 3)
plot(mc, data=iris[,1:4], what="classification",dimens=c(3,4))
table(iris$Species, mc$classification)
sil <- silhouette(mc$classification, iris.dist)
summary(sil)
plot(sil)

#hc
library(dplyr)
hc.cluster <- function(hc, k){
  a <- rect.hclust(hc, k=k)
  a <- lapply(1:length(a), function(x) data.frame(i=a[[x]], cluster=x))
  a <- do.call("rbind", a)
  a <- a %>% arrange(i)
  a
}

hc <- hclust(iris.dist, method="ward.D")
hc.c <- hc.cluster(hc, k=3)
table(iris$Species, hc.c$cluster)
sil <- silhouette(hc.c$cluster, iris.dist)
summary(sil)
plot(sil)

#clustering 聚类(Derek)
library(fpc)
library(cluster)
plot(ruspini)
ruspini <- ruspini[sample(1:nrow(ruspini)),]
ruspini <- scale(ruspini)
db<-dbscan(ruspini,eps=.5,MinPts=5)
db
str(db)
plot(ruspini, col=db$cluster+1L)
d<-dist(ruspini)
km <- kmeans(ruspini, centers=4, nstart=10)
sil.km <- silhouette(km$cluster, d)
a<-summary(sil.km)
str(a)
a$si.summary 
plot(sil.km)

#GMM
library(mclust)
mc <-  Mclust(ruspini, 4)
plot(ruspini, col=mc$classification+1L)
sil.gmm <- silhouette(mc$classification, dist(ruspini))
summary(sil.gmm)
plot(sil.gmm)

library(seriation)
pimage(d, colorkey=TRUE)
pimage(d, order=order(km$cluster), colorkey=TRUE)
dissplot(d, labels=km$cluster, options=list(main="k-means with k=4"))
dissplot(d, labels=db$cluster+1L, options=list(main="DBSCAN"))
dissplot(d, labels=kmeans(ruspini, centers=4)$cluster)
dissplot(d, labels=kmeans(ruspini, centers=9)$cluster)
dissplot(d)

#rJava有的时候不得不kill后台的进程，然后执行下面的代码。
#Sys.setenv(JAVA_HOME='/var/aa/jdk1.7.0_72/jre') 
options(java.parameters="-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

library(RJDBC)

#SQL Server
server <- "jdbc:sqlserver://s79w0193.sgp.hp.com:2048;DatabaseName=pwax_rt"
driver <- "/opt/mount1/aa/r/driver/sqljdbc_4.1/enu/sqljdbc41.jar"
driver <- "/opt/mount1/aa/r/driver/sqljdbc4.jar"

getQuery <- function(sql){
  drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", driver, "`")
  conn <- dbConnect(drv, server,  "PWAXRT_ADMIN", "LineMonitor123")
  data <- dbGetQuery(conn, sql)  
  dbDisconnect(conn)
  data  
}

getQuery("select * from dbo.buffer")

#get the data from https://app.wmcloud.com/open/dashboard?lang=zh
require(RCurl)
httpheader=c("Authorization"="6d27a153716f85713eb12f7e5396b19b90174b3ebf9700d29a5c6cfb4ea6414a");
getdata=function(url){
  http=paste("https://api.wmcloud.com:443/data",url,sep = "")
  return (getURL(http, httpheader=httpheader, ssl.verifypeer = FALSE,.encoding="utf8"))
}
result=getdata("/api/master/getSecID.json?field=&assetClass=&ticker=000001,600000&partyID=&cnSpell=")
cat(result)
cat(result,file="Rout.csv")

#移动平均
x <- 1:10
k <- 3
fun <- mean
y <- apply( embed( x, k ), 1, fun )

x = read.table(textConnection('
                               X Y
                               a 10
                               a 20
                               a 30
                               a 40
                               a 50
                               b 100
                               b 200
                               b 300
                               b 400
                               b 500
                               c 10
                               c 20
                               c 30'), header = TRUE)
cbind(x, Z = as.vector(
  t(
    unlist(aggregate(Y ~ X, x, filter, rep(1/3, 3), sides = 1)[, -1])
    )
  )
)

xx=as.data.frame(xx)
library(plyr)
ddply(xx,.(X),transform,z = as.vector(filter(Y,rep(1/3,3),sides=1)))

#移动平均
library(fTrading)
x = (1:10)^2
x
trim = c(TRUE, TRUE, FALSE, FALSE)
na.rm = c(TRUE, FALSE, TRUE, FALSE)
rollFun(x, 3, trim=FALSE, na.rm=FALSE, FUN = mean)
for (i in 1:4)
  rollFun(x, 5, trim[i], na.rm[i], FUN = min)
for (i in 1:4)
  rollFun(x, 5, trim[i], na.rm[i], FUN = max)
for (i in 1:4)
  rollVar(x, 5, trim[i], unbiased = TRUE, na.rm[i])
for (i in 1:4)
  rollVar(x, 5, trim[i], unbiased = FALSE, na.rm[i])


#气泡图
x <- c("A","B","C","D")
y <- c("a","b","c","d")
dat <- expand.grid(x=x,y=y)
dat$r <- as.numeric(dat$y)**4

library(ggplot2)
# create ggplot object
g<-ggplot(dat, aes(x=x,y=y,size=r))
# add some 'geom' layers
g+geom_point(shape=21)+
  geom_text(size=4,aes(label=r))+
  # scale the size of the point by area
  scale_size_area(max_size=30,guide="none")+
  theme_bw()

smhy <- c(ws1=4.13*4.2+1.33*1.1, ws2=3.2*4.2, ws3=3.2*4.4, 
          wsj1=2.3*2.2, wsj2=2.2*2.2+1.1*0.6 , 
          cf=2.3*3.4, ct=2.82*3.7, ccs=2.3*1.15/2,  
          kt=4.3*4.4+1.2*4.55+1.2*2.3-1.4*0.7+1*3.4, 
          yt1=1.3*4.3, yt2=1*2.82, pc=1.8);
smhy;sum(smhy);sum(smhy)/131.52

smhy <- c(ws1=4.13*4.2+1.33*1.1, ws2=3.2*4.2, ws3=3.2*4.4, 
          wsj1=2.3*2.2, wsj2=2.2*2.2+1.1*0.6 , 
          cf=2.3*3.4, ct=2.82*3.7, ccs=2.3*1.15/2,  
          kt=4.3*4.4+1.2*4.55+1.2*2.3-1.4*0.7+1*3.4, 
          yt1=1.3*4.3, yt2=1*2.82, pc=1.8);
smhy;sum(smhy);sum(smhy)/131.52

rlen <- function(raw, rate=11.8/6.99){
  rate*raw
}

rlens <- function(raw){
  newlen <- sapply(raw, rlen)
  print(newlen)
}

house <- function() {
  ws1 <- rlens(c(2.17, 2.46, 0.57,0.57))
  ws2 <- rlens(c(1.82, 2.05, 0.57,0.49))  
  ws3 <- rlens(c(1.8, 2.00)) 
  wsj1 <- rlens(c(1.15, 1.65)) 
  wsj2 <- rlens(c(1.15, 1.21, 0.4, 0.45)) 
  cf <- rlens(c(1.06, 1.91)) 
  ccs <- rlens(c(1.06, 0.6)) 
  ct <- rlens(c(1.5, 2.12))
  kt <- rlens(c(2.35, 2.56, 4.3, 0.68, 0.58, 1.4)) 
  yt1 <- rlens(c(2.44, 0.85)) 
  yt2 <- rlens(c(1.49, 0.68)) 
  pc <- rlens(c(3.9, 0.4)) 
  smhy <- c(ws1=ws1[1]*ws1[2]+ws1[3]*ws1[4], ws2=ws2[1]*ws2[2]+ws2[3]*ws2[4], ws3=ws3[1]*ws3[2], 
            wsj1=wsj1[1]*wsj2[2],wsj2=wsj2[1]*wsj2[2]+wsj2[3]*wsj2[4] , 
            cf=cf[1]*cf[2], ct=ct[1]*ct[2], ccs=ccs[1]*ccs[2],  
            kt=kt[1]*kt[2]+kt[3]*kt[4]+kt[5]*kt[6], 
            yt1=yt1[1]*yt1[2], yt2=yt2[1]*yt2[2], pc=pc[1]*pc[2]/2);
  print(smhy);
  print(sum(smhy));
  print(sum(smhy)/131.52)
}

house <- function() {
  ws1 <- rlens(c(2.17, 2.48, 0.57,0.50))
  ws2 <- rlens(c(1.8, 2.08, 0.65,0.51))  
  ws3 <- rlens(c(1.8, 2.00)) 
  wsj1 <- rlens(c(1.15, 1.85)) 
  wsj2 <- rlens(c(1.15, 1.21, 0.49, 0.49)) 
  cf <- rlens(c(1.09, 1.9)) 
  ccs <- rlens(c(1.09, 0.6)) 
  ct <- rlens(c(1.49, 2.16))
  kt <- rlens(c(2.38, 2.6, 4.3, 0.68, 0.58, 1.4)) 
  yt1 <- rlens(c(2.4, 0.8)) 
  yt2 <- rlens(c(1.49, 0.68)) 
  pc <- rlens(c(3.9, 0.4)) 
  smhy <- c(ws1=ws1[1]*ws1[2]+ws1[3]*ws1[4], ws2=ws2[1]*ws2[2]+ws2[3]*ws2[4], ws3=ws3[1]*ws3[2], 
            wsj1=wsj1[1]*wsj2[2],wsj2=wsj2[1]*wsj2[2]+wsj2[3]*wsj2[4] , 
            cf=cf[1]*cf[2], ct=ct[1]*ct[2], ccs=ccs[1]*ccs[2],  
            kt=kt[1]*kt[2]+kt[3]*kt[4]+kt[5]*kt[6], 
            yt1=yt1[1]*yt1[2]/2, yt2=yt2[1]*yt2[2]/2, pc=pc[1]*pc[2]/2);
  print(sum(smhy[c('ws1','ws2','ws3','ct','kt')]))
  print(sum(smhy[c('ws1','ws2','ws3','ct','kt','yt1','yt2')]))
  print(smhy);
  print(sum(smhy));
  print(sum(smhy)/131.52)
}


rlen(1.8)
