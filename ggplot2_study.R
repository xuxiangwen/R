library(ggplot2)

#ggplot中的图
ls("package:ggplot2", pattern="^geom_.+") #几何类型
ls("package:ggplot2", pattern="^stat_.+") #统计类型

set.seed(100)
d.sub <- diamonds[sample(nrow(diamonds), 500),]
p <- ggplot(d.sub, aes(x=carat, y=price))
theme_set(theme_bw())
p + stat_identity()
p + geom_point()

#geom_text
p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))

p + geom_text()
# Change size of the label
p + geom_text(size=10)
p <- p + geom_point()

# Set aesthetics to fixed value
p + geom_text()
p + geom_point() + geom_text(hjust=0, vjust=0)
p + geom_point() + geom_text(angle = 45)

# Add aesthetic mappings
p + geom_text(aes(colour=factor(cyl)))
p + geom_text(aes(colour=factor(cyl))) + scale_colour_discrete(l=40)

p + geom_text(aes(size=wt))
p + geom_text(aes(size=wt)) + scale_size(range=c(3,6))

# You can display expressions by setting parse = TRUE.  The
# details of the display are described in ?plotmath, but note that
# geom_text uses strings, not expressions.
p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
              parse = TRUE)

# Add an annotation not from a variable source
c <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
c + geom_text(data = NULL, x = 5, y = 30, label = "plot mpg vs. wt")
# Or, you can use annotate
c + annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")

# Use qplot instead
qplot(wt, mpg, data = mtcars, label = rownames(mtcars),
      geom=c("point", "text"))
qplot(wt, mpg, data = mtcars, label = rownames(mtcars), size = wt) +
  geom_text(colour = "red")

# You can specify family, fontface and lineheight
p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
p + geom_text(fontface=3)
p + geom_text(aes(fontface=am+1))
p + geom_text(aes(family=c("serif", "mono")[am+1]))



#
#data.frame(x=c(x1,x2,x3), y=rep(y,3), type=rep(LETTERS[1:3], each=length(y)))
p

#蝴蝶图
#theta data.frame(x=radius*sin(theta), y=radius*cos(theta))
ggplot(dd, aes(x, y))+geom_path()+theme_null()+xlab("")+ylab("")


#https://github.com/cosname/ggplot2-translation/tree/master/Rcode

#qplot
set.seed(1400)
dsmall <- diamonds[sample(nrow(diamonds),100),]


qplot(carat, price, data=diamonds)
qplot(log(carat), log(price), data=diamonds) #看上去有些线性关系了
qplot(carat, x*y*z, data=diamonds)  #克拉和长宽高有线性关系，但也有异常点

#添加点的颜色和形状
qplot(carat, price, data=dsmall, colour = color)
qplot(carat, price, data=dsmall, shape = cut)
qplot(carat, price, data=dsmall, shape = cut, colour = color)

#半透明
qplot(carat, price, data=diamonds, alpha = I(1/10))
qplot(carat, price, data=diamonds, alpha = I(1/100))
qplot(carat, price, data=diamonds, alpha = I(1/200))

#geom控制图形的类型
qplot(carat, price, data=dsmall, geom=c('point','smooth'))
qplot(carat, price, data=diamonds, geom=c('point','smooth'))

#如果n比较小〈1000，method=loess
qplot(carat, price, data=dsmall, geom=c('point','smooth'), span=0.2)
qplot(carat, price, data=dsmall, geom=c('point','smooth'), span=1)

#也可调用mgcv
library(mgcv)
qplot(carat, price, data=dsmall, geom=c('point','smooth'), method='gam', formula = y~s(x))
qplot(carat, price, data=dsmall, geom=c('point','smooth'), method='gam', formula = y~s(x, bs='cs'))

#箱线图和扰动点图
qplot(color, price/carat, data=diamonds, geom=c('boxplot'), alpha=I(1/5))
qplot(color, price/carat, data=diamonds, geom=c('boxplot'), alpha=I(1/50))
qplot(color, price/carat, data=diamonds, geom=c('boxplot'), alpha=I(1/200))

qplot(color, price/carat, data=diamonds, geom=c('jitter'), alpha=I(1/5))
qplot(color, price/carat, data=diamonds, geom=c('jitter'), alpha=I(1/50))
qplot(color, price/carat, data=diamonds, geom=c('jitter'), alpha=I(1/200))

#直方图和密度曲线图
qplot(carat, data=diamonds, geom='histogram', colour=color)
qplot(carat, data=diamonds, geom='density', fill=color)

qplot(carat, data=diamonds, geom='histogram', binwidth=1, xlim=c(0,3))
qplot(carat, data=diamonds, geom='histogram', binwidth=0.1, xlim=c(0,3))
qplot(carat, data=diamonds, geom='histogram', binwidth=0.01, xlim=c(0,3))

#条形图（柱状图）
qplot(color, data=diamonds, geom='bar')
qplot(color, data=diamonds, geom='bar', weight=carat) + scale_y_continuous('carat')

#时间序列的线条图和路径图
str(economics)
head(economics)

qplot(date, unemploy/pop, data=economics, geom='line')
qplot(date, unemploy, data=economics, geom='line')

year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy/pop, unemploy, data=economics, geom=c('point', 'line'))
qplot(unemploy/pop, unemploy, data=economics, geom=c('path'), colour=year(date))

#分面
qplot(carat, data=diamonds, facets=color~., geom='histogram', binwidth=0.1, xlim=c(0,3))
qplot(carat, ..density.., data=diamonds, facets=color~., geom='histogram', binwidth=0.1, xlim=c(0,3))

#其他选项
qplot(carat, price, data=dsmall, xlab='Price($)', ylab='Weight(Carats)', main='Price-Weight Relationship')
qplot(carat, price/carat, data=dsmall, xlab=expression(frac(price, carat)), ylab='Weight(Carats)', 
      main='Small Diamands', xlim=c(0.2,1))
qplot(carat, price, data=dsmall, log='xy')

#分析mpg数据（1999，2008年汽车的厂商，幸好，类别，引擎大小，。。。）
#分析引擎大小和耗油量的关系
#displ：发动机排量（升）
#hwy: 告诉公路耗油量（英里每加仑）
#cyl: 气缸树木
p <- qplot(displ, hwy, data=mpg, colour=factor(cyl))
summary(p)
print(p)
save(p, file='plot.rdata')
load('plot.rdata')
ggsave('plot.png', width=5, height=5)

#chapter 4
library(ggplot2)
## 通过ggplot创建图形对象
p <- ggplot(diamonds, aes(carat, price, colour = cut))
## 添加“点”几何对象
p <- p + layer(geom = "point")
p

## 例：手动创建图形对象并添加图层
p <- ggplot(diamonds, aes(x = carat))
p <- p + layer(geom = "bar", geom_params = list(fill = "steelblue"), stat = "bin", 
               stat_params = list(binwidth = 0.2))
p

## 应用“快捷函数”，得到与上例相同的图形
p + geom_histogram(binwidth = 0.2, fill = "steelblue") +

## 在用ggplot创建的图形对象上添加图层
ggplot(msleep, aes(sleep_rem/sleep_total, awake)) + geom_point()
# 等价于
qplot(sleep_rem/sleep_total, awake, data = msleep)

# 也可以给qplot添加图层
qplot(sleep_rem/sleep_total, awake, data = msleep) + geom_smooth()
# 等价于
qplot(sleep_rem/sleep_total, awake, data = msleep, geom = c("point", "smooth"))
# 或
ggplot(msleep, aes(sleep_rem/sleep_total, awake)) + geom_point() + geom_smooth()

## 例：summary给出图形对象的默认设置和每个图层的信息
p <- ggplot(msleep, aes(sleep_rem/sleep_total, awake))
summary(p)
p <- p + geom_point()
summary(p)

## 例：用不同的数据初始化后添加相同的图层
library(scales)
bestfit <- geom_smooth(method = "lm", se = F, colour = alpha("steelblue", 0.5), 
                       size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit

## 用%*%添加新的数据集来代替原来的数据集
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p
mtcars <- transform(mtcars, mpg = mpg^2)
p %+% mtcars

## aes函数的参数
aes(x = weight, y = height, colour = age)
# 也可以使用变量的函数值作为参数
aes(weight, height, colour = sqrt(age))

p <- ggplot(mtcars)
summary(p)

p <- p + aes(wt, hp)
summary(p)

## 使用默认的参数映射来添加图层
p <- ggplot(mtcars, aes(x = mpg, y = wt))
p + geom_point()

## 图4.1 修改图形属性。用factor(cyl)修改颜色(左)，用disp修改y坐标(右)。
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(y = disp))

p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour = "darkblue")
# 注意这里将颜色映射到'darkblue'与上面将颜色设定给'darkblue'的区别
p + geom_point(aes(colour = "darkblue"))

# The difference between (left) setting colour to \code{'darkblue'} and
# (right) mapping colour to \code{'darkblue'}.  When \code{'darkblue'}
# is mapped to colour, it is treated as a regular value and scaled with
# the default colour scale.  This results in pinkish points and a legend.

## 图4.2
## 将颜色设定为'darkblue'(左)与将颜色映射到'darkblue'(右)的区别。当颜色映
## 射到'darkblue'时，'darkblue'将被看作一个普通的字符串，使用默认的颜色标
## 度进行标度转换，结果得到了粉红色的点和图例。
qplot(mpg, wt, data = mtcars, colour = I("darkblue"))
qplot(mpg, wt, data = mtcars, colour = "darkblue")

## 图4.3 正确分组时(分组变量group =
## Subject)每个个体的折线图(左)。错误的分组时连
## 接所有观测点的折线图(右)。此处省略了分组图形属性，效果等同于group =
## 1。
data(Oxboys, package = "nlme")
# 左图的代码
p <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
p
# 或
qplot(age, height, data = Oxboys, group = Subject, geom = "line")
# 右图的代码
qplot(age, height, data = Oxboys, geom = "line")

## 图4.4
## 给Oxboys数据添加光滑曲线。左图用了和折线图同样的分组变量，得到了每个男
## 孩的拟合直线。右图在平滑层里用了aes(group = 1)，得到了所有男孩的拟合直
## 线。 左图
p + geom_smooth(aes(group = Subject), method = "lm", se = F)
# 或
qplot(age, height, data = Oxboys, group = Subject, geom = "line") + geom_smooth(method = "lm", 
                                                                                se = F)
# 右图
p + geom_smooth(aes(group = 1), method = "lm", size = 2, se = F)
# 或
qplot(age, height, data = Oxboys, group = Subject, geom = "line") + geom_smooth(aes(group = 1), 
                                                                                method = "lm", size = 2, se = F)


## 图4.5
## 如果想用箱线图来查看每个时期的身高分布，默认的分组是正确的(左图)。如果
## 想用\texttt{geom_line()}添加每个男孩的轨迹，就需要在新图层里设定
## aes(group = Subject)(右图)。 左图
qplot(Occasion, height, data = Oxboys, geom = "boxplot")
# 右图
qplot(Occasion, height, data = Oxboys, geom = "boxplot") + geom_line(aes(group = Subject), 
                                                                     colour = "#3366FF")
# 或
boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
boysbox + geom_line(aes(group = Subject), colour = "#3366FF")

## 图4.6
## 对于线条和路径，线段的图形属性是由起始点的图形属性决定的。如果颜色是离
## 散的(左图)，在相邻的颜色间插入其他颜色是没有任何意义的。如果颜色是连续
## 的(右图)，可以在相邻的颜色间进行插补，但默认条件下R不会这样做。
df <- data.frame(x = 1:3, y = 1:3, colour = c(1, 3, 5))
qplot(x, y, data = df, colour = factor(colour), size = I(5)) + geom_line(aes(group = 1), size = 2)
qplot(x, y, data = df, colour = colour, size = I(5)) + geom_line(size = 2)

## 用线性插值法做颜色渐变线条
xgrid <- with(df, seq(min(x), max(x), length = 50))
interp <- data.frame(x = xgrid, y = approx(df$x, df$y, xout = xgrid)$y, colour = approx(df$x, 
                                                                                        df$colour, xout = xgrid)$y)
qplot(x, y, data = df, colour = colour, size = I(5)) + geom_line(data = interp, size = 2)

## 图4.7 一个条形图(左)按组分解后得到的叠加条形图(右)，两者轮廓相同。
qplot(color, data = diamonds)
qplot(color, data = diamonds, fill = cut)

## 例：生成变量
ggplot(diamonds, aes(carat)) + geom_histogram(aes(y = ..density..), binwidth = 0.1)
# 或
qplot(carat, ..density.., data = diamonds, geom = "histogram", binwidth = 0.1)

## 图4.8 应用于条形图的三种位置调整。从左到右依次是:堆叠(stacking)，填充
## (filling)和并列(dodging)
dplot <- ggplot(diamonds, aes(clarity, fill = cut))
dplot + geom_bar(position = "stack")
dplot + geom_bar(position = "fill")
dplot + geom_bar(position = "dodge")

## 图4.9 同一调整(identity
## adjustment)不适用于条形图(左)，因为后画的条形会挡住先
## 画的条形。但它适用于线型图(右)，因为线条不存在相互遮掩的问题。
dplot + geom_bar(position = "identity")
qplot(clarity, data = diamonds, geom = "line", colour = cut, stat = "bin", group = cut)

## 图4.10 直方图的三种变体。频率多边形(frequency
## polygon)(左)；散点图，点的大小和
## 高度都映射给了频率(中)；热图(heatmap)用颜色来表示频率。
d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
d + stat_bin(aes(size = ..density..), binwidth = 0.1, geom = "point", position = "identity")
d + stat_bin(aes(y = 1, fill = ..count..), binwidth = 0.1, geom = "tile", position = "identity")

## 例：nlme包的Oxboys数据集
require(nlme, quiet = TRUE, warn.conflicts = FALSE)
model <- lme(height ~ age, data = Oxboys, random = ~1 + age | Subject)
oplot <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()

age_grid <- seq(-1, 1, length = 10)
subjects <- unique(Oxboys$Subject)

preds <- expand.grid(age = age_grid, Subject = subjects)
preds$height <- predict(model, preds)

oplot
oplot + geom_line(data = preds, colour = "#3366FF", size = 0.4)

Oxboys$fitted <- predict(model)
Oxboys$resid <- with(Oxboys, fitted - height)

oplot %+% Oxboys + aes(y = resid) + geom_smooth(aes(group = 1))

model2 <- update(model, height ~ age + I(age^2))
Oxboys$fitted2 <- predict(model2)
Oxboys$resid2 <- with(Oxboys, fitted2 - height)

oplot %+% Oxboys + aes(y = resid2) + geom_smooth(aes(group = 1)) 

#chapter 5
library(effects)
library(ggplot2)
## 图5.1
# 使用不同的基本几何对象绘制相同数据的效果。从左上到右下的图形名称分别为：散
# 点图、条形图、线图、面积图、路径图、含标签的散点图、色深图/水平图和多边形图。注意
# 观察条形图、面积图和瓦片图的坐标轴范围：这三种几何对象占据了数据本身范围以外的空
# 间，于是坐标轴被自动拉伸了。
df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a","b","c")
)
p <- ggplot(df, aes(x, y)) + xlab(NULL) + ylab(NULL)
p + geom_point() + labs(title = "geom_point")
p + geom_bar(stat="identity") +
  labs(title = "geom_bar(stat=\"identity\")")
p + geom_line() + labs(title = "geom_line")
p + geom_area() + labs(title = "geom_area")
p + geom_path() + labs(title = "geom_path")
p + geom_text(aes(label = label)) + labs(title = "geom_text")
p + geom_tile() + labs(title = "geom_tile")
p + geom_polygon() + labs(title = "geom_polygon")


## 图5.2
## 永远不要指望依靠默认的参数就能对某个具体的分布获得一个表现力强的图形
## (左图)。(右图) 对 x 轴进行了放大，xlim = c(55,
## 70)，并选取了一个更小的组距宽度， binwidth =
## 0.1，较左图揭示出了更多细节。我们可以发现这个分布是轻度右偏的。同时别
## 忘了在标题中写上重要参数 (如组距宽度) 的信息。
qplot(depth, data = diamonds, geom = "histogram")
qplot(depth, data = diamonds, geom = "histogram", xlim = c(55, 70), binwidth = 0.1)


## 图5.3
## 钻石数据切割和深度分布的三种视图。从上至下分别是分面直方图、条件密度图和频
## 率多边形图。它们都显示了出一个有趣的模式：随着钻石质量的提高，分布逐渐向左偏移且
## 愈发对称。
depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68)
depth_dist + geom_histogram(aes(y = ..density..), binwidth = 0.1) + facet_grid(cut ~.)
depth_dist + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill")
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut), binwidth = 0.1)


## 图5.4 箱线图可以用于观察针对一个类别型变量 (如 cut) 取条件时
## (左图)，或连续型变量 (如 carat) 取条件时
## (右图)，连续型变量的分布情况。对于连续型变量，必须设置 group 图
## 形属性以得到多个箱线图。此处使用了group = round_any(carat, 0.1, floor)
## 来获得针 对变量carat 以 0.1 个单位为大小分箱后的箱线图。
library(plyr)
qplot(cut, depth, data = diamonds, geom = "boxplot")
qplot(carat, depth, data = diamonds, geom = "boxplot", 
      group = round_any(carat, 0.1, floor), xlim = c(0, 3))


## 图5.5 几何对象 jitter
## 可在二维分布中有一个离散型变量时绘制出一个较为粗略的图
## 形。总体来说，这种数据打散的处理对小数据集更有效。上图展示了 mpg
## 数据集中离散型变 量 class 和连续型变量 city，下图则将连续型变量 city
## 替换为离散型变量 drv。
qplot(class, cty, data = mpg, geom = "jitter")
qplot(class, drv, data = mpg, geom = "jitter")


## 图5.6
## 密度图实际上就是直方图的平滑化版本。它的理论性质比较理想，但难以由图回溯到
## 数据本身。左图为变量 depth 的密度图。右图为按照变量 cut
## 的不同取值上色的版本。
qplot(depth, data = diamonds, geom = "density", xlim = c(54, 70))
qplot(depth, data = diamonds, geom = "density", xlim = c(54, 70), fill = cut, 
      alpha = I(0.2))


## 图5.7
## 修改使用的符号可以帮助我们处理轻微到中等程度的过度绘制问题。从左至右分别
## 为：默认的 shape、shape = 1(中空的点)，以及 shape= '.'(像素大小的点)。
df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(df, aes(x, y))
norm + geom_point()
norm + geom_point(shape = 1)
norm + geom_point(shape = ".")  # 点的大小为像素级


## 图5.8 以从一个二元正态数据中抽样所得的数据为例，使用 alpha
## 混合来减轻过度绘制问题。alpha 值从左至右分别为：1/3, 1/5, 1/10。
norm + geom_point(colour = "black", alpha = 1/3)
norm + geom_point(colour = "black", alpha = 1/5)
norm + geom_point(colour = "black", alpha = 1/10)


## 图5.9 diamond 数据中的变量table 和变量 depth
## 组成的图形，展示了如何使用数据打 散和 alpha
## 混合来减轻离散型数据中的过度绘制问题。从左至右为：不加任何处理的点，使用
## 默认扰动参数打散后的点，横向扰动参数为 0:5 (横轴单位距离的一半)
## 时打散后的点，alpha 值 1/10，alpha 值 1/50，alpha 值 1/200。
td <- ggplot(diamonds, aes(table, depth)) + xlim(50, 70) + ylim(50, 70)
td + geom_point()
td + geom_jitter()
jit <- position_jitter(width = 0.5)
td + geom_jitter(position = jit)
td + geom_jitter(position = jit, colour = "black", alpha = 1/10)
td + geom_jitter(position = jit, colour = "black", alpha = 1/50)
td + geom_jitter(position = jit, colour = "black", alpha = 1/200)


## 图5.10
## 第一行使用正方形显示分箱，第二行使用六边形显示。左栏使用默认分箱参数，中
## 栏使用参数bins = 10，右栏使用参数binwidth = c(0.02,
## 200)。为了节约空间，均略去 了图例。
d <- ggplot(diamonds, aes(carat, price)) + xlim(1, 3) + theme(legend.position = "none")
d + stat_bin2d()
d + stat_bin2d(bins = 10)
d + stat_bin2d(binwidth = c(0.02, 200))
library(hexbin)
d + stat_binhex()
d + stat_binhex(bins = 10)
d + stat_binhex(binwidth = c(0.02, 200))


## 图5.11
## 使用密度估计对点的密度建模并进行可视化。上图为基于点和等值线的密度展示，
## 下图为基于色深的密度展示。
d <- ggplot(diamonds, aes(carat, price)) + xlim(1, 3) + theme(legend.position = "none")
d + geom_point() + geom_density2d()
d + stat_density2d(geom = "point", aes(size = ..density..), contour = F) + 
  scale_size_area()
d + stat_density2d(geom = "tile", aes(fill = ..density..), contour = F)
last_plot() + scale_fill_gradient(limits = c(1e-05, 8e-04))


## 图5.12 函数 borders() 的使用实例。左图展示了美国 (2006 年 1 月)
## 五十万人口以上的城 市，右图为德克萨斯州的城市区划。
library(maps)
data(us.cities)
big_cities <- subset(us.cities, pop > 5e+05)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5)
tx_cities <- subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) + borders("county", "texas", colour = "grey70") + 
  geom_point(colour = "black", alpha = 0.5)


## 图5.13
## 左侧的等值域图展示了各州人身伤害案件的数量，右侧的等值域图展示了人身伤害
## 和谋杀类案件的比率。
library(maps)
states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, by = "region")
# 由于绘制多边形时涉及顺序问题 且merge破坏了原始排序 故将行重新排序
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, fill = assault, geom = "polygon")
qplot(long, lat, data = choro, group = group, fill = assault/murder, geom = "polygon")


###### 章节5.7
library(plyr)  # ddply()在新版本中已被剥离并整合到plyr包中，这里先载入该包
ia <- map_data("county", "iowa")
mid_range <- function(x) mean(range(x, na.rm = TRUE))
centres <- ddply(ia, .(subregion), colwise(mid_range, .(lat, long)))
ggplot(ia, aes(long, lat)) + geom_polygon(aes(group = group), fill = NA, colour = "grey60") + 
  geom_text(aes(label = subregion), data = centres, size = 2, angle = 45)


## 图5.14 进行数据变换以移除显而易见的效应。左图对 x 轴和 y
## 轴的数据均取以 10 为底的
## 对数以剔除非线性性。右图剔除了主要的线性趋势。
d <- subset(diamonds, carat < 2.5 & rbinom(nrow(diamonds), 1, 0.2) == 1)
d$lcarat <- log10(d$carat)
d$lprice <- log10(d$price)

# 剔除整体的线性趋势
detrend <- lm(lprice ~ lcarat, data = d)
d$lprice2 <- resid(detrend)

mod <- lm(lprice2 ~ lcarat * color, data = d)

library(effects)
effectdf <- function(...) {
  suppressWarnings(as.data.frame(effect(...)))
}
color <- effectdf("color", mod)
both1 <- effectdf("lcarat:color", mod)

carat <- effectdf("lcarat", mod, default.levels = 50)
both2 <- effectdf("lcarat:color", mod, default.levels = 3)

qplot(lcarat, lprice, data = d, colour = color)
qplot(lcarat, lprice2, data = d, colour = color)


## 图5.15(书中无代码) 展示模型估计结果中变量 color 的不确定性。左图为
## color 的边际效应。右图则 是针对变量 caret 的不同水平 (level)，变量
## color 的条件效应。误差棒显示了 95% 的逐点 置信区间。
fplot <- ggplot(mapping = aes(y = fit, ymin = lower, ymax = upper)) + ylim(range(both2$lower, 
                                                                                 both2$upper))
fplot %+% color + aes(x = color) + geom_point() + geom_errorbar()
fplot %+% both2 + aes(x = color, colour = lcarat, group = interaction(color, 
                                                                      lcarat)) + geom_errorbar() + geom_line(aes(group = lcarat)) + scale_colour_gradient()


## 图5.16(书中无代码) 展示模型估计结果中变量 carat 的不确定性。左图为
## caret 的边际效应。右图则是 针对变量 color 的不同水平，变量 caret
## 的条件效应。误差带显示了 95% 的逐点置信区间。
fplot %+% carat + aes(x = lcarat) + geom_smooth(stat = "identity")

ends <- subset(both1, lcarat == max(lcarat))
fplot %+% both1 + aes(x = lcarat, colour = color) + geom_smooth(stat = "identity") + 
  scale_colour_hue() + theme(legend.position = "none") + geom_text(aes(label = color, 
                                                                       x = lcarat + 0.02), ends)


## 图5.17 函数 stat_summary 的使用示例。首行从左至右分别展示了连续型变量
## x 的：中位 数曲线，median_hilow() 所得曲线和平滑带，均值曲线，以及
## mean_cl_boot() 所得曲线和 平滑带。次行从左至右分别展示了离散型变量 x
## 的：mean() 所得均值点，mean_cl_normal ()
## 所得均值点和误差棒，median_hilow()
## 所得中位数点和值域，以及median_hilow() 所 得中位数点和值域条。请注意
## ggplot2 展示了整个数据的取值范围，而不仅仅是各种描述性
## 统计量所涉及的范围。

## 以下两行为解决0.9.3版本中stat_summary的漏洞。之后的版本不需要下两行
library(devtools)
source_gist("https://gist.github.com/4578531")


m <- ggplot(movies, aes(year, rating))
m + stat_summary(fun.y = "median", geom = "line")
m + stat_summary(fun.data = "median_hilow", geom = "smooth")
m + stat_summary(fun.y = "mean", geom = "line")
m + stat_summary(fun.data = "mean_cl_boot", geom = "smooth")
m2 <- ggplot(movies, aes(factor(round(rating)), log10(votes)))
m2 + stat_summary(fun.y = "mean", geom = "point")
m2 + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar")
m2 + stat_summary(fun.data = "median_hilow", geom = "pointrange")
m2 + stat_summary(fun.data = "median_hilow", geom = "crossbar")


###### 章节5.9.1
midm <- function(x) mean(x, trim = 0.5)
m2 + stat_summary(aes(colour = "trimmed"), fun.y = midm, geom = "point") + 
  stat_summary(aes(colour = "raw"), fun.y = mean, geom = "point") + scale_colour_hue("Mean")


###### 章节5.9.2
iqr <- function(x, ...) {
  qs <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = T)
  names(qs) <- c("ymin", "ymax")
  qs
}
m + stat_summary(fun.data = "iqr", geom = "ribbon")


###### 章节5.10
(unemp <- qplot(date, unemploy, data = economics, geom = "line", xlab = "", 
                ylab = "No. unemployed (1000s)"))


###### 章节5.10
presidential <- presidential[-(1:3), ]

yrng <- range(economics$unemploy)
xrng <- range(economics$date)
unemp + geom_vline(aes(xintercept = as.numeric(start)), data = presidential)


###### 章节5.10
library(scales)
unemp + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party), 
                  ymin = yrng[1], ymax = yrng[2], data = presidential) + scale_fill_manual(values = alpha(c("blue", "red"), 0.2))


###### 章节5.10
last_plot() + geom_text(aes(x = start, y = yrng[1], label = name), data = presidential, 
                        size = 3, hjust = 0, vjust = 0)


###### 章节5.10
caption <- paste(strwrap("Unemployment rates in the US have\nvaried a lot over the years", 
                         40), collapse = "\n")
unemp + geom_text(aes(x, y, label = caption), data = data.frame(x = xrng[2], 
                                                                y = yrng[2]), hjust = 1, vjust = 1, size = 4)


###### 章节5.10
highest <- subset(economics, unemploy == max(unemploy))
unemp + geom_point(data = highest, size = 3, colour = alpha("red", 0.5))


## 图5.18 使用点的大小来展示权重：无权重 (左图)，以人口数量为权重
## (中图)，以面积为权 重 (右图)。
qplot(percwhite, percbelowpoverty, data = midwest)
qplot(percwhite, percbelowpoverty, data = midwest, size = poptotal/1e+06) + 
  scale_size_area("Population\n(millions)", breaks = c(0.5, 1, 2, 4))
qplot(percwhite, percbelowpoverty, data = midwest, size = area) + scale_size_area()

## 图5.19 未考虑权重的最优拟合曲线 (左图)
## 和以人口数量作为权重的最优拟合曲线 (右图)。
lm_smooth <- geom_smooth(method = lm, size = 1)
qplot(percwhite, percbelowpoverty, data = midwest) + lm_smooth
qplot(percwhite, percbelowpoverty, data = midwest, weight = popdensity, size = popdensity) + 
  lm_smooth


## 图5.20 不含权重信息 (左侧) 以及含权重信息 (右侧)
## 的直方图。不含权重信息的直方图展
## 示了郡的数量，而含权重信息的直方图展示了人口数量。权重的加入的确极大地改变了对图
## 形的解读！
qplot(percbelowpoverty, data = midwest, binwidth = 1)
qplot(percbelowpoverty, data = midwest, weight = poptotal, binwidth = 1) + ylab("population") 

#####################################
#ggplot2 examples
#####################################
# ggplot2例子1：图层控制与直方图
# 建立数据层
p <- ggplot(mpg, aes(x = hwy))
# 建立直方图图层
p <- p + geom_histogram(position = 'identity',   # 分布类型：单独的，非累积
                        alpha = 0.5,                                      # 透明度
                        # 按照年份填充颜色，density为使用密度函数
                        aes(y = ..density..,fill = factor(year))
                        ) 
p
# 建立密度函数分布图层
p <- p + stat_density(geom = 'line',             # 曲线类型
                      position = 'identity',                      # 分布类型：单独的，非累积
                      #aes(colour = factor(year))
                      )            # 颜色按年份
p
# 根据某个变量分开绘制
p <- p + facet_grid(drv ~ year)                     # drv为行变量，year为列变量

# ggplot2例子2：位置调整与条形图
# 条形图各种形式
with(mpg,table(class,year))
p <- ggplot(data = mpg, aes(x = class,fill = factor(year)))
p + geom_bar(position = 'dodge')    # 将不同年份的数据并列放置
p + geom_bar(position = 'stack')      # 将不同年份数据推叠放置
p + geom_bar(position = 'fill')          # 和stack类似，但是以百分比的形式出现
p + geom_bar(position = 'identity',alpha = 0.5)        # 直接显示，要显示成透明才能看到

# 美国GDP增长率例子
y = c(1.1,1.8,2.5,3.6,3.1,2.7,1.9,-0.1,-3.5,3.0)
x = 2001:2010
data = data.frame(x,y)     # ggplot2一定要是数据框的形式
p <- ggplot(data, aes(x,y,fill = y))           # 建立数据层，颜色以y变量实现渐变
p <- p + geom_bar(stat = "identity")     # 图形类型为独立显示
p <- p + geom_abline(intercept = 2, slope = 2,size=1,colour='gray')
p <- p + geom_text(aes(label=y),hjust=0.5, vjust=-0.5 ) # 添加标签，设置偏移系数
p <- p + scale_y_continuous(limits=c(-3.8,4.2))               # 设置y坐标的范围
p <- p + labs(x='年份', y='GDP增长率%')                         # 设置坐标标签
p <- p + opts(title = "美国GDP增长率")                            # 设置标题

# 课间佐料：同一个窗口画多个图
layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(1,1), c(1,1), TRUE)
plot(x1, y1)
plot(x2, y2)
plot(x3, y3)

# ggplo2例子3：散点图
p <- ggplot(mpg, aes(cty, hwy))

# 对色彩的形状的控制
p1 <- p + geom_point(aes(colour = factor(year),    # 颜色按年区分
                         shape = factor(year),     # 点状按年区分
                         size = displ),                  # 点的大小以变量displ控制
                     alpha = 0.6,                        # 控制透明度
                     position = 'jitter')              # 在点太过集中的时候，错开
print(p1)

# 对坐标的控制
cty.mean=with(mpg,mean(cty))
cty.sd=with(mpg,sd(cty))
p1 + scale_x_continuous(trans='log',    # 对x轴进行对数变换
                        # 只需要在x轴显示坐标
                        breaks=c(cty.mean-cty.sd,cty.mean,cty.mean+cty.sd), 
                        # 只需要在x轴显示坐标对应的命名
                        labels=c("high", "mean", "low"))
+ scale_y_continuous(trans='log')    # 对y轴进行对数变换

# 添加文字说明
p <- ggplot(mpg, aes(x=cty, y=hwy,colour=factor(year),label=year))
p <- p + geom_text(hjust=0,vjust=-1,alpha=0.8)
p <- p + geom_point(size=3,aes(shape=factor(year)))

# 矩阵散点图
plotmatrix(USArrests) + geom_smooth()  # ggplot2包自带的矩阵散点图函数
pairs(USArrests)                       # 基础包自带的矩阵散点图


# ggplo2例子4：时间序列
library(quantmod)
getSymbols('^SSEC',src='yahoo',from = '1997-01-01')
close <- (Cl(SSEC))
time <- index(close)
value <- as.vector(close)
p <- ggplot(data.frame(time,value),aes(time,value))
p <- p + geom_line()

yrng <- range(value)
xrng <- range(time)
data <- data.frame(start=as.Date(c('1997-01-01','2003-01-01')),
                   end=as.Date(c('2002-12-30','2012-01-20')),
                   core=c('jiang','hu'))
timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-01-10','2010-03-31'))
events <- c('证券法实施','国有股持减','股权分置改革','次贷危机爆发','融资融券试点')
data2 <- data.frame(timepoint,events,stock=value[time %in% timepoint])
p <- p + geom_line() 

if (F) {
  p <- p + geom_rect(aes(NULL,NULL,xmin = start, xmax = end, fill = core),
                     ymin = yrng[1],
                     ymax=yrng[2],
                     data = data) 
  p <- p + scale_fill_manual(values = alpha(c('blue','red'),0.2))
}
# 添加文本说明
p <- p + geom_text(aes(timepoint, stock, label = events),
                   data = data2,
                   vjust = -2,
                   size = 5) 
# 添加标记点
p <- p + geom_point(aes(timepoint, stock),
                    data = data2,
                    size = 5,
                    colour = 'red',
                    alpha = 0.5)

#------------------------------------------------------------
# multiple plots per page
# 一页多图
#------------------------------------------------------------
require(ggplot2)
require(grid)
#####现将图画好，并且赋值变量，储存#####
a <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
b <- ggplot(diamonds, aes(carat, depth, colour = color)) + geom_point()
c <- ggplot(diamonds, aes(carat, depth, colour = color)) + geom_point() + 
  facet_grid(.~color,scale = "free") 

########新建画图页面###########
grid.newpage()  ##新建页面
pushViewport(viewport(layout = grid.layout(2,2))) ####将页面分成2*2矩阵
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}
print(c, vp = vplayout(1,1:2))   ###将（1,1)和(1,2)的位置画图c
print(b, vp = vplayout(2,1))   ###将(2,1)的位置画图b
print(a, vp = vplayout(2,2))  ###将（2,2)的位置画图a
#dev.off() ##画下一幅图，记得关闭窗口
