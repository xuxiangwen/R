

cost<-0.5
p1<-0.7
p2<-0.1
prod<-50
sell<-round(rnorm(1,100,10),0)
profit<-
  ifelse(sell>prod,
    prod*(p1-cost),
    sell*(p1-cost)+(prod-sell)*(p2-cost)
  )
profit
sellfun<-function(prod=50){
  cost<-0.5
  p1<-0.7
  p2<-0.1
  sell<-round(rnorm(10000,100,10),0)
  profit<-
    ifelse(sell>prod,
           prod*(p1-cost),
           sell*(p1-cost)+(prod-sell)*(p2-cost)
    )
  return(mean(profit))
}
  
x<-seq(50,150,by=1)
y<-sapply(x,sellfun)
plot(x,y,type='l')
max(y)
which.max(y)
x[which.max(y)]

x<-rnorm(n=1e4,0,10)
sd(x)
hist(x,20)
sampfunc<-function(n=100){
  y<-sample(x,size=n,replace=T)
  return(mean(y))
}
samplemean<-replicate(1000,sampfunc())
hist(samplemean,20)
sd(samplemean)
samplemean<-replicate(1000,sampfunc(1000))
sd(samplemean)
hist(samplemean,20)