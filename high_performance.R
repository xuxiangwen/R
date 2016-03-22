library(nycflights13)
library(dplyr)

#dplyr group_by性能比较aggregate
dim(flights)
head(with(flights,tapply(dep_delay,dest,mean,na.rm=T)))

system.time(aggregate(dep_delay~dest,flights,mean))
system.time(flights%>%
              group_by(month,dest)%>%
              summarise(avg_delay=mean(dep_delay,na.rm=T))
)

A <- matrix(data = 0, 5000, 5000)
x <- sample(1:5000, size = 5000, replace = TRUE)
y <- sample(1:5000, size = 5000, replace = TRUE)
for (i in 1:5000) {
  A[x[i], y[i]] <- 1
}

#bigmemory
library(bigmemory)
library(biganalytics)
options(bigmemory.typecast.warning = FALSE)
B <- big.matrix(5000, 5000, init = 0)
for (i in 1:5000) {
  B[x[i], y[i]] <- 1
}
B
## An object of class "big.matrix"
## Slot "address":
## <pointer: 0x26a5690>