#23
x <- c(36, 40, 19, 2, 0, 2, 1, 0)
x1 <- c(x[1:3], sum(x[4:length(x)]))
k <- 0:(length(x)-1)
lambda <- t(k)%*%x/sum(x)
f <- numeric(0)
for(i in 0:2){
  f <- c(f, exp(-1)/factorial(i)*sum(x))
}
f <- c(f, ppois(2,1,F)*sum(x))
q <- t(f-x1)%*%((f-x1)/f)
q <- sum((f-x1)*(f-x1)/f)
q_alpha<-qchisq(0.95, length(f)-1-1)
q>q_alpha

#24
x <- c(121,78,43,58)
n <- sum(x)
f <- c(pexp(100, 0.005)*n)
f[2]<-c(pexp(100*2, 0.005)*n)-sum(f)
f[3]<-c(pexp(100*3, 0.005)*n)-sum(f)
f[4]<-n-sum(f)
q <- sum(x*x/f)-n
q_alpha<-qchisq(0.95, length(x)-1)
q>q_alpha
  
#25
x<-c(rep(25,5),rep(35,15),rep(45,30),rep(55,51),
     rep(65,60),rep(75,23),rep(85,10),rep(95,6))
hist(x)
x<-c(5,15,30,51,60,23,10,6)
n<-sum(x)
l<-length(x)
f <- pnorm(10+20, 60, 15)*n
for(i in 2:l){
  f[i] <- (pnorm(i*10+20, 60, 15)-pnorm(i*10+10, 60, 15))*n
}
f[length(x)]<-(1-pnorm(90, 60, 15))*n
f1<-c(f[1]+f[2],f[-c(1,2,l-1,l)],f[l-1]+f[l])
x1<-c(x[1]+x[2],x[-c(1,2,l-1,l)],x[l-1]+x[l])
l1<-length(x1)
q <- sum(x1*x1/f1)-n;q
q_1_alpha<-qchisq(1-0.1, l1-1);q_1_alpha
q>q_1_alpha

#26
x <- c(1, 31, 55, 25)
n<-sum(x)
f <- c(1/56, 15/56, 30/56, 10/56)*n
x1<-c(x[1]+x[2], x[3], x[4])
f1<-c(f[1]+f[2], f[3], f[4])
l1<-length(x1)
q <- sum(x1*x1/f1)-n;q
q_1_alpha<-qchisq(1-0.05, l1-1);q_1_alpha
q>q_1_alpha

#27
x <- c(132, 100, 200, 168)
n<-sum(x)
f <- c(20, 15, 40, 25)/100*n
x1<-x
f1<-f
l1<-length(x1)
q <- sum(x1*x1/f1)-n;q
q_1_alpha<-qchisq(1-0.05, l1-1);q_1_alpha
q>q_1_alpha

#28
x <- c(48,31,20,9,6,5,4,2,1,1,2,1,0)
n <- sum(x)
c <- 1:length(x)
p <- 1-sum(x)/sum(x*c)
x <- c(48,31,20,9,6,5,11)
f <- numeric(0)
i<-1;f[i] <- p^(i-1)*(1-p)*n
i<-2;f[i] <- p^(i-1)*(1-p)*n
i<-3;f[i] <- p^(i-1)*(1-p)*n
i<-4;f[i] <- p^(i-1)*(1-p)*n
i<-5;f[i] <- p^(i-1)*(1-p)*n
i<-6;f[i] <- p^(i-1)*(1-p)*n
i<-7;f[i] <- sum(x)-sum(f)
x1<-x
f1<-f
l1<-length(x1)
q <- sum(x1*x1/f1)-n;q
q_1_alpha<-qchisq(1-0.05, l1-1-1);q_1_alpha
q>q_1_alpha

#29
x1<-c(34,39,41,28,33)
x2<-c(36,40,35,31,39,36)
n1<-length(x1)
n2<-length(x2)
mu_r<-n1*(n1+n2+1)/2
sigma_r_2<-n1*n2*(n1+n2+1)/12
q<-(sum(rank(c(x1,x2))[1:n1])-mu_r)/sigma_r_2^0.5;q
p<-pnorm(q);p
q_1_alpha<-qnorm(1-0.01);q_1_alpha
q>q_1_alpha

#30
x1<-c(5.5,5.6,5.3,4.6,5.3,5.0,6.2,5.8,5.1,5.2,5.9)
x2<-c(3.8,4.3,4.2,4.0,4.9,4.5,5.2,4.8,4.5,3.9,3.7,4.6)
n1<-length(x1)
n2<-length(x2)
mu_r<-n1*(n1+n2+1)/2
sigma_r_2<-n1*n2*(n1+n2+1)/12
q<-(sum(rank(c(x1,x2))[1:n1])-mu_r)/sigma_r_2^0.5;q
p<-pnorm(q);p
q_1_alpha<-qnorm(1-0.01);q_1_alpha
q>q_1_alpha

