(ma <- cbind(1:6, 1:3, 4:1))
(m <- colMeans(ma))
(s <- var(ma));(ss <- solve(s))

(ma1 <- sweep(ma, 2, m))
ma1_s <- solve(ma1)
(s1 <- t(ma1)%*%ma1);(ss1 <- solve(s1))
(z <- c(0, 0, 0))

#a<-scale(ma, center=T, scale=T)
(z1 <- sweep(t(z), 2, m))
(sd1 <- c(sd(ma1[,1]), sd(ma1[,2])))
(var1 <- c(var(ma1[,1]), var(ma1[,2])))
(z11 <- sweep(z1, 2, sd1, FUN = "/")) 
(d11 <- sqrt(z11%*%t(z11)))
(z12 <- sqrt(sum((z1*z1)/var1)))

z1(z1%*%ss%*%t(z1))
(z1%*%ss1%*%t(z1))
(d <- mahalanobis(z, m, s))


# ma <- rbind(c(3,4,5,6),c(2,2,8,4))
# (m <- colMeans(ma))
# (s <- var(ma));(ss <- solve(s))
# (z <- sweep(t(ma[,1]), 2, ma[,2]))
# (d <- mahalanobis(z, m, s))

