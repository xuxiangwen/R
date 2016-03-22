library(quantmod)
library(tseries)
orcl<-get.hist.quote(instrument="ORCL",quote =c("cl","vol"))
plot(orcl$Close,main = "Oracle Stock Price", ylim=c(0,130), col="black", type="l", lwd=0.5, 
     pch=19,cex=0.6, xlab="Date" ,ylab="Stock Price (USD)"）
minIndex <- index(orcl)[which.min(orcl$Close)]
minPrice <- min(orcl$Close)
maxIndex <- index(orcl)[which.max(orcl$Close)]
maxPrice <- max(orcl$Close)
     
abline(h=maxPrice, col="red",lty=2)
abline(h=minPrice, col="green",lty=2)   
abline(v=maxIndex,col="red",lty=2)       
abline(v=minIndex,col="green",lty=2)
mtext(side=3,at=maxIndex, text=as.character(maxIndex),las=1,col="red",cex=0.8)
text(maxIndex,maxPrice,maxPrice,cex=0.8,col="red",adj=c(0,1))
mtext(side=3,at=minIndex, text=as.character(minIndex),las=1,col="green",cex=0.8)
text(minIndex,minPrice,minPrice,cex=0.8,col="green",adj=c(0,1))