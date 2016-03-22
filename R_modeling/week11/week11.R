library(quantmod)
library(tseries)
library(RColorBrewer)
ORCL<-get.hist.quote(instrument="ORCL",
                       start="2008-01-01", end="2011-12-31",                        
                       quote ="Volume", compression="d",
                       provider="yahoo")
GOOG<-get.hist.quote(instrument="GOOG",
                       start="2008-01-01", end="2011-12-31",                        
                       quote ="Volume", compression="d",
                       provider="yahoo")
MSFT<-get.hist.quote(instrument="MSFT",
                       start="2008-01-01", end="2011-12-31",                        
                       quote ="Volume", compression="d",
                       provider="yahoo")
ORCL_Y <-factor(substr(index(ORCL), 1, 4))
GOOG_Y <-factor(substr(index(GOOG), 1, 4))
MSFT_Y <-factor(substr(index(MSFT), 1, 4))
shares <- data.frame(
    Oracle = tapply(as.numeric(ORCL$Volume), ORCL_Y, sum)/sum(as.numeric(ORCL$Volume))*100,
    Google = tapply(as.numeric(GOOG$Volume), GOOG_Y, sum)/sum(as.numeric(GOOG$Volume))*100,
    Microsoft = tapply(as.numeric(MSFT$Volume), MSFT_Y, sum)/sum(as.numeric(MSFT$Volume))*100
  )
par(mar=c(5,4,4,8),xpd=T)
x<-barplot(as.matrix(shares), 
           col=brewer.pal(4,"Set1"),border="white", 
           main="Stock Volume (Percentage)")
legend("right",legend=rownames(shares),bty="n",
       inset=c(-0.3,0),fill=brewer.pal(4,"Set1"))

y <- shares
for(i in 1:nrow(y)){
  if (i==1) 
    y[1,] <- y[1,]/2
  else
    y[i,] <- shares[i-1,]/2+y[i-1,]+y[i,]/2
  text(x,y[i,],paste(round(shares[i,], digits=2),"%",sep=""))
}



"