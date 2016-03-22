rt<-read.table("exam0203.txt", head=TRUE);
print(rt)
lm.sol<-lm(Weight~Height, data=rt)
print(summary(lm.sol))