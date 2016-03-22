x<-c(1.000, 0.846, 0.805, 0.859, 0.473, 0.398, 0.301, 0.382,
     0.846, 1.000, 0.881, 0.826, 0.376, 0.326, 0.277, 0.277,
     0.805, 0.881, 1.000, 0.801, 0.380, 0.319, 0.237, 0.345,
     0.859, 0.826, 0.801, 1.000, 0.436, 0.329, 0.327, 0.365,
     0.473, 0.376, 0.380, 0.436, 1.000, 0.762, 0.730, 0.629,
     0.398, 0.326, 0.319, 0.329, 0.762, 1.000, 0.583, 0.577,
     0.301, 0.277, 0.237, 0.327, 0.730, 0.583, 1.000, 0.539,
     0.382, 0.415, 0.345, 0.365, 0.629, 0.577, 0.539, 1.000)

names<-c("Height","Arm Length","Upperarm Length","lower Limbs Length","Weight","Neck Circumference",
         "Chest Circumference","Chest Width")
r<-matrix(x, nrow=8, dimnames=list(names, names))
hc<-hclust(d); dend<-as.dendrogram(hc)

nP<-list(col=3:2, cex=c(2.0, 0.75), pch= 21:22,
         bg= c("light blue", "pink"),
         lab.cex = 1.0, lab.col = "tomato")
addE <- function(n){
  if(!is.leaf(n)){
    attr(n,"edgePar")<-list(p.col="plum")
    attr(n,"edgetext")<-paste(attr(n,"members"),"members")
  }
  n
}

de <- dendrapply(dend, addE); plot(de, nodePar= nP)