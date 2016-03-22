library(ggplot2)
library(reshape2)
library(plyr)
library(scales)


who<-read.csv("WHO.csv",head=TRUE)
ten <- who[order(who$"Population..in.thousands..total", decreasing = T)[1:10],]
tenten <- ten[c(
  "Country",
  "Population..in.thousands..total",
  "Adult.literacy.rate....",
  "Population.in.urban.areas....",
  "Hospital.beds..per.10.000.population.",
  "Number.of.physicians",
  "Bad_teeth_per_child",
  "Children_and_elderly",
  "Children_per_woman",
  "HIV_infected",
  "Income_growth")]

tenten$"Number.of.physicians" <- tenten$"Number.of.physicians"/tenten$"Population..in.thousands..total"
tenten$"Population..in.thousands..total" <- 10000000/tenten$"Population..in.thousands..total"
tenten$"Bad_teeth_per_child" <- 100/tenten$"Bad_teeth_per_child"
tenten$"Children_per_woman" <- 100/tenten$"Children_per_woman"
tenten$"HIV_infected" <- 100/tenten$"HIV_infected"

for(i in 2:ncol(tenten)){
  narows <- which(is.na(tenten[i]))
  if (length(narows) > 0) 
    tenten[narows,i]<-mean(tenten[,i], na.rm = T)  
}

tt_melt<-melt(tenten,id=c('Country'))  
tt_scale<-ddply(tt_melt,.(variable),transform,rescale=rescale(value))
p<-ggplot(data=tt_scale,mapping=aes(variable,Country)) 
base_size <- 9
p<-p + theme_grey(base_size = base_size) + 
  labs(x = "",y = "") + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_text(size=10,angle = 330, hjust = 0, colour = "black"),
        axis.text.y = element_text(size=10,colour = "black"))
p<-p + geom_tile(aes(fill = rescale),colour="white")
p<-p+scale_fill_gradient(low="white",high="steelblue")
p
