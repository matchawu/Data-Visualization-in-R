data=read.table("class.csv",sep=",",header=T)

library(ggplot2)
library(gcookbook)

class.csv
data
head(data)

library(MASS) 

class1 <- class
class1$Team <- factor(class1$Team)
levels(class1$Team)
library(plyr) 
class1$Team <- revalue(class1$Team, c("0"="No Smoke", "1"="Smoke"))
ggplot(class1, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

ggplot(data,aes(x=Team,y=Grade))+geom_jitter()