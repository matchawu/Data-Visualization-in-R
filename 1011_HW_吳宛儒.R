library(ggplot2)
library(gcookbook) 

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity",position="dodge",colour="black") 
  #複式繪圖.cabbage.黑色邊框

