library(ggplot2)
library(gcookbook) 

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity",position="dodge",colour="black") 
  #�Ʀ�ø��.cabbage.�¦����

