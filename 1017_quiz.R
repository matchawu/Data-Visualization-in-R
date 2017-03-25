library(ggplot2)
library(gcookbook)

library(plyr) 

ce <- ddply(cabbage_exp, "Date", transform,
            percent_weight = Weight/sum(Weight)*100,
            label_y=cumsum(Weight)-0.5*Weight,
            percent_weight_label_y=cumsum(Weight/sum(Weight)*100)-0.5*Weight/sum(Weight)*100
            )

ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE))+
  geom_text(aes(y=percent_weight_label_y,label=percent_weight),vjust=0.07,colour="white")



