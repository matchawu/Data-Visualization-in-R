library(ggplot2)
library(gcookbook)

library(plyr) 

ce <- ddply(cabbage_exp, "Date", transform, #分割資料或應用算式
            percent_weight = Weight / sum(Weight) * 100,label_y=cumsum(Weight / sum(Weight) * 100))
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE)) +
  geom_text(aes(y=label_y,label=Weight/sum(Weight)*100),vjust=1.5,colour="white")