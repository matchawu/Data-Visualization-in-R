library(ggplot2)
library(gcookbook) #直接導入gcookbook(a library)的資料

head(BOD) #當這個資料集很龐大，只要看前面幾行(但這裡的bod是小小的資料~)

ggplot(pg_mean,aes(x=group,y=weight)) + geom_bar(stat="identity")
#geom_bar是長條圖，stat="identity"是跟ggplot2講每個x值對到的y是甚麼

str(BOD) #another data named BOD
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat="identity")
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat="identity") #將空白的項目跳過
#x=factor(Time)代表time這個變數中跳過項目為空白的~例如第六項目是0，就跳過

ggplot(pg_mean,aes(x=group,y=weight))+
  geom_bar(stat="identity",fill="yellow",colour="orange") #fill填滿，colour是邊框顏色

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity",position="dodge") #複式

ce <- cabbage_exp[1:5, ] #取前五列丟到ce裡面
ggplot(ce,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity",position="dodge",colour="black")+
  scale_fill_brewer(palette="Pastel1") #這樣她會佔據全部(look d21)





