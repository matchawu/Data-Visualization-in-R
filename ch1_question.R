#[Bar Graph]

library(ggplot2) #視覺化圖表
library(gcookbook) #資料集
library(plyr) #為了ddply這個函數

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")

head(BOD) #知道前幾組資料長什麼樣子
str(BOD) #知道資料的結構(例如:有幾個變數)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity") #改成類別型變數(類別型變數:性別只有男女)

ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") #改長條的顏色和邊線

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") #希望兩個長條閃開(像是躲避球)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  scale_fill_brewer(palette="Pastel1") #為圖表套一個美編的模板(後面會上到)

ce <- cabbage_exp[1:5, ] #取前5列
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +  #用品種去分長條的顏色
  geom_bar(stat="identity", position="dodge", colour="black") +
  scale_fill_brewer(palette="Pastel1")

ggplot(diamonds, aes(x=color)) + geom_bar(stat="count") #計次
ggplot(diamonds, aes(x=carat)) + geom_histogram() #跟stat=”bin”一樣

head(uspopchange) #美國人口變化比例
upc <- subset(uspopchange, rank(Change)>40) #取前10大
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")

ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) + #長條圖由短排到長
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + #改變長條的顏色(色號)
  xlab("State") #替代x軸文字

csub <- subset(climate, Source=="Berkeley" & Year >= 1900) #取地區是Berkeley和年份大於1900年的資料
csub$pos <- csub$Anomaly10y >= 0  #新增pos這個變數 只要Anomaly10y大於等於0就會是TRUE 小於0就是FALSE
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity")

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.1) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) #不要圖示

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5) 
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=1)
#改變長條寬度

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge") #預設分開的距離是長條的寬度
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") + 
  guides(fill=guide_legend(reverse=TRUE)) #圖示上下顛倒

ce <- ddply(cabbage_exp, "Date", transform, 
            percent_weight = Weight / sum(Weight) * 100) 
#分割資料或應用算式(這邊是計算百分比)

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white") #顯示數值文字(vjust是垂直調整)

ce <- arrange(cabbage_exp, Date, Cultivar)
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight)) #為了y的位置新增的變數(累加同一日期的y值)

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label_y, label=Weight), vjust=1.5, colour="white") #位置是label_y 顯示數值是Weight


tophit[, c("name", "lg", "avg")] #vector陣列連結變數
ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(tophit, aes(x=reorder(name, avg), y=avg)) +
  geom_point(size=3) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1), #角度和hjust水平調整
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

#[Line Graph]

ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD1 <- BOD # 複製一個資料集
BOD1$Time <- factor(BOD1$Time) #改成類別型變數
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0) #y軸從0開始

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()

ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() +
  scale_y_log10() #用取log看出趨勢

tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len)) #只選這兩個變數
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line() #依顏色區分
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line() #依線的樣式區分
ggplot(tg, aes(x=factor(dose), y=length, colour=supp, group=supp)) + geom_line() #改成類別型變數要告訴他是一個group

ggplot(tg, aes(x=dose, y=length, shape=supp)) + geom_line() +
  geom_point(size=4) #依形狀分
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() +
  geom_point(size=4, shape=21)

ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dashed", size=1, colour="blue")

ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=22, colour="darkred", fill="pink")

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
#線下面積圖
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +  #透明度
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup))) #圖示上下顛倒

