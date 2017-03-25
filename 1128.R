library(ggplot2)

#x.y軸對調
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + coord_flip() 

#改變group的順序
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + coord_flip() + 
  scale_x_discrete(limits=rev(levels(PlantGrowth$group))) 
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + coord_flip() +
  scale_x_discrete(limit=c("trt2","trt1","ctrl"))

#y的範圍
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p + ylim(0, max(PlantGrowth$weight))
p + coord_cartesian(ylim = c(4, 6.5))
p + expand_limits(y=0)

#y軸上下對調
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + scale_y_reverse()
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + ylim(6.5,3.5)

#如果同時要顛倒數字又要限制範圍不能用ylim
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  scale_y_reverse(limits=c(8, 0))

#調整xy軸比例 符合數字正確的比例
sp <- ggplot(marathon, aes(x=Half,y=Full)) + geom_point()
sp + coord_fixed()

#y軸是x軸的1/2倍
sp + coord_fixed(ratio=1/2) +
  scale_y_continuous(breaks=seq(0, 420, 30)) +
  scale_x_continuous(breaks=seq(0, 420, 15))

#y軸分點
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6, 8))

#類別變數分點
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  scale_x_discrete(limits=c("trt1", "ctrl"), breaks="ctrl")

#圓形長條圖
ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15, origin=-7.5) +
  coord_polar()+
  scale_x_continuous(limits=c(0,360))

#日期
ggplot(economics, aes(x=date, y=psavert)) + geom_line()
#日期範圍
econ <- subset(economics, date >= as.Date("1992-05-01") &
                 date < as.Date("1993-06-01"))
p <- ggplot(econ, aes(x=date, y=psavert)) + geom_line()
datebreaks <- seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by="2 month")

p + scale_x_date(breaks=datebreaks) +
  theme(axis.text.x = element_text(angle=30, hjust=1))

p + scale_x_date(breaks=datebreaks, labels=date_format("%Y %b")) +
  theme(axis.text.x = element_text(angle=30, hjust=1))

#改語言
# Mac and Linux
Sys.setlocale("LC_TIME", "it_IT.UTF-8")
# Windows
Sys.setlocale("LC_TIME", "italian")

# 把時間序列轉成資料結構
www <- data.frame(minute = as.numeric(time(WWWusage)),
                  users = as.numeric(WWWusage))

timeHM_formatter <- function(x) {
  h <- floor(x/60) #包含不大於x的相應元素的最大整數的數值向量
  m <- floor(x %% 60) #餘數
  lab <- sprintf("%d:%02d", h, m) # HH:MM
  return(lab)
}

ggplot(www, aes(x=minute, y=users)) + geom_line() +
  scale_x_continuous(name="time", breaks=seq(0, 100, by=10),
                     labels=timeHM_formatter)

timeHM_formatter(c(0, 50, 51, 59, 60, 130, 604))
