library(ggplot2)
library(gcookbook)

#Continuous variable
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + geom_point()

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="black", high="white")

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient_(low="black", high="white", breaks=12:17, guide=guide_legend())

ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
  geom_point(alpha=.5) +
  scale_size_area() + 
  scale_colour_brewer(palette="Set1")

#overplotting
sp <- ggplot(diamonds, aes(x=carat, y=price))
sp + geom_point() +
  scale_fill_gradient(low="lightblue",high="red", limits=c(0, 6000))

sp + stat_bin2d(bins=50) +
  scale_fill_gradient(low="lightblue", high="red", limits=c(0, 8000))

sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
sp1 + geom_point()
sp1 + geom_point(position="jitter")
sp1 + geom_jitter(width=0.5,height=0.8)

#marginal rug針對單一維度做出密集程度geom_rug(size=0.2)可自定義粗細
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug(size=0.2)

#label
subset(countries, Year==2009 & healthexp>2000)
sp <- ggplot(subset(countries, Year==2009 & healthexp>2000),
             aes(x=healthexp, y=infmortality)) +geom_point()
sp + geom_text(aes(label=Name), size=4)
#對其
sp + geom_text(aes(label=Name), size=4, vjust=0)
#往上位移
sp + geom_text(aes(y=infmortality+.1, label=Name), size=4, hjust=0)





#labeling chosen countries 
cdat <- subset(countries, Year==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name
idx <- cdat$Name1 %in% c("Canada", "Ireland", "United Kingdom", "United States",
                         "New Zealand", "Iceland", "Japan", "Luxembourg",
                         "Netherlands", "Switzerland") 
#%in% 判斷後續的東西有沒有在資料裡面 有的話放進去idx
idx
cdat$Name1[!idx] <- NA #給予空直
ggplot(cdat, aes(x=healthexp, y=infmortality)) +
  geom_point() +
  geom_text(aes(x=healthexp+100, label=Name1), size=4, hjust=0) +
  xlim(2000, 10000)


#balloon plot
hec <- HairEyeColor[,,"Male"] + HairEyeColor[,,"Female"]
library(reshape2) #to vector 重整數據
hec <- melt(hec, value.name="count") #reshape中的melt函數
#21-25的獨行都可以fill.colour顏色
ggplot(hec, aes(x=Eye, y=Hair)) +
  geom_point(aes(size=count), shape=21, colour="black", fill="cornsilk") +
  scale_size_area(max_size=20, guide=FALSE) +
  geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22, label=count), vjust=1,
            colour="grey60", size=4) #grey60代表程度 grey100就是白色了

#scatter plot matrix

c2009 <- subset(countries, Year==2009,
                select=c(Name, GDP, laborrate, healthexp, infmortality))
pairs(c2009[,2:5])
#抓取資料的第2到5欄

pairs(c2009[,2:5], upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.smooth)
#upper.panel右上三角型 diag.panel左上右下對角線 lower.panel左下三角形
#panel.cor 寫出相關係數 panel.hist劃出長條圖 panel.smooth平滑取縣

#basic histogram
ggplot(faithful, aes(x=waiting)) + geom_histogram()
faithful
w <- faithful$waiting
ggplot(NULL, aes(x=w)) + geom_histogram() #null是不用宣告的意思

binsize <- diff(range(faithful$waiting))/10
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=binsize, fill="white", colour="black")

#??????




#multiple histogram
library(MASS) #用mass這個資料及
birthwt
birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
levels(birthwt1$smoke)
library(plyr) 
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"="Smoke"))
ggplot(birthwt1, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

#scale:y軸欄位依樣
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +facet_grid(race ~ .)

#free scale:y軸欄位都不一樣
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(race ~ ., scales="free")

#multiple histograms
birthwt1$smoke <- factor(birthwt1$smoke)
ggplot(birthwt1, aes(x=bwt, fill=smoke)) +
  geom_histogram(position="identity", alpha=0.4) #長條圖 所以要加position="identity"

#density curve
ggplot(faithful, aes(x=waiting)) + geom_density()

#消除底線用geom_line,density
ggplot(faithful, aes(x=waiting)) + geom_line(stat="density") +
  expand_limits(y=0)

#density curve + histogram
ggplot(faithful, aes(x=waiting, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() +
  xlim(35, 105)

#multiple density curves
library(MASS) 
birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
ggplot(birthwt1, aes(x=bwt, colour=smoke)) + geom_density()#有辦法去除掉最下面那條嗎

#frequency polygon
ggplot(faithful, aes(x=waiting)) + geom_freqpoly() 
ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=4)


