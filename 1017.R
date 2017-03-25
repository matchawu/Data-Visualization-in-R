library(ggplot2)
library(gcookbook)

head(diamonds) #知道資料的前幾列是甚麼樣子

ggplot(diamonds,aes(x=cut)) + geom_bar(stat="count") 
#預設就是stat="count"可以省略就變成geom_bar()

ggplot(diamonds,aes(x=carat)) + geom_bar(stat="count") 
#連續型變數還是可以用count但會把每一個數值當作類別會很亂所以盡量用bin

ggplot(diamonds,aes(x=carat)) + geom_bar(stat="bin")
#相較於count就不會那麼亂

ggplot(diamonds,aes(x=color)) + geom_bar(stat="count",colour="blue",fill="yellow") 
#color的計次長條圖 外框黃色 內裡綠色
#不行打成ggplot(diamonds,aes(x=color)) + geom_bar(stat="bin") 

ggplot(diamonds,aes(x=carat)) + geom_histogram()
ggplot(diamonds,aes(x=carat)) + geom_bar(stat="bin")
#geom_histogram()和geom_bar(stat="bin")是一樣的意思(意即此兩行的顯示方式是一樣的意思)

#rank的時候最大的會跑在最後面
#rank(x)=3 代表

#run 總共有五十個資料
#現在我要找數字最大的前十名
#所以後面十個就是數字最大的十個
#str(資料名稱)是顯示資料的結構，有多少筆、有多少類別跟項目
str(uspopchange) 
head(uspopchange)
upc <- subset(uspopchange,rank(Change)>40) #前十大的資料放在upc這個資料下面
ggplot(upc,aes(x=Abb,y=Change,fill=Region))+
  geom_bar(stat="identity",colour="black")+
  scale_fill_manual(values=c("#669933","FFCC66"))+
  xlab("State")
#根據區域去分顏色
#以change的大小去reorder排他~資料排資料 圖裡面的東西要排序 由短排到長
#xlab("State") 代表用state這一項去替代原本x軸要顯示的圖示 lab就是label的意思

#ggplot(upc,aes(x=recorder(Abb,Change),y=Change,fill=Region))+
 # geom_bar(stat="identity",colour="black")+
  #scale_fill_manual(values=c("#669933","#FFCC66"))+xlab("State")
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) +
  xlab("State")
#reorder就像是老師把前十名學生選出來(rank)但是沒有排序，若要再排一到十名就用reorder

#csub名稱隨意定，只是一個指定過去的資料名稱
csub <- subset(climate,Source=="Berkeley"&Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+
  geom_bar(stat="identity",position="identity")+
#scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) 好像一定要有values
  
#position="identity" 代表位置要獨立(?)
#guide=FALSE(?)

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
#預設寬度

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5)
#寬度等於0.5

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge")
#不特別指定position_dodge所以會黏在一起

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.5))
#另外指定position=position_dodge(分隔) 分隔-width(0.5)就等於兩條之間的寬度

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))
#0.7-0.5=0.2所以中間會間隔0.2

#position="dodge"代表不要讓兩個資料堆疊在一起
#假設width=a和position=position_dodge(b)
#如果b<a，會堆疊再一起(例如0.2<0.5)
#如果b=a，會正常(年再一起)
#如果b>a，則中間會有b-a的空格寬度

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.1) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD")) #有圖示

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.1) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) #不要圖示
#guide=FALSE是代表不要有圖示，打在scale_fill_manual(指定長條圖的顏色)裡面


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")
#圖示顏色會顛倒(圖中以及右邊guide)
#用reverse=TRUE來做
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE))

#將之用為百分比表示，可以知道c39跟c52在全部100%裡面的分布
#利用ddply
#sum(Weight)重量的總和
#ce跟percent_weight是自定義的名稱
library(plyr) 
#ddply需要用plyr這個套件
ce <- ddply(cabbage_exp, "Date", transform, #分割資料或應用算式
            percent_weight = Weight / sum(Weight) * 100)
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE)) #上面有講過的圖示翻轉
#將y軸改成percent_weight

#stat="identity"代表長條圖
#stat="count"代表類別行長條圖
#stat="bin"代表連續型變數的直方圖，即黏在一起的長條圖

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white")
#vjust是垂直調整的意思，否則數值會寫在長條圖的頂端會看不清楚
#數值越大，越中間(下來)
#white是字的顏色


ce <- arrange(cabbage_exp, Date, Cultivar)
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight))

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity",colour="black")+
  geom_text(aes(y=label_y,label=paste(format(Weight,nsmall=2),"kg")),vjust=1.5,colour="white",size=10)+
  guides(fill=guide_legend(reverse=TRUE))+
  scale_fill_brewer(palette = "Pastel1")

#scale_fill_manual(values=c("#669933", "#FFCC66"),guide=FALSE)如何不顯示圖示?

#label_y新變數是讓2.26可以出現在3.18+2.26的地方，就是cumsum(Weight)將weight家再一起的意思

  

  
  
  
  

