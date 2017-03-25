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

#marginal rug�w���@���װ��X�K���{��geom_rug(size=0.2)�i�۩w�q�ʲ�
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug(size=0.2)

#label
subset(countries, Year==2009 & healthexp>2000)
sp <- ggplot(subset(countries, Year==2009 & healthexp>2000),
             aes(x=healthexp, y=infmortality)) +geom_point()
sp + geom_text(aes(label=Name), size=4)
#���
sp + geom_text(aes(label=Name), size=4, vjust=0)
#���W�첾
sp + geom_text(aes(y=infmortality+.1, label=Name), size=4, hjust=0)





#labeling chosen countries 
cdat <- subset(countries, Year==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name
idx <- cdat$Name1 %in% c("Canada", "Ireland", "United Kingdom", "United States",
                         "New Zealand", "Iceland", "Japan", "Luxembourg",
                         "Netherlands", "Switzerland") 
#%in% �P�_���򪺪F�観�S���b��Ƹ̭� �����ܩ�i�hidx
idx
cdat$Name1[!idx] <- NA #�����Ū�
ggplot(cdat, aes(x=healthexp, y=infmortality)) +
  geom_point() +
  geom_text(aes(x=healthexp+100, label=Name1), size=4, hjust=0) +
  xlim(2000, 10000)


#balloon plot
hec <- HairEyeColor[,,"Male"] + HairEyeColor[,,"Female"]
library(reshape2) #to vector ����ƾ�
hec <- melt(hec, value.name="count") #reshape����melt���
#21-25���W�泣�i�Hfill.colour�C��
ggplot(hec, aes(x=Eye, y=Hair)) +
  geom_point(aes(size=count), shape=21, colour="black", fill="cornsilk") +
  scale_size_area(max_size=20, guide=FALSE) +
  geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22, label=count), vjust=1,
            colour="grey60", size=4) #grey60�N���{�� grey100�N�O�զ�F

#scatter plot matrix

c2009 <- subset(countries, Year==2009,
                select=c(Name, GDP, laborrate, healthexp, infmortality))
pairs(c2009[,2:5])
#�����ƪ���2��5��

pairs(c2009[,2:5], upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.smooth)
#upper.panel�k�W�T���� diag.panel���W�k�U�﨤�u lower.panel���U�T����
#panel.cor �g�X�����Y�� panel.hist���X������ panel.smooth���ƨ���

#basic histogram
ggplot(faithful, aes(x=waiting)) + geom_histogram()
faithful
w <- faithful$waiting
ggplot(NULL, aes(x=w)) + geom_histogram() #null�O���Ϋŧi���N��

binsize <- diff(range(faithful$waiting))/10
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=binsize, fill="white", colour="black")

#??????




#multiple histogram
library(MASS) #��mass�o�Ӹ�Ƥ�
birthwt
birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
levels(birthwt1$smoke)
library(plyr) 
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"="Smoke"))
ggplot(birthwt1, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

#scale:y�b���̼�
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +facet_grid(race ~ .)

#free scale:y�b��쳣���@��
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(race ~ ., scales="free")

#multiple histograms
birthwt1$smoke <- factor(birthwt1$smoke)
ggplot(birthwt1, aes(x=bwt, fill=smoke)) +
  geom_histogram(position="identity", alpha=0.4) #������ �ҥH�n�[position="identity"

#density curve
ggplot(faithful, aes(x=waiting)) + geom_density()

#�������u��geom_line,density
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
ggplot(birthwt1, aes(x=bwt, colour=smoke)) + geom_density()#����k�h�����̤U��������

#frequency polygon
ggplot(faithful, aes(x=waiting)) + geom_freqpoly() 
ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=4)

