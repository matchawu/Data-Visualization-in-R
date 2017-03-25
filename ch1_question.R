#[Bar Graph]

library(ggplot2) #��ı�ƹϪ�
library(gcookbook) #��ƶ�
library(plyr) #���Fddply�o�Ө��

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")

head(BOD) #���D�e�X�ո�ƪ�����ˤl
str(BOD) #���D��ƪ����c(�Ҧp:���X���ܼ�)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity") #�令���O���ܼ�(���O���ܼ�:�ʧO�u���k�k)

ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") #��������C��M��u

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") #�Ʊ��Ӫ����{�}(���O���ײy)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  scale_fill_brewer(palette="Pastel1") #���Ϫ��M�@�Ӭ��s���ҪO(�᭱�|�W��)

ce <- cabbage_exp[1:5, ] #���e5�C
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +  #�Ϋ~�إh���������C��
  geom_bar(stat="identity", position="dodge", colour="black") +
  scale_fill_brewer(palette="Pastel1")

ggplot(diamonds, aes(x=color)) + geom_bar(stat="count") #�p��
ggplot(diamonds, aes(x=carat)) + geom_histogram() #��stat=��bin���@��

head(uspopchange) #����H�f�ܤƤ��
upc <- subset(uspopchange, rank(Change)>40) #���e10�j
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")

ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) + #�����ϥѵu�ƨ��
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + #���ܪ������C��(�⸹)
  xlab("State") #���Nx�b��r

csub <- subset(climate, Source=="Berkeley" & Year >= 1900) #���a�ϬOBerkeley�M�~���j��1900�~�����
csub$pos <- csub$Anomaly10y >= 0  #�s�Wpos�o���ܼ� �u�nAnomaly10y�j�󵥩�0�N�|�OTRUE �p��0�N�OFALSE
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity")

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.1) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) #���n�ϥ�

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5) 
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=1)
#���ܪ����e��

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge") #�w�]���}���Z���O�������e��
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") + 
  guides(fill=guide_legend(reverse=TRUE)) #�ϥܤW�U�A��

ce <- ddply(cabbage_exp, "Date", transform, 
            percent_weight = Weight / sum(Weight) * 100) 
#���θ�Ʃ����κ⦡(�o��O�p��ʤ���)

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white") #��ܼƭȤ�r(vjust�O�����վ�)

ce <- arrange(cabbage_exp, Date, Cultivar)
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight)) #���Fy����m�s�W���ܼ�(�֥[�P�@�����y��)

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label_y, label=Weight), vjust=1.5, colour="white") #��m�Olabel_y ��ܼƭȬOWeight


tophit[, c("name", "lg", "avg")] #vector�}�C�s���ܼ�
ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(tophit, aes(x=reorder(name, avg), y=avg)) +
  geom_point(size=3) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1), #���שMhjust�����վ�
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

#[Line Graph]

ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD1 <- BOD # �ƻs�@�Ӹ�ƶ�
BOD1$Time <- factor(BOD1$Time) #�令���O���ܼ�
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0) #y�b�q0�}�l

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()

ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() +
  scale_y_log10() #�Ψ�log�ݥX�Ͷ�

tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len)) #�u��o����ܼ�
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line() #���C��Ϥ�
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line() #�̽u���˦��Ϥ�
ggplot(tg, aes(x=factor(dose), y=length, colour=supp, group=supp)) + geom_line() #�令���O���ܼƭn�i�D�L�O�@��group

ggplot(tg, aes(x=dose, y=length, shape=supp)) + geom_line() +
  geom_point(size=4) #�̧Ϊ���
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() +
  geom_point(size=4, shape=21)

ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dashed", size=1, colour="blue")

ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=22, colour="darkred", fill="pink")

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
#�u�U���n��
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +  #�z����
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup))) #�ϥܤW�U�A��
