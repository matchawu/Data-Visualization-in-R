library(ggplot2)
library(gcookbook)

head(diamonds) #���D��ƪ��e�X�C�O�ƻ�ˤl

ggplot(diamonds,aes(x=cut)) + geom_bar(stat="count") 
#�w�]�N�Ostat="count"�i�H�ٲ��N�ܦ�geom_bar()

ggplot(diamonds,aes(x=carat)) + geom_bar(stat="count") 
#�s���ܼ��٬O�i�H��count���|��C�@�Ӽƭȷ��@���O�|�ܶéҥH�ɶq��bin

ggplot(diamonds,aes(x=carat)) + geom_bar(stat="bin")
#�۸���count�N���|�����

ggplot(diamonds,aes(x=color)) + geom_bar(stat="count",colour="blue",fill="yellow") 
#color���p�������� �~�ض��� ���̺��
#���楴��ggplot(diamonds,aes(x=color)) + geom_bar(stat="bin") 

ggplot(diamonds,aes(x=carat)) + geom_histogram()
ggplot(diamonds,aes(x=carat)) + geom_bar(stat="bin")
#geom_histogram()�Mgeom_bar(stat="bin")�O�@�˪��N��(�N�Y����檺��ܤ覡�O�@�˪��N��)

#rank���ɭԳ̤j���|�]�b�̫᭱
#rank(x)=3 �N��

#run �`�@�����Q�Ӹ��
#�{�b�ڭn��Ʀr�̤j���e�Q�W
#�ҥH�᭱�Q�ӴN�O�Ʀr�̤j���Q��
#str(��ƦW��)�O��ܸ�ƪ����c�A���h�ֵ��B���h�����O�򶵥�
str(uspopchange) 
head(uspopchange)
upc <- subset(uspopchange,rank(Change)>40) #�e�Q�j����Ʃ�bupc�o�Ӹ�ƤU��
ggplot(upc,aes(x=Abb,y=Change,fill=Region))+
  geom_bar(stat="identity",colour="black")+
  scale_fill_manual(values=c("#669933","FFCC66"))+
  xlab("State")
#�ھڰϰ�h���C��
#�Hchange���j�p�hreorder�ƥL~��ƱƸ�� �ϸ̭����F��n�Ƨ� �ѵu�ƨ��
#xlab("State") �N����state�o�@���h���N�쥻x�b�n��ܪ��ϥ� lab�N�Olabel���N��

#ggplot(upc,aes(x=recorder(Abb,Change),y=Change,fill=Region))+
 # geom_bar(stat="identity",colour="black")+
  #scale_fill_manual(values=c("#669933","#FFCC66"))+xlab("State")
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) +
  xlab("State")
#reorder�N���O�Ѯv��e�Q�W�ǥͿ�X��(rank)���O�S���ƧǡA�Y�n�A�Ƥ@��Q�W�N��reorder

#csub�W���H�N�w�A�u�O�@�ӫ��w�L�h����ƦW��
csub <- subset(climate,Source=="Berkeley"&Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+
  geom_bar(stat="identity",position="identity")+
#scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) �n���@�w�n��values
  
#position="identity" �N����m�n�W��(?)
#guide=FALSE(?)

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
#�w�]�e��

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5)
#�e�׵���0.5

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge")
#���S�O���wposition_dodge�ҥH�|�H�b�@�_

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.5))
#�t�~���wposition=position_dodge(���j) ���j-width(0.5)�N�������������e��

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))
#0.7-0.5=0.2�ҥH�����|���j0.2

#position="dodge"�N�����n����Ӹ�ư��|�b�@�_
#���]width=a�Mposition=position_dodge(b)
#�p�Gb<a�A�|���|�A�@�_(�Ҧp0.2<0.5)
#�p�Gb=a�A�|���`(�~�A�@�_)
#�p�Gb>a�A�h�����|��b-a���Ů�e��

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.1) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD")) #���ϥ�

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.1) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) #���n�ϥ�
#guide=FALSE�O�N�����n���ϥܡA���bscale_fill_manual(���w�����Ϫ��C��)�̭�


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")
#�ϥ��C��|�A��(�Ϥ��H�Υk��guide)
#��reverse=TRUE�Ӱ�
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE))

#�N���ά��ʤ�����ܡA�i�H���Dc39��c52�b����100%�̭�������
#�Q��ddply
#sum(Weight)���q���`�M
#ce��percent_weight�O�۩w�q���W��
library(plyr) 
#ddply�ݭn��plyr�o�ӮM��
ce <- ddply(cabbage_exp, "Date", transform, #���θ�Ʃ����κ⦡
            percent_weight = Weight / sum(Weight) * 100)
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE)) #�W�������L���ϥ�½��
#�Ny�b�令percent_weight

#stat="identity"�N��������
#stat="count"�N�����O�������
#stat="bin"�N���s���ܼƪ�����ϡA�Y�H�b�@�_��������

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white")
#vjust�O�����վ㪺�N��A�_�h�ƭȷ|�g�b�����Ϫ����ݷ|�ݤ��M��
#�ƭȶV�j�A�V����(�U��)
#white�O�r���C��


ce <- arrange(cabbage_exp, Date, Cultivar)
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight))

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity",colour="black")+
  geom_text(aes(y=label_y,label=paste(format(Weight,nsmall=2),"kg")),vjust=1.5,colour="white",size=10)+
  guides(fill=guide_legend(reverse=TRUE))+
  scale_fill_brewer(palette = "Pastel1")

#scale_fill_manual(values=c("#669933", "#FFCC66"),guide=FALSE)�p����ܹϥ�?

#label_y�s�ܼƬO��2.26�i�H�X�{�b3.18+2.26���a��A�N�Ocumsum(Weight)�Nweight�a�A�@�_���N��

  

  
  
  
  
