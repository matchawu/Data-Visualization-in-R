library(ggplot2)
library(gcookbook) #�����ɤJgcookbook(a library)�����

head(BOD) #���o�Ӹ�ƶ����e�j�A�u�n�ݫe���X��(���o�̪�bod�O�p�p�����~)

ggplot(pg_mean,aes(x=group,y=weight)) + geom_bar(stat="identity")
#geom_bar�O�����ϡAstat="identity"�O��ggplot2���C��x�ȹ�쪺y�O�ƻ�

str(BOD) #another data named BOD
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat="identity")
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat="identity") #�N�ťժ����ظ��L
#x=factor(Time)�N��time�o���ܼƤ����L���ج��ťժ�~�Ҧp�Ĥ����جO0�A�N���L

ggplot(pg_mean,aes(x=group,y=weight))+
  geom_bar(stat="identity",fill="yellow",colour="orange") #fill�񺡡Acolour�O����C��

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity",position="dodge") #�Ʀ�

ce <- cabbage_exp[1:5, ] #���e���C���ce�̭�
ggplot(ce,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat="identity",position="dodge",colour="black")+
  scale_fill_brewer(palette="Pastel1") #�o�˦o�|���ڥ���(look d21)




