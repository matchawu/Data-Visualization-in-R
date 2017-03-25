library(ggplot2)
library(gcookbook)
library(MASS)

#p.2
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot()

ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot(width=.5)

ggplot(birthwt,es(x=factor(race),y=bwt)) +
  geom_bxplot(outlier.size=1.5,outlier.shape=21)

#p.5 
ggplot(birthwt, aes(x=1, y=bwt)) + 
  geom_boxplot() +scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())
#ºô½u??

ggplot(birthwt,aes(x=factor(race),y=bwt)) +
  geom_boxplot(notch=TRUE)

#p.7 
library(gcookbook)
p <- ggplot(heightweight, aes(x=sex, y=heightIn))
p + geom_violin()

#p.8 
p + geom_violin() + geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
  stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)

p + geom_violin(trim=FALSE)



#p.9
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() 
p +coord_flip() + scale_x_discrete(limit=c("trt1","trt2","ctrl"))

#p.10
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() 
p +coord_flip() + scale_x_discrete(limit=c("trt1","trt2","ctrl"))

p + coord_flip() + scale_x_discrete(limits=rev(levels(PlantGrowth$group)))
#p.11
p + expand_limits(y=2)

#p.12
p + ylim(0,max(PlantGrowth$weight))
p + scale_y_continuous(limits=c(0, max(PlantGrowth$weight)))


#p.13
p + ylim(6.5, 3.5)

#p.14
p + coord_cartesian(ylim = c(5, 6.5))

#p.15
p + scale_y_continuous(breaks=NULL)
p + scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6, 8))


p + scale_y_continuous(breaks=seq(4,7,by=.5))


p + scale_x_discrete(limits=c("trt2", "ctrl"), breaks="ctrl")


p + theme(axis.ticks = element_blank(), axis.text.y = element_blank())


p + theme(axis.title.x=element_blank())



p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p + guides(fill=FALSE)
p + scale_fill_discrete(guide=FALSE)
p + theme(legend.position="none")



p  + theme(legend.position="top")


p + theme(legend.position=c(1,0), legend.justification=c(1,0))


p + theme(legend.position=c(.85,.2)) +
  theme(legend.background=element_rect(fill="blue", colour="red"))

p + theme(legend.position=c(.85,.2)) +
  theme(legend.background=element_rect(fill="blue", colour="red")) +
  theme(legend.key=element_blank())


p + theme(legend.position=c(.85,.2)) +
  theme(legend.background=element_blank())
theme(legend.key=element_blank())


p + scale_fill_discrete(limits=c("ctrl", "trt2", "trt1"))  


p + scale_fill_grey(start=0.8, end=1, limits=c("trt1", "trt2", "ctrl"))


p + scale_fill_brewer(palette="Pastel2", limits=c("trt1", "trt2", "ctrl"))




p + scale_fill_hue(guide=guide_legend(reverse=TRUE))

p + guides(fill=guide_legend(reverse=TRUE))


p + labs(fill="Condition")
p + scale_fill_discrete(name="Condition")
p + guides(fill=guide_legend(title="Condition"))
p + scale_fill_hue(guide = guide_legend(title ="Condition"))


p + theme(legend.title=element_text(face="italic", colour="red", size=14,angle = 0))
p + guides(fill=guide_legend(title.theme=element_text(face="italic", colour="red", size=14,angle=0)))


p + theme(legend.text=element_text(face="italic",angle=0, colour="red",
                                   size=14))
p + guides(fill=guide_legend(label.theme=
                               element_text(face="italic", angle=0, colour="red", size=14)))


p + scale_fill_discrete(labels=c("Control", "Treatment 1", "Treatment 2"))



p + scale_fill_discrete(limits=c("trt1", "trt2", "ctrl"),
                        labels=c("Treatment 1", "Treatment 2", "Control"))


p + scale_fill_discrete(labels=c("Control", "Type 1\ntreatment",
                                 "Type 2\ntreatment")) +
  theme(legend.text=element_text(lineheight=.8),
        legend.key.height=unit(1, "cm"))


countries2009 <- subset(countries, Year==2009 & healthexp>2000)
p <- ggplot(countries2009, aes(x=infmortality))
p + geom_dotplot()


p + geom_dotplot(binwidth=0.25)  +
  scale_y_continuous(breaks=NULL) + geom_rug() +
  theme(axis.title.y=element_blank())

p + geom_dotplot(method="histodot", binwidth=.25) + geom_rug() +
  scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())


p + geom_dotplot(binwidth=.25, stackdir="center")+
  scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())

p + geom_dotplot(binwidth=.25, stackdir="centerwhole")+
  scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())


ggplot(heightweight, aes(x=sex, y=heightIn)) +
  geom_dotplot(binaxis="y", binwidth=.5, stackdir="center")


ggplot(heightweight, aes(x=sex, y=heightIn)) +
  geom_boxplot(outlier.colour=NA, width=.4) +
  geom_dotplot(binaxis="y", binwidth=.5, stackdir="center", fill=NA)


ggplot(heightweight, aes(x=sex, y=heightIn)) +
  geom_boxplot(aes(x=as.numeric(sex) + .2, group=sex), width=.25) +
  geom_dotplot(aes(x=as.numeric(sex) - .2, group=sex), binaxis="y",
               binwidth=.5, stackdir="center") +
  scale_x_continuous(breaks=1:nlevels(heightweight$sex),
                     labels=levels(heightweight$sex))

str(heightweight$sex)
as.numeric(heightweight$sex)



