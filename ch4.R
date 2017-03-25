library(ggplot2)
library(gcookbook)

p <- ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point()
p + ___________("text", x=3, y=48, label="Group 1")+
  ___________("text", x=4.5, y=66, label="Group 2")




p <- ggplot(data.frame(x=c(-3,3)), aes(x=x)) + stat_function(fun = dnorm)
p + annotate("text", x=2, y=0.3,________________,
             __________="frac(1, sqrt(2 * pi)) * e ^ {-x^2 / 2}")


p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
p + ___________(yintercept=60) + ___________(xintercept=14)
p + _____________(intercept=37.4, slope=1.75)
coef(lm(heightIn~ageYear  , data = heightweight))


hw_means <- ddply(heightweight, "sex", summarise, heightIn=mean(heightIn))
p + _______________(aes(____________=heightIn, ____________=sex), data=hw_means,
                    linetype="dashed", size=1)


pg <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_point()
pg + geom_vline(____________________)
pg + geom_vline(____________=____________(__________ (PlantGrowth$group)==¡§___________"))
                                          
                                          
                                          
                                          
                                          p <- ggplot(subset(climate, Source=="Berkeley"), aes(x=Year, y=Anomaly10y)) +
                                          geom_line()
                                          p + annotate("___________", x=1950, ________=1980, y=-.25, _________=-.25)
                                          p + annotate("___________", x=1850, ________=1820, y=-.8, _________=-.95, colour="blue",
                                          size=2, _____________________) +
                                          annotate"___________¡§,  x=1950, _________=1980, y=-.25, ________=-.25,
                                          ___________________(________________, ___________=90, length=unit(.2,"cm")))
                            
                            
                            
                            
                            p + annotate("___________", _______=1950, _______=1980, _______=-1, _______=1, alpha=.1, fill="blue")
                            
                            
                            pg <- PlantGrowth # Make a copy of the PlantGrowth data
                            pg$hl <- "no" # Set all to "no"
                            pg$hl[pg$group=="trt2"] <- "yes" # If group is "trt2", set to "yes"
                            ggplot(pg, aes(x=group, y=weight,_____________)) + geom_boxplot() +
                              ___________________(values=c("grey85", "#FFDDCC"), guide=FALSE)
                            ggplot(PlantGrowth, aes(x=group, y=weight,____________)) + geom_boxplot() +
                              ___________________(values=c("grey85", "grey85", "#FFDDCC"), guide=FALSE)
                            
                            
                            ce <- subset(cabbage_exp, Cultivar == "c39")
                            ggplot(ce, aes(x=Date, y=Weight)) +
                              geom_bar(stat="identity", fill="white", colour="black") +
                              _________________(aes(ymin=_____________, ymax=______________), width=.2)
                            ggplot(ce, aes(x=Date, y=Weight)) +
                              geom_line(aes(_____________=1)) +
                              geom_point(size=4) +
                              _________________(aes(ymin=_____________, ymax=______________), width=.2)
                            
                            
                            
                            
                            ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
                              geom_bar(stat="identity", position="dodge") +
                              geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),
                                            position="dodge", width=.2)
                            ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
                              geom_bar(stat="identity",position="dodge") +
                              geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),
                                            ____________________________, width=.2)
                            
                            
                            
                            
                            p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() + _________________(. ~ drv)
                            f_labels <- data.frame(drv = c("4", "f", "r"), label = c("4wd", "Front", "Rear"))
                            p + ______________(x=6, y=40, aes(label=label), data=f_labels)
                            p + ______________("text", x=6, y=42, label="label text")
                            