AP=as.data.frame(AirPassengers)

Date=as.Date(vector())

for(i in 1:144){
  Date[i]=as.Date("1949-01-01")+i*30
}

Date=data.frame(Date=Date)
AP$Date=Date$Date

model=lm(data=AP,x~Date)

lm.prd=predict(model,Date,se.fit = T)

AP$lm=lm.prd$fit
AP$lm.se=lm.prd$se.fit

HW.add=HoltWinters(AirPassengers)
HW=as.data.frame(HW.add$fit)
HW.x=c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),HW$xhat)

AP$HW=HW.x

AP

pg<-AP
ggplot(AP,aes(x=x,y=lm))+geom_point(size=4, colour="pink", fill="pink")
ggplot(AP,aes(x=x,y=HW))+geom_point(size=4, colour="blue", fill="blue")


ggplot(AP,aes(x=x,y=Date))+geom_line(aes(lm=1))+geom_point(size=4)+geom_errorbar(aes(ymin=lm-lm.se,ymax=lm+lm.se),width=.2)

ggplot(AP,aes)
