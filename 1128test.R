names(iris)
iris$score=iris$Sepal.Width*8+iris$Petal.Width*3+2.8*iris$Petal.Length

iris$price=iris$Sepal.Width*-5.3+iris$Petal.Width*-2.1+2.8*iris$Petal.Length+iris$Sepal.Length*5.5

iris$level=cut(iris$score,breaks=5,label=c("drop","poor","normal","good","perfect"))

iris1=iris[,-c(1:4,6)]

iris1

p <- ggplot(iris1, aes(x=level, y=price)) + geom_line() 

ggplot(iris1,aes(x=level,y=price))+geom_dotplot()+ylim(0,max(iris1$price))
iris1

p <- ggplot(iris1,aes(x=price,fill=level))+geom_bar()+ylim(0,1)
p+coord_flip()