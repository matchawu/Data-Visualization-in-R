library(ggplot2)
library(gcookbook)

p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
#主題項目外觀控制
p + theme(axis.title.x=element_text(size=16, lineheight=.9, family="Times",face="bold.italic", colour="red"))
p + ggtitle("Age and Height\nof Schoolchildren") +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times",
                                face="bold.italic", colour="red"))
p + annotate("text", x=15, y=53, label="Some text", size = 7, family="Times",fontface="bold.italic", colour="red")

p + geom_text(aes(label=weightLb), size=4, family="Times", colour="red")


p + theme(
  panel.grid.major = element_line(colour="red"),
  panel.grid.minor = element_line(colour="red", linetype="dashed", size=0.2),
  panel.background = element_rect(fill="lightblue"),
  panel.border = element_rect(colour="blue", fill=NA, size=2))


p + ggtitle("hihi") +
  theme(
    axis.title.x = element_text(colour="pink", size=14),
    axis.text.x = element_text(colour="red"),
    axis.title.y = element_text(colour="orange", size=14, angle = 90),
    axis.text.y = element_text(colour="green"),
    plot.title = element_text(colour="yellow", size=20, face="bold"))

# 圖例選項
p + theme(
legend.background = element_rect(fill="grey85", colour="blue", size=1),
legend.title = element_text(colour="blue", face="bold", size=14),
legend.text = element_text(colour="red"),
legend.key = element_rect(colour="blue", size=0.25))


p + facet_grid(sex ~ .) + theme(
  strip.background = element_rect(fill="lightblue"),
  strip.text.y = element_text(size=14, angle=-90, face="bold"))


mytheme <- theme_bw() +
theme(text = element_text(colour="red"),
axis.title = element_text(size = rel(1.25)))
# 基本
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
# 使用修改後的主題改圖
p + mytheme


library(gcookbook)
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p + theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank())

# 隱藏縱向網格線(=x軸)
p + theme(panel.grid.major.x = element_blank(),
panel.grid.minor.x = element_blank())

# 隱藏橫向網格線(=y軸)
p + theme(panel.grid.major.y = element_blank(),
panel.grid.minor.y = element_blank())




