install.packages("ggThemeAssist") #安装软件包
#可视化修图
require(ggplot2)
require(ggThemeAssist) #同时载入

p<-ggplot(data=mtcars,aes(x=mpg,y=wt, #x轴，y轴
                          color=factor(cyl)))+geom_point() #转因子

#theme函数进行修图
?theme #查看b帮助文档
ggThemeAssistGadget(p) #可视化修图
#自动生成代码如下
p + theme(plot.subtitle = element_text(hjust = 0.5, 
    vjust = 1), plot.caption = element_text(vjust = 1), 
    axis.line = element_line(colour = "burlywood3", 
        linetype = "solid"), axis.ticks = element_line(linetype = "twodash"), 
    panel.grid.major = element_line(colour = "hotpink3", 
        linetype = "blank"), panel.grid.minor = element_line(colour = "hotpink1", 
        size = 0.4, linetype = "blank"), 
    axis.title = element_text(family = "mono", 
        face = "bold"), axis.text = element_text(family = "Korea1deb", 
        size = 17, face = "italic", colour = "indianred2", 
        angle = 30), axis.text.x = element_text(family = "Bookman", 
        colour = "blue"), axis.text.y = element_text(angle = 5), 
    plot.title = element_text(family = "URWBookman", 
        face = "italic"), panel.background = element_rect(fill = "gray87"), 
    plot.background = element_rect(size = 3), 
    legend.key = element_rect(fill = "green2", 
        colour = "aquamarine1", size = 1, 
        linetype = "longdash"), legend.background = element_rect(fill = "khaki")) +labs(x = "ggg", y = "ttt", colour = "helloworld", 
    fill = "adtt", size = "adad", linetype = "dsds", 
    shape = "ttra", alpha = "dsdsad", subtitle = "helloworld", 
    caption = "hellochina")


p + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    panel.background = element_rect(fill = "khaki2"), 
    plot.background = element_rect(fill = "bisque2"), 
    legend.position = "none")
