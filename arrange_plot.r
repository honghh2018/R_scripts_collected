library(gridExtra) #网格类型的包
install.packages("ggpubr")
library(ggpubr) #ggplot2升级版
mtcars
str(mtcars)
#加一行列名为name的
mtcars$name<-rownames(mtcars) #将行名作为这一列的数据
str(mtcars)

mtcars$cyl<-as.factor(mtcars$cyl) #cyl改成因子型
head(mtcars)

#绘制箱线图
head(ToothGrowth) #数据集
boxplot1<-ggplot(data=ToothGrowth,mapping=aes(x=factor(dose), #x轴，名称
                                              y=len, #y轴，名称，b不指定的话默认是列名
                                              color=factor(dose), #因子
                                              palette="jco"))+
                                              geom_boxplot()


#绘制点图
dp<-ggplot(data=ToothGrowth,aes(x=factor(dose), #剂量作为x轴的数据，默认x轴名称也是这个列名
                                y=len,
                                color=factor(dose), #颜色作为剂量标识
                                fill=factor(dose)))+
                              geom_dotplot(binwidth=1,binaxis = "y",stackdir = "center",
                                           position = "dodge")     #气泡大小
#绘制条形图
#用mtcars     ###x横轴，y纵轴，填充色
bp<-ggbarplot(data=mtcars,x="name",y="mpg",fill="cyl",
              color="white", #点颜色
              palette="jco", #整体配色方案
              sort.val="asc", #排序方法
              sort.by.groups=TRUE, #排序，ggplot2y一般不能排序，而ggbarplot函数可以
              x.text.angle=90 #设置文本的显示角度
              )
bp+font("x.text",size=8) #设置字体


##将多个图形排在一起
ggarrange(boxplot1,
          dp,
          bp, #三个图形对象（boxplot1,dp,bp）
          labels=c("A","B","C"), #图例
          ncol=3,
          nrow=1)  #需要这个包gridExtra



ggarrange(boxplot1,
          dp,
          #rremove("x.text")删除x轴内容
          bp+rremove("x.text"), #三个图形对象（boxplot1,dp,bp） #+rremove("x.text")去除第三张图的横坐标的文字说明，否者图被挤没了
          labels=c("A","B","C"), #图例
          ncol=1, #列数
          nrow=3)  #需要这个包gridExtra（行数）




#绘制散点图，含有边际密度
#ggscatter在ggpubr中
head(iris)
str(iris)
sp<-ggscatter(data=iris,x="Sepal.Length", #横轴和纵轴的对应数据集中的列名。
              y="Sepal.Width",color="Species", #Species是因子
              palette="jco",size=3,alpha=0.6)+border()

#绘制密度图
xplot<-ggdensity(data=iris,x="Sepal.Length",
                 fill="Species",palette="jco")

#去掉主题
xplot<-xplot+clean_theme()



yplot<-ggdensity(iris,"Sepal.Width",fill="Species",palette = "jco")+rotate() #加上rotate涂层，反转
yplot<-yplot+clean_theme()
#组合图形,xplot,NULL,sp,yplot这四张图，其中NULL是空的，占位;align对齐;width是图片宽度，length是高度
ggarrange(xplot,NULL,sp,yplot,ncol=2,
          nrow=2,
          align="hv", #对齐，坐标对齐，hv是horizone,vertical的所写
          widths=c(2,1), #宽度和高度
          heights=c(1,2),
          common.legend=TRUE) 



#生成图文本表格的组合
#绘制密度图

density.p<-ggdensity(iris,
                     x="Sepal.Length",
                     fill="Species",palette="jco")

#生成表格,统计Sepal.Length列的统计值，中位数，均值等等
stable<-desc_statby(iris,measure.var = "Sepal.Length",grps="Species")

stable<-stable[,c("Species","length","mean","sd")] #选者"Species","length","mean","sd"这些数据

#ggpubr包函数,表格变成图片表格
stable.p<-ggtexttable(stable,rows=NULL,theme=ttheme("mOrange")) #theme是图片标题
#生成文本类型的图片
text<-paste("iris data set gives the measurement in cm","of iris","data test",sep=" ") #文本粘帖一起
#文字生成图形
text.p<-ggparagraph(text=text,face="italic",size=11,
                    color="black")


#组合图片
ggarrange(density.p,stable.p,text.p,ncol=1,nrow=3,
          heights=c(1,0.5,0.3), #设置三个图形的高度
          align="v") #垂直 v
