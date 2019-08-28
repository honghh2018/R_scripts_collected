library(ggplot2,quietly=TRUE)
library(reshape,quietly=TRUE)
setwd('C:\\Users\\xiaohui\\Desktop\\生信笔试题\\')
data1<-read.delim('kegg_pathway_enrichment.txt',header=T,
                                                #row.names = 1,
                                                sep='\t',
                                                check.names = F)
data2<-data1[1:20,-c(2,3,seq(ncol(data1)-2,ncol(data1),2))]
data2$Rich_Factor<-round(data2$`Input number`/data2$`Background number`,3)
data2<-rename(data2,c("Input number"='Gene_Number',"#Term"="KEGG_Terms","P-Value"="PValue"))



?scale_colour_gradientn
###draw plot dot
p<-ggplot(data2, aes(Rich_Factor,y=reorder(KEGG_Terms, Rich_Factor) ) )
p<-p+geom_point(aes(colour=PValue,size=Gene_Number))+
  scale_colour_gradient(low = "green", high = "red",guide = "colourbar") +
  expand_limits(color=data2$PValue)


p<-p+ggtitle("Statistics of Pathway Enrichment") + xlab("Rich Factor") +ylab("KEGG Terms")+theme_bw()
p<-p+theme(panel.border=element_rect(colour = "black"))
p<-p+theme(plot.title=element_text(vjust=1), legend.key=element_blank())

p <- p + theme(title = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10))
p <- p + theme(legend.title = element_text(face="bold", size=10),legend.text = element_text(size=10) )


################################
