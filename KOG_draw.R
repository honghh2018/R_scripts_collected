rm(list=ls())
library(ggplot2)
setwd('C:\\Users\\xiaohui\\Desktop\\R_study')
kog_data<-read.delim('kog_class.stat.xls',header = T,sep='\t',
                                                                    check.names = F)
                                                                    #ID	Class_Name	Numbers
# J	Translation, ribosomal structure and biogenesis	1720
# A	RNA processing and modification	1324
# K	Transcription	1763
# L	Replication, recombination and repair	827
groups<-paste0(kog_data[,1],':',kog_data[,2])

p<-ggplot(data=kog_data,aes(x=`#ID`,y=Numbers))+geom_bar(aes(fill=groups),stat="identity")
p<-p+ theme(legend.key.size=unit(0.2, "cm"))+guides(fill=guide_legend(ncol=1))
p<-p+labs(x="Function Class", title='KOG Function Classification of Consensus Sequence',tag='A')
p <-p+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p<-p+theme(legend.title = element_blank())
p<-p+theme(plot.title = element_text(hjust = 0.5))
