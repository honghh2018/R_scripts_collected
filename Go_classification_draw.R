rm(list=ls())
setwd('C:\\Users\\huige\\Desktop\\R_study\\GOclasssification_draw')
Go_data<-read.delim('deg.GO_enrichment.stat.xls',sep = '\t',
                    comment.char = '#',check.names = F,header = F)


colnames(Go_data)<-c('GO_Classification','Terms','Total_genes','DEG','Genes')
total_gene<-Go_data[,1:3]
total_gene$type<-rep('Total_genes',nrow(total_gene))
colnames(total_gene)[3]<-'Gene_number'
deg_gene<-Go_data[,c(1:2,4)]
deg_gene$type<-rep('deg_gene',nrow(deg_gene))
colnames(deg_gene)[3]<-'Gene_number'
combined_data<-rbind.data.frame(total_gene,deg_gene)
temp_data<-Go_data[,1:4]
library(reshape2)
temp_m<-melt(temp_data,id.vars = 'Terms')
type<-combined_data$type
library(ggplot2)
library(ggthemes)  ##theme packages
#?rainbow
### new code
#classification1<-factor(unique(combined_data$GO_Classification),levels=c("cellular component", "molecular function","biological process"))
p<-ggplot(combined_data,aes(x=Terms,y=Gene_number))
p<-p+geom_bar(aes(fill=type),stat='identity',position = 'dodge',width=0.5)
p<-p + scale_y_continuous(sec.axis = sec_axis(~./5, name = "DEG Number"))
p<-p+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p<-p+theme(axis.text.x = element_text(family = "serif", angle = 45,
                        hjust=1,vjust=1,colour = "red"), 
                        legend.position = "right") +labs(fill = "Classification")

p<-p+geom_text(mapping = aes(label = paste0(combined_data$Gene_number, "")), vjust = -0.5) +
  #facet_grid(.~factor(combined_data$GO_Classification,levels=c('cellular component','molecular function','biological process')))
  facet_wrap(~factor(combined_data$GO_Classification,levels=c('cellular component','molecular function','biological process')),strip.position = "top", scales = "free_x",
             drop = FALSE,shrink = TRUE)
secy<-combined_data$Gene_number[which(combined_data$type=='deg_gene')]
p<-p+theme(strip.background = element_blank(), strip.placement = "outside")+xlab(NULL)+ylab('Gene Numbers')
