rm(list=ls())

library(RColorBrewer)
library(cluster)
library(reshape2)
library(dplyr)
library(ggplot2)

setwd('C:\\Users\\R')
### read expression data
exprSet<-read.delim('All_gene_fpkm.list',header = T,
                                        #row.names = 1,
                                        sep='\t',comment.char = '',
                                        quote = '',check.names = F)[1:2000,]

### read duplicated information
group_frame<-read.delim('group_list.txt',sep='\t',header = F,stringsAsFactors = F)
'''
V1	V2
ApEpC  A
ApHeC  A
ApEpY  B
ApMgY  B
ApHeY  C
ApMgC  C
'''
sample_names<-colnames(exprSet)

newdataframe<-exprSet[match(colnames(exprSet),as.character(group_frame[,1])),]
match_samples<-colnames(exprSet)[2:length(colnames(exprSet))][match(group_frame[,1],sample_names[2:length(sample_names)])] #return index, last subjected to first arrange
### sort column with previously vector match_samples
new_exprSet<-exprSet[
  ,with(exprSet, match_samples)
  ]

library(tibble) # including has_rownames function
has_rownames(new_exprSet)
new_exprSet<-remove_rownames(new_exprSet)
new_exprSet$ID<-exprSet[,1]
temp1 <- new_exprSet[, c(length(new_exprSet), 1:length(new_exprSet)-1)]
temp_group<-table(group_frame$V2)


getmeancol<-function(data_frame,ingroup,tab_group){
  j<-0
  new_data_frame<-data.frame(ID=data_frame[,1])
  for(i in unique(ingroup$V2)){
    j =j+as.numeric(tab_group[[i]])
    new_data_frame[,i]<-rowMeans(select(data_frame,j:j+1))
  }
  return(new_data_frame)
}

new_means_datafram<-getmeancol(temp1,group_frame,temp_group)
new_means_datafram<-exprSet



#colnames(exprSet)<-sub('(#ID|ID)','Gene',names(exprSet))
#exprSet_tibble<-as.tibble(exprSet) #转换为dplyr的tibble数据框
names(exprSet)[1]<-'Gene'
samplename<-1:(length(colnames(exprSet))-1)
names(samplename)<-colnames(exprSet)[2:length(colnames(exprSet))]
str(samplename)
#names(samplename)<-colnames(exprSet)[which(colnames(exprSet)!='ID')]

for(i in samplename){
  colnames(exprSet)[i+1]=i
}

rownames(exprSet)<-exprSet[,1]
exprSet1<-exprSet %>%select(2:7) #select 2-7heatmap.2(transposed_alpha_mtx,


exprSet_t<-as.data.frame(t(exprSet1))
exprSet_cor<-exprSet_t %>%   
  cor(use='pairwise.complete.obs',method='pearson')  #Obtain cor list，pairwise.complete.obs was parameters needed Miss values(NA)
### draw cor heatmap throught heatmap.2 function within gplots
if(!requireNamespace('gplots')) install.packages('gplots')
library(gplots) #including heatmap.2 function
color_scheme <- rev(brewer.pal(8,"RdBu")) 
pdf(file='heatmap.2.pdf',height = 10,width=8)
heatmap.2(exprSet_cor[1:500,1:500],na.rm=TRUE,
          cexRow=0.5, cexCol=0.5,
          Rowv = NULL,  # use the dendrogram previously calculated
          Colv = NULL, # don't mess with my columns! (keep current ordering )
          dendrogram = NULL,   # only draw row dendrograms
          breaks = seq(-3, 3, length.out = 9),  # OPTIONAL: set break points for colors
          col = color_scheme,  # use previously defined colors
          trace = "none", density.info = "none"  # remove distracting elements of plot
         )
dev.off()
exprSet_dist <- as.dist(1 - exprSet_cor) #deleting positive correlation from cor matrix by as.dist

fpkm_kmedoids <- pam(exprSet_dist, 8)
k_pam_clusters <- fpkm_kmedoids$cluster
sum(table(k_pam_clusters))

clusters_df <- data.frame(Gene = names(k_pam_clusters), 
                          cluster = as.factor(k_pam_clusters))

exprSet_t$ID<-rownames(exprSet_t)

exprSet_m<-melt(exprSet_t,id.vars="ID",variable.name="Gene",value.name="expression") 
exprSet_m$ID<-as.numeric(exprSet_m$ID)

exprSet_m<-exprSet_m %>%  
  left_join(clusters_df,by=c('Gene')) #left join the cluster_df into exprSet_m

cluster_means<-exprSet_m %>% group_by(cluster,ID) %>% 
  summarize(mean.exp=mean(expression,na.rm=TRUE))
options(warn=-1) #suppressing warning
p<-exprSet_m %>%
  ggplot(aes(ID, expression, group=Gene)) + 
  geom_line(alpha=0.25) + 
  geom_line(aes(ID, mean.exp, group=NULL,color=cluster),
            data = cluster_means,
            size=1.1) +
            ylim(0, 2) +  #y tick arrange
            facet_wrap(~cluster, ncol=4)+ 
            theme_bw()+
            scale_x_discrete(limits=c(names(samplename)))+
            xlab('SampleName')+
            theme(axis.text.x = element_text(face="bold", color="black", 
                            size=8, angle=45,margin=margin(8,0,0,0)), #margin funs adjustting space between x axis and x text
                  axis.text.y=element_text(face="bold", color="black",size=8))

ggsave(p,filename = 'K-medoids_Cluster.pdf',path='./',width=22,height=15,units='cm',colormodel='srgb')
?ggsave
### Done



