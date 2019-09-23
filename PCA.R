######metabolome analysis
setwd('C:\\Users\\xiaohui\\Desktop\\R_study\\PCA')
meta_data<-read.delim('All_metabolite_profiles.xls',header = T,row.names = 1,
                                                    check.names = F,comment.char = '',
                                                    sep = '\t',quote = '')
meta_data_t<-t(na.omit(meta_data))
dim(meta_data_t)
### scaling by log2
meta_data_log<-log(meta_data_t,2)
meta_data_log[which(is.infinite(meta_data_log))]<-0.0 ### replaced Inf for zero
sample_names<-rownames(meta_data_log)

### read grouping file
group_info<-read.table( pipe(paste('sed "s/[[:blank:]]\\+/\\t/g" ', 'group_info.txt')),stringsAsFactors=F,header=as.logical(TRUE),check.names=F,comment.char='',sep='\t')
### format
#Sample	Group
#E30	E
#E31	E
#E32	E
#D26	D
#D28	D
#D29	D
#C23	C
#C24	C
#C25	C
#B16	B
#B20	B
#B21	B
#A12	A
#A15	A
#A18	A
match_samples<-match(sample_names,group_info[,1]) ### return index by sample_names
### pca
res_pca<-summary(prcomp(meta_data_log,center =as.logical(TRUE), scale. = as.logical(FALSE)))
res_pca1<-prcomp(meta_data_log)
### PCA line drawed
Proportion_of_Variance<-res_pca$importance[2,1:3]
X_lab<-sprintf("PC1(%.2f%%)",Proportion_of_Variance[1]*100)
Y_lab<-sprintf("PC2(%.2f%%)",Proportion_of_Variance[2]*100)
#Z_lab<-sprintf("PC3(%.2f%%)",Proportion_of_Variance[3]*100)
Group=as.character(group_info[match_samples,2])
df<-data.frame(res_pca$x[,1:2],group=as.character(group_info[match_samples,2]))


library(ellipse) ### adding line
library(ggplot2)
library(ggrepel) ### inlcuding geom_text_repel
pdf(file=paste0('./','2D','_pca.pdf'),height = 15,width = 10)
p<-ggplot(df, aes(x=PC1, y=PC2,color=group))
p<-p+geom_point(aes(shape=group))
p<-p+geom_path(data=df, aes(x=PC1, y=PC2,color=group), 
                                    size=1, linetype=2)
p<-p+geom_text_repel(aes(PC1, PC2,label = rownames(df)),
                          show.legend=F)+
                          xlab(X_lab) + ylab(Y_lab)

print(p)
dev.off()
