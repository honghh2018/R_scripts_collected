rm(list=ls())
library(DESeq2)
setwd('C:/Users/Desktop/')
###read group file like Con and Treat
IN_file <- file("C:\\Users\\Desktop\\deg_group.xls", 
                "r",
                encoding = "UTF-8",blocking = TRUE)
DEG_group<-readLines(IN_file,skipNul=TRUE,warn = FALSE)
close(IN_file)
colData<-NULL

for(i in 1:length(DEG_group)){
  tempdata<-unlist(strsplit(as.character(DEG_group[i]),split="[;|,]",perl=T))
  colData<-rbind(colData,tempdata)
}
colData<-as.data.frame(t(colData))
deg_group=rep(c('con','treat'),nrow(colData)-3,each=1)
deg_group<-deg_group[order(deg_group,decreasing = F)]
#(colData$Group<-group_list)
(colData<-data.frame(row.names = colData$tempdata,deg_group=deg_group))

###count files input
preread_col<-read.delim('All_gene_counts.list',sep='\t',header=T,
                        quote='',comment.char = '',
                        check.names = F,row.names=1,nrows = 1)
exprSet<-NULL
if('geneLength' %in% colnames(preread_col)){
  delcol<-'geneLength'
  exprSet=read.delim('All_gene_counts.list',sep='\t',header=T,
                     quote='',comment.char = '',
                     check.names = F,row.names = 1)
  
  exprSet<-exprSet[,!names(exprSet)%in% delcol]
}else{
  
  exprSet=read.delim('All_gene_counts.list',sep='\t',header=T,
                     quote='',comment.char = '',
                     check.names = F,row.names = 1)
}
exprSet=exprSet[apply(exprSet,1,function(x) sum(x>0)>ncol(exprSet)-1),] 
raw_data<-DESeqDataSetFromMatrix(countData = exprSet,colData = colData,
                            design = ~ deg_group) 
deg_step.1<-DESeq(raw_data)
res_deg<-results(deg_step.1,contrast = c("deg_group","con","treat"))
res_deg_Odered<-res_deg[order(res_deg$padj),]
DEG_eventual=as.data.frame(res_deg_Odered) 
DEG_eventual<-na.omit(DEG_eventual) 

DEG_eventual<-DEG_eventual[,c(2,5,6)]
colnames(DEG_eventual)<-c('log2FC','pvalue','qvalue')
DEG_eventual$ID<-row.names(DEG_eventual)

Up_regulate<-NULL
Down_regulate<-NULL
Normal_regulate<-NULL

for(i in 1:nrow(DEG_eventual)){
  if((DEG_eventual[i,1] >=2) & (DEG_eventual[i,3]<=0.01)){
    Up_regulate<-rbind(Up_regulate,c(rownames(DEG_eventual)[i],'Up'))
  }else if((DEG_eventual[i,1] <= -2) & (DEG_eventual[i,3]<=0.01)){
    Down_regulate<-rbind(Down_regulate,c(rownames(DEG_eventual)[i],'Down'))
  }else{
    Normal_regulate<-rbind(Normal_regulate,c(rownames(DEG_eventual)[i],'Normal'))
  }
}
combined_regulate<-as.data.frame(rbind(Up_regulate,Down_regulate,Normal_regulate))
colnames(combined_regulate)<-c('ID','regulated')
merge_table<-merge(DEG_eventual,combined_regulate,by=c('ID'))
merge_table<-merge_table[order(merge_table$regulated,decreasing = T),]

###output files
write.table(merge_table,file='All_Gene_DEG.xls',sep='\t',
                              row.names = T,
                              col.names = T,
                              quote=FALSE,fileEncoding = 'UTF-8')

write.table(merge_table[which(merge_table$regulated=='Up'),],file='Up_Gene_DEG.xls',sep='\t',
            row.names = F,
            col.names = T,
            quote=FALSE,fileEncoding = 'UTF-8')
write.table(merge_table[which(merge_table$regulated=='Down'),],file='Down_Gene_DEG.xls',sep='\t',
            row.names = F,
            col.names = T,
            quote=FALSE,fileEncoding = 'UTF-8')
stat_gene<-as.data.frame(table(merge_table$regulated))
colnames(stat_gene)<-c('Regulated','Gene_Numbers')
stat_gene<-stat_gene[order(stat_gene$Gene_Numbers),]
write.table(stat_gene,file='stat_Gene_DEG.xls',sep='\t',
            row.names = F,
            col.names = T,
            quote=FALSE,fileEncoding = 'UTF-8')
