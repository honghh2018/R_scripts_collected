rm(list=ls())
if(!require(DESeq)){
  BiocManager::install('DESeq')
}
library(DESeq)
library(dplyr)
setwd(gsub("\\\\","/","C:\\Users\\Desktop\\R_study\\DEG_DESeq"))
exprSet<-read.delim('Drosophila_RNA-Seq_data_counts.xls',header = T,
                                                        sep = '\t',
                                                        comment.char = '',
                                                        check.names = F,
                                                        row.names = 1)

colnames(exprSet)<-sub('[.]bam','',names(exprSet))

sample_table<-read.delim('Drosophila_RNA-Seq_sample.xls',header = T,
                                                        sep = '\t',
                                                        comment.char = '',
                                                        check.names = F,
                                                        row.names = 1,
                                                        stringsAsFactors = F)
DEG_group<-sample_table[order(sample_table$Group),]
exprSet_select<-exprSet%>%select(rownames(DEG_group))
DEG_group1<-sample_table[order(sample_table$Group),][,1]

#DEG Analysis
DEG_step1=newCountDataSet(exprSet_select,DEG_group1)
DEG_step2=estimateSizeFactors(DEG_step1)
DEG_step3=estimateDispersions(DEG_step2)
DEG_res=nbinomTest(DEG_step3,"Untreated","Treated")

DEG_res_order<-DEG_res%>%arrange(padj) 

write.table(DEG_res_order,file='./All_Gene_DEG_table.xls',sep = '\t',
                    quote = FALSE,
                    row.names = F)
