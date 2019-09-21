
setwd("C:\\Desktop\\R_study\\DEG_EBSeq")
library(EBSeq)

count_data<-read.delim('genes_counts.list',header = T,row.names = 1,check.names = F,
                                              comment.char = '',sep='\t')
count_data<-as.matrix(count_data[,!(names(count_data)%in%"geneLength")][,1:6])  ### deleting geneLength
colnames(count_data)<-NULL  ### deleting column names 
### filter count_data
#rowSums(count_data[1:2,])
count_data1<-count_data[rowSums(count_data)>5,] ### sum of row 
### normalization
count_data_normal<-MedianNorm(count_data)

### Running EBSeq on gene expression estimates
names(count_data)
DEG_EBOUT<-EBTest(Data = count_data,Conditions = as.factor(rep(c('C','T'),each=3)),sizeFactors = count_data_normal,maxround = 10)

DEG_EBOUT$Alpha
DEG_EBOUT$Beta
DEG_EBOUT$P #maxround values determined two position substract less than 0.01
### get result
EBDEG_RESULT<-GetDEResults(DEG_EBOUT,FDR = 0.05) ### FDR 0.05

### garnered FDR
FDR <- DEG_EBOUT$PPMat[ ,1]

### get FC
Gene_fc<-PostFC(DEG_EBOUT)
summary(Gene_fc)
Gene_fc_log2<-log(Gene_fc$PostFC,2)
C1_Mean <- unlist(DEG_EBOUT$C1Mean)    #C1 group standardlizing 
C2_Mean <- unlist(DEG_EBOUT$C2Mean)    #C2 group standardlizing

### total table
EBSeq_total <- data.frame(C1_Mean, C2_Mean, Gene_fc, Gene_fc_log2, FDR)
### get DEG 
EBSeq_DE<-EBSeq_total[rownames(EBSeq_total)%in%EBDEG_RESULT$DEfound,]
### which
EBSeq_DE1<-EBSeq_total[which(rownames(EBSeq_total)%in%EBDEG_RESULT$DEfound),]
### draw volcano
rm(volcano_data)
FDR <- c(EBSeq_total$FDR)
log2FC <- c(EBSeq_total$Gene_fc_log2)
regulated<-rep(NA,length(log2FC))

volcano_data <- data.frame(FDR,log2FC,regulated)
#volcano_data$threshold = as.factor(abs(EBSeq_DE1$Gene_fc_log2) > 2 & EBSeq_DE1$FDR < 0.05)

##Construct the plot object
library(ggplot2)
# switch column "regulated" 
for (i in 1:nrow(volcano_data) ) {
  if ( as.double(volcano_data[i,ncol(volcano_data)-2]) <=0.05 && as.double(volcano_data[i,ncol(volcano_data)-1]) >=2 ) {
    volcano_data[i,ncol(volcano_data)] <- "up"
  } else if ( as.double(volcano_data[i,ncol(volcano_data)-2]) <= 0.05 && as.double(volcano_data[i,ncol(volcano_data)-1]) <=0-2 ) {
    volcano_data[i,ncol(volcano_data)] <- "down"
  } else {
    volcano_data[i,ncol(volcano_data)] <- "normal"
  }
}
### draw volcano plot
p <- ggplot(data=volcano_data, aes(x=log2FC, y=-log10(FDR), colour=regulated) ) + geom_point(size=1)
# adding line
p <- p + geom_vline(xintercept=c(-2,2), linetype="longdash", size=0.2)
p <- p + geom_hline(yintercept=c(-log10(0.05)), linetype="longdash", size=0.2) 
# adding title
p <- p + labs(list(title="Volcano Plot", x="log2(FC)"))
