rm(list=ls())
setwd('C:\\Users\\R_study\\clusterprofilers')

### GEO download
rm(list=ls())
suppressPackageStartupMessages(library(GEOquery)) ###ignoring tips
library(GEOquery)
gset <- getGEO("GSE42872", GSEMatrix =T, AnnotGPL=F,getGPL=F,destdir='.' ) ### AnnotGPL=F taking no annotation

group<-pData(gset[[1]]) ###garned sample information from list element [[1]]
library(stringr)
group1<-group[,1:2]
group1$type<-rep(c('Control','Vemurafenib'),each=3)
colnames(group1)[2]<-'variable' ### modified specified column names

### read.table read file with suffix .gz
test1<-read.table('./GSE42872_series_matrix.txt.gz',sep = '\t',comment.char = '!',
                  quote = '',header = T,fill = T)

a1=exprs(gset[[1]]) #obtainned matrix of expression
rownames(test1)<-test1[,1] ### tranfer first column into ID ,in general, pure digital numbers
test1<-test1[,-1]
#### probe transformation
#### BiocManager::install('pd.hugene.1.0.st.v1')
#### BiocManager::install('hugene10sttranscriptcluster')
suppressPackageStartupMessages(library(hugene10sttranscriptcluster.db)) ### the packages
ls("package:hugene10sttranscriptcluster.db") ### all including check
#str(hugene10sttranscriptcluster.db)
###
probe_symbols=toTable(hugene10sttranscriptclusterSYMBOL) ###get corresponding between symbol and probe ID
####then 
table(is.element(rownames(test1),probe_symbols$probe_id))
####filter same probe genes
test1<-test1[is.element(rownames(test1),probe_symbols$probe_id),]
###classified by symbol filtering same probe genes
tmp<-by(test1,probe_symbols$symbol,function(x) rownames(x)[which.max(rowMeans(x))]) ###requiring max genes
probes<-as.character(tmp)

##############################
##############################
### filter again
exprSet<-test1[rownames(test1)%in%probes,]
### plotting barplot through ggplot
library(ggplot2)
library(reshape2)
exprSet_m<-exprSet
exprSet_m$ID<-rownames(exprSet)
exprSet_m<-melt(exprSet_m,id.vars = c('ID'))
exprSet_m$variable<-gsub('X.','',exprSet_m$variable)
exprSet_m$variable<-gsub('[.]','',exprSet_m$variable)
exprSet_m<-merge(exprSet_m,group1,by=c('variable'),all=T)

p<-ggplot(exprSet_m)+geom_boxplot(aes(x=variable,y=value,fill=type))
####got sample evolutionary relationship
colnames(exprSet)<-substr(colnames(exprSet),3,12)
colnames(exprSet)<-paste(colnames(exprSet),1:6,sep='_')
hc<-hclust(dist(t(exprSet)))
plot(hc)
#### pca analysis
library(ggfortify)
df<-as.data.frame(t(exprSet))
df$group<-group1$type
autoplot(prcomp(df[,1:ncol(df)-1]),data=df,colour='group')

#### deg analysis，limma
group_list<-as.character(group1$type)
suppressPackageStartupMessages(library(limma))
design<-model.matrix(~0+factor(group_list)) ###output design matrix
colnames(design)<-levels(factor(group_list))
rownames(design)<-colnames(exprSet)
#### contrast matrix
contrast_matrix<-makeContrasts(paste0(unique(group_list),collapse = '-'),
                               levels=design)


#### start
#step1
fit<-lmFit(exprSet,design) ### expression and groupping matrix
#step2
fit1<-contrasts.fit(fit,contrast_matrix)
fit1<-eBayes(fit1) ### default no trend
#step3
tempout<-topTable(fit1,coef=1,n=Inf)  ###all results

#### following heatmap and enrichment analysis
#heatmap
library(pheatmap)
topgene<-head(rownames(tempout),n=30)
topgene_expr<-exprSet[topgene,]
topgene_expr=t(scale(t(topgene_expr))) ###scaling
topgene_expr<-log10(topgene_expr)
topgene_expr[which(is.nan(topgene_expr))]<-0
pheatmap(topgene_expr)
#### 
plot(tempout$logFC,-log10(tempout$P.Value))
tempout$regulate<-NA
library(ggplot2)
for (i in 1:nrow(tempout) ) {
  if ( as.double(tempout[i,ncol(tempout)-3]) <=0.05 && as.double(tempout[i,1]) >=2 ) {
    tempout[i,ncol(tempout)] <- "up"
  } else if ( as.double(tempout[i,ncol(tempout)-3]) <= 0.05 && as.double(tempout[i,1]) <=0-2 ) {
    tempout[i,ncol(tempout)] <- "down"
  } else {
    tempout[i,ncol(tempout)] <- "normal"
  }
}
p<-ggplot(tempout)+geom_point(aes(x=logFC,y=-log10(P.Value),color=regulate),alpha=0.4,size=1.75)
p<-p+theme_set(theme_bw(base_size = 20))+xlab('log2 fold change')+ylab('-log10 P_Value')
p<-p+ggtitle(c('volcano of all table'))+theme(plot.title = element_text(size=15,hjust = 0.5))
p<-p+scale_colour_manual(values=c('blue','black','red'))
#### hypergeometric test for enrichment
suppressPackageStartupMessages(library(clusterProfiler))
#### top 1000 genes to tranfer symbol into entrez ID
#### merge symbol ID
colnames(probe_symbols)[1]<-'ID'
tempout$ID<-NULL ###remove last column
tempout$ID<-rownames(tempout)
tempout1<-merge(tempout,probe_symbols,by='ID',all=T)                                                  
tempout2<-tempout1[complete.cases(tempout1),] ###abandon NA columns

### sort
tempsort<-tempout2[unlist(sort(tempout2$regulate,na.last=NA,decreasing = FALSE,index.return=TRUE)[2]),]
rownames(tempsort)<-tempsort$symbol
tempsort1<-tempsort[,-c(1,ncol(tempout1))]
gene<-head(rownames(tempsort1),1000)
#### bitr function working on tranfer symbol into entrezID
tran_gene_to_entrez<-bitr(gene,fromType = 'SYMBOL',toType = c('ENSEMBL','ENTREZID'),OrgDb ='hugene10sttranscriptcluster.db')
####start kegg enrichment,need more miniutes
kk<-enrichKEGG(gene=tran_gene_to_entrez$ENTREZID,
               organism = 'hsa',
               pvalueCutoff  =0.05)
head(kk)[,1:6]

#### plotting
data(geneList,package = 'DOSE') ###load data
View(geneList) ###fold change， rownames' entrez ID
#### tranferring entrez iD
symbol_id<-rownames(tempsort1)
symbol2entrezID<-bitr(symbol_id,fromType = 'SYMBOL',toType = c('ENSEMBL','ENTREZID'),OrgDb ='hugene10sttranscriptcluster.db')
combined<-merge(symbol2entrezID,tempsort,by.x='SYMBOL',by.y='symbol')
combined1<-combined$logFC
names(combined1)<-combined$ENTREZID
View(combined1)
combined1<-sort(combined1,decreasing = T) ###sort mandatory
####
kk2<-gseKEGG(geneList = combined1,
            organism='hsa',
            nPerm=1000,
            minGSSize = 120,
            pvalueCutoff = 0.05,
            verbose=FALSE)
gseaplot(kk2,geneSetID = 'hsa04145')







