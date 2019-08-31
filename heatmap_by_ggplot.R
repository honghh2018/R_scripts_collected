#!/usr/bin/Rscript
library(getopt,quietly=TRUE)
library(ggplot2)
spec=matrix(c(
	'infile', 'i', 1, "character",
	'outdir', 'o', 1, "character",
	'Norm', 'N', 1, "character",
	'help', 'h', 0, "logical"
),byrow=TRUE,ncol=4)

opt=getopt(spec)
cat('Comment:Clustering rows and columns in Norm Z-score be chosen but not log10\n')
Usage<-function(){
	cat('Comment:Clustering rows and columns in Norm Z-score be chosen but not log10\n')
	stop(paste(c('Error:','\n',"Your input's files Error...",'\n','Example:','\n','heatmap_by_ggplot.R -i infile -o outdir -N Z-score or log10','\n'),collapse=""))
	q(status=1)	
}

if(is.null(opt$infile)||is.null(opt$outdir) ||is.null(opt$Norm) || ! is.null(opt$help)){
	Usage()
}

###dir creat
if(!file.exists(opt$outdir)){
	dir.create(opt$outdir)
}

library(reshape2,quietly=TRUE) #melt and dcast width to long

#read expression table
exprSet<-read.delim(opt$infile,header=T,
                        row.names = 1,check.names = FALSE,
                        comment.char='',sep='\t',quote='')

exprSet1<-exprSet[apply(exprSet,1,var)!=0,]
p<-NULL
if(opt$Norm=='Z-scorce'){
	exprSet_Z<-as.data.frame(t(apply(exprSet1,1,scale)))
	colnames(exprSet_Z)<-names(exprSet1)
	hc<-hclust(dist(exprSet_Z),method = "centroid") #row clustering
	RowIndex<-hc$order #keep order
	hc<-hclust(dist(t(exprSet_Z)),method = "centroid")  #column clustering
	ColIndex<-hc$order #keep order
	###adjustification of data frame
	exprSet_Z<-exprSet_Z[RowIndex,ColIndex]
	###Transform width data into long data
	exprSet_Z$ID<-rownames(exprSet1)
	exprSet_M<-melt(exprSet_Z,id.vars=c("ID"))
	cat('Data cleaning over\n')
	###plotting
	p<-ggplot(exprSet_M,aes(x=variable,y=ID))+geom_tile(aes(fill=value))
	p<-p+theme(axis.text.x= element_text(angle = 270,hjust=1,vjust=1))
	p<-p+scale_fill_gradient2(low='green',mid="black",high='red')
	p<-p+xlab(NULL)+ylab(NULL)
	p<-p+theme(legend.key=element_blank())+theme(axis.text.y = element_blank())
}

if(opt$Norm=='log10'){
	#exprSet1[exprSet1==0]<-0.000001
	exprSet_L<-log10(exprSet1+0.000001)
	exprSet_L$ID<-rownames(exprSet1)
	exprSet_M<-melt(exprSet_L,id.vars=c("ID"))
	p<-ggplot(exprSet_M,aes(x=variable,y=ID))+geom_tile(aes(fill=value))
	p<-p+theme(axis.text.x= element_text(angle = 270,hjust=1,vjust=1))
	p<-p+scale_fill_gradient2(low='green',mid="black",high='red')
	p<-p+xlab(NULL)+ylab(NULL)
	p<-p+theme(legend.key=element_blank())+theme(axis.text.y = element_blank())
}

###save files
ggsave <- ggplot2::ggsave
body(ggsave) <- body(ggplot2::ggsave)[-2]
ggsave(filename='heatmap.pdf',p,path=paste(c(opt$outdir,'/'),collapse=''),width=20,height=15,units=c("cm"),colormodel="srgb")
#pdf(file=paste(c(opt$outdir,'/','heatmap.pdf'),collapse=''),onefile=FALSE,width=15,height=15)
#print(p)
#dev.off()
