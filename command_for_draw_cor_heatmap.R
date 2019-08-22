#!/share/nas2/genome/bin/Rscript
library(pheatmap)
library(RColorBrewer)
library(stringr)
library(getopt)


spec=matrix(c(
  'infile', 'i', 1, "character",
  'outfile', 'o', 1, "character",
  'dekey', 'k', 1, "character",
  'help', 'h', 0, "logical"
),byrow=TRUE,ncol=4)
opt = getopt(spec)

####function definition####
usage<-function(x){
  cat(x)
  cat('Uasge Example:\n')
  cat("
        Rscript pheatmap.R -i <infile> -o <outdir> -k <dekey>
        Options:
                 --infile -i get correlation matrix file [forces]
                 --outfile -o out directory [forces]
		 --dekey -k slicing the key [options] 
                 --help -h calling help
   ")
  q(status=1)
}

####main####
if(is.null(opt$infile) && is.null(opt$outfile) && is.null(opt$help)){
	err<-"\tYour input Error,please try again...\n"
	usage(err)	
}
if(file.exists(opt$outfile)){
	#file.remove(opt$outfile)
	rm_outdir=paste('rm -rf',opt$outfile,sep=' ')
	system(rm_outdir)
}
dir.create(opt$outfile)


data<-read.table(opt$infile,header=T,row.names=1,stringsAsFactors=F,comment.char='',check.names=F,quote='',sep='\t') 
data1<-t(data)
rowname_save=rownames(data)
colname_save=colnames(data)
data2<-as.data.frame(cor(data1))
data3<-t(data2[60:80,1:50])
if(!is.null(opt$dekey)){
	colnames(data3)<-gsub(opt$dekey,'',colnames(data3))
	cat(paste(paste(paste("substitute for",opt$dekey,sep=' '),'down',sep=' '),'\n',sep=''))
	write.table(data3,file=paste(opt$outfile,'cor_data_modify.xls',sep='/'),sep='\t',quote=FALSE,row.names=TRUE,col.names=TRUE)
	pdf(file=paste0(opt$outfile,'/','cor_heatmap_modify.pdf'),onefile=FALSE,width=15,height=10)
	pheatmap(data3,cellheight=8,cellwidth=15,
                            border=FALSE,
                            frontsize_row=5,
                            frontsize_col=5,
                            scale='row')
	dev.off()
}else{
	write.table(data3,file=paste(opt$outfile,'cor_data.xls',sep='/'),sep='\t',quote=FALSE,row.names=TRUE,col.names=TRUE)
	pdf(file=paste0(opt$outfile,'/','cor_heatmap.pdf'),onefile=FALSE,width=15,height=10)
	pheatmap(data3,cellheight=8,cellwidth=15,
                            border=FALSE,
                            frontsize_row=5,
                            frontsize_col=5,
                            scale='row')
	dev.off()

}
#pheatmap(data3,cellheight=5,cellwidth=5,border=FALSE,
#					frontsize_row=4,frontsize_col=4,
#					scale='row',color=colorRampPalette(rev(c("#ff0000", "#000000", "#00ff00")))(100))
