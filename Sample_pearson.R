library(getopt)
spec=matrix(c(
        "infile",'i',1,"character",
        "outfile",'o',1,"character",
        "help",'h',0,"logical"
),byrow=TRUE,ncol=4);
opt=getopt(spec);
print_err<-function(spec=NULL){
        cat(getopt(spec,usage=TRUE));
        cat("your input para was wrong!\n")
        q(status=1);
}

if(is.null(opt$infile)||is.null(opt$outfile)){print_err(spec)}

in_file<-read.delim(opt$infile,sep="\t",header=TRUE,row.names=1)
sample_cor<-cor(in_file,method="pearson",use="everything")
write.table(sample_cor,file=opt$outfile,sep="\t",quote=FALSE,header=T,row.names=1)
dev.off()
