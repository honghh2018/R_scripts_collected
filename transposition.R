library(getopt)

spec=matrix(c(
  'infile', 'i', 1, "character",
  'output', 'o', 1, "character",
  'help', 'h', 0, "logical"
),byrow=TRUE,ncol=4);
opt = getopt(spec);

in_file<-read.delim(opt$infile,sep="\t",header=FALSE,row.names=1) #if it have header and must be transposition,you should use header=FALSE,or header=TRUE
result<-t(in_file)
write.table(result,file=opt$output,sep="\t",quote=FALSE,row.names=FALSE)

＃＃advanced scripts
library(getopt)

spec=matrix(c(
  'infile', 'i', 1, "character",
  'output', 'o', 1, "character",
  'help', 'h', 0, "logical"
),byrow=TRUE,ncol=4);
opt = getopt(spec);

print_usage<-function(spec=NULL){  #formal parameter
	cat(getopt(spec,usage=TRUE));
	cat("Usage example:\n");
	cat("
		Usage example:
		Rscripts transposition.R -i <infile> -o <outfile>
	\n")
	q(status=1);
}
##start run the scripts##
if(is.null(opt$infile) ||is.null(opt$output)){print_usage(spec)}  #actual parameters

in_file<-read.delim(opt$infile,sep="\t",header=FALSE,row.names=1)
result<-t(in_file)
write.table(result,file=opt$output,sep="\t",quote=FALSE,row.names=FALSE)



