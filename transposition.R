library(getopt)

spec=matrix(c(
  'infile', 'i', 1, "character",
  'output', 'o', 1, "character",
  'help', 'h', 0, "logical"
),byrow=TRUE,ncol=4);
opt = getopt(spec);

in_file<-read.delim(opt$infile,sep="\t",row.names=0,header = TRUE,check.names=F)
result<-t(in_file)
write.table(result,file=opt$output,row.names=0,header = TRUE,check.names=F)
