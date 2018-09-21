library(getopt)

spec=matrix(c(
  'fpkm', 'f', 1, "character",
  'trait', 't', 1, "character",
  'output', 'o',1, "character",
  'help', 'h', 0, "logical"
),byrow=TRUE,ncol=4);
opt = getopt(spec);


##
#if(is.null(opt$fpkm)||is.null(opt$trait)||is.null(opt$help)||is.null(opt$output)){
#  print("your input was wrong!,please try again!")
#  q(status=1)
#}
fpkm_file<-read.delim(opt$fpkm,sep="\t",header = TRUE,check.names=1)
#colnames(fpkm_file)<-read.delim(opt$fpkm,row.names=1,header=T,check.names=T,stringsAsFactors = F,nrows=1)

trait_data<-read.delim(opt$trait,sep="\t",header = TRUE,check.names=1)
#colnames(trait_data)<-read.delim(opt$trait,row.names=1,header=T,check.names=T,stringsAsFactors = F,nrows=1)

r_square<-NULL
pvalue <- NULL
gene_id<-(fpkm_file[,1])
for (i in 1:nrow(fpkm_file)){
  #print(length(as.numeric(fpkm_file[i,])))
  #print(length(as.numeric(trait_data[i,])))
  temp <- cor.test(as.numeric(fpkm_file[i,]),as.numeric(trait_data[i,]),method="pearson")
  pvalue[i] <- temp$p.value
  r_square[i]<-temp$estimate
}
Results <- data.frame(fpkm_file[,1],r_square,pvalue)
write.table(Results,file=opt$output,sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)
