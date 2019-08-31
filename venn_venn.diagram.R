#!/usr/bin/Rscript

args <-commandArgs(TRUE)

if(length(args) != 1){
	cat("Your input file error...please try below Example\n")
	stop("
		Example:
			Draw_Box_Plot.R infile \n
	   ")
}
library(reshape2)
library(VennDiagram)
#All_fullname_file <- dir('./*',pattern = NULL,all.files = FALSE,full.names = TRUE,recursive = TRUE)
All_fullname_file1<-Sys.glob(file.path(args[1],'*.xls'),dirmark =FALSE)
mydata_list<-NULL
samplecor<-NULL
x <- character()
y <- character()
for(i in 1:length(All_fullname_file1)){
  cat(paste('basename->',basename(All_fullname_file1[i]),'\n',sep=''))
  samplename=sub('[.]DEG[.]final[.]xls','',basename(All_fullname_file1[i]))
  if(nchar(samplename)>=5){
    x[i]<-samplename
    samplename=paste('DEG_',LETTERS[i])
    y[i]<-samplename
    #samplecor$Plotting_name[i]<-samplename
  }
  tempdata=read.delim(All_fullname_file1[i],
             header=F,sep = '\t',
             stringsAsFactors = F,check.names = F,
             quote='',skip = 1)
  mydata_list[[samplename]]<-tempdata[,1]
}
###write correspondent table
samplecor<-data.frame(DEG_group=x,Plotting_name=y)
write.table(samplecor,file='correspondenc.txt',quote=F,sep='\t',row.names = F,col.names = T)

venn_plot<-venn.diagram(x=mydata_list,filename = 'venn.tiff',
                        fill=rainbow(length(mydata_list)),
                        fontfamily = "serif",
                        fontface = 1,
                    alpha=0.1,cex=1,cat.cex=1,margin = 0.5)
