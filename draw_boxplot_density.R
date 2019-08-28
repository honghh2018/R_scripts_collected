#!/usr/bin/Rscript
library(getopt)
# command parameter parsing
args <-commandArgs(TRUE)

if(length(args) != 2){
	cat("Your input file error...please try below Example\n")
	stop("
		Example:
			Draw_Box_Plot.R infile outfile prefix\n
	   ")
}

####mkdir directory
if(file.exists(args[2])){
        rm_outdir=paste('rm -rf',args[2],sep=' ')
        system(rm_outdir)
}
dir.create(args[2])


#get file
if(!is.null(args[3])){
	args[3]="Draw"
}

All_fullname_file <- dir(path=args[1],pattern = NULL,all.files = FALSE,full.names = TRUE,recursive = TRUE)
for(i in 1:length(All_fullname_file)){
	cat(paste('Abspath',All_fullname_file[i],'\n',sep=''))
}



#data process
library(stringr)
All_fullname_file <- dir(path=args[1],pattern = NULL,all.files = FALSE,full.names = TRUE,recursive = TRUE)
splitname<-NULL
total_data<-NULL
total_name<-NULL
for(i in 1:length(All_fullname_file))
{
    	splitname[i]<-gsub('[.]txt','',basename(All_fullname_file[i]))
	data1<-read.table(All_fullname_file[i],header = T,row.names = 1,sep='\t',check.names = FALSE,stringsAsFactors = F)
  data1<-data1[,3]
  logical_mark<-data1>0
  save_line=data1[logical_mark]
  
  total_data<-c(total_data,log10(save_line))
  total_name<-c(total_name,rep(splitname[i],length(save_line)))
}

#Produce data frame
log10rpkm<-data.frame(log10rpkm=total_data,samplenames=total_name)
Sample=factor(total_name)

#draw boxplot 
library(ggplot2)
m <- ggplot(log10rpkm, aes(factor(Sample), log10rpkm))+labs(title = "RPKM Distribution")
pb <- m + geom_boxplot(aes(fill=Sample)) + xlab("Sample") + ylab("log10(RPKM)")
pb <- pb + theme(axis.title.x = element_text(face="bold", size=14),
               axis.text.x  = element_text(face="bold", size=12),
               axis.title.y = element_text(face="bold", size=14),
               axis.text.y  = element_text(face="bold", size=12))

pb=pb+theme(legend.position='none',axis.text.x=element_text(angle = 30,vjust=1,hjust=1))

pb <- pb + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            plot.title = element_text(hjust = 0.5))

#####save plot
png(file=paste(args[2],'/',args[3],'_','boxplot.png',sep=''),
					height = 3000, width = 3400, 
					res = 500, units = "px")
print(pb)
dev.off()
pdf(file=paste(args[2],'/',args[3],'_','boxplot.pdf',sep=''),
				    onefile=FALSE,                                       
				    height = 10, width = 8)
print(pb)
dev.off()

#draw density plot
###draw density

pd<-ggplot(log10rpkm, aes(x=log10rpkm, fill=Sample)) + geom_density(alpha=.3)+labs(title = "RPKM Density Distribution")+ylab("Density")
pd<-pd+theme(axis.title.x = element_text(face="bold", size=14),
      axis.text.x  = element_text(face="bold", size=12),
      axis.title.y = element_text(face="bold", size=14),
      axis.text.y  = element_text(face="bold", size=12))
pd <- pd + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.title = element_text(hjust = 0.5))

#####save plot
png(file=paste(args[2],'/',args[3],'_','density.png',sep=''),
                                        height = 3000, width = 3400,
                                        res = 500, units = "px")
print(pd)
dev.off()
pdf(file=paste(args[2],'/',args[3],'_','density.pdf',sep=''),
                                    onefile=FALSE,
                                    height = 10, width = 8)
print(pd)
dev.off()

####################################

library(stringr)
#install.packages('xts')
#library(xts)
setwd('C:\\Users')
All_fullname_file <- dir(path='All_sample_quantify',pattern = NULL,all.files = FALSE,full.names = TRUE,recursive = TRUE)


splitname<-NULL
total_data<-NULL
total_name<-NULL
for(i in 1:length(All_fullname_file))
{
  splitname[i]<-unlist(strsplit(unlist(strsplit(All_fullname_file[i],"/")[[length(strsplit(All_fullname_file[i],"/"))]])[2],"[.]")[[1]])[1]
  data1<-read.table(All_fullname_file[i],header = T,row.names = 1,sep='\t',check.names = FALSE,stringsAsFactors = F)
  data1<-data1[,3]
  logical_mark<-data1>0
  save_line=data1[logical_mark]
  log10rpkm<-data.frame(log10rpkm=log10(save_line))
  
  total_data<-c(total_data,log10(save_line))
  total_name<-c(total_name,rep(splitname[i],length(save_line)))
}

#Produce data frame
log10rpkm<-data.frame(log10rpkm=total_data,samplenames=total_name)
Sample=factor(total_name)

library(ggplot2)
m <- ggplot(log10rpkm, aes(factor(Sample), log10rpkm))+labs(title = "RPKM Distribution")
pb <- m + geom_boxplot(aes(fill=Sample)) + xlab("Sample") + ylab("log10(RPKM)")
pb <- pb + theme(axis.title.x = element_text(face="bold", size=14),
                 axis.text.x  = element_text(face="bold", size=12),
                 axis.title.y = element_text(face="bold", size=14),
                 axis.text.y  = element_text(face="bold", size=12))

pb=pb+theme(legend.position='none',axis.text.x=element_text(angle = 30,vjust=1,hjust=1))

pb <- pb + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.title = element_text(hjust = 0.5))

###draw density

pd<-ggplot(log10rpkm, aes(x=log10rpkm, fill=Sample)) + geom_density(alpha=.3)+labs(title = "RPKM Density Distribution")
pd<-pd+theme(axis.title.x = element_text(face="bold", size=14),
             axis.text.x  = element_text(face="bold", size=12),
             axis.title.y = element_text(face="bold", size=14),
             axis.text.y  = element_text(face="bold", size=12))
pd <- pd + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.title = element_text(hjust = 0.5))

#######################
library(stringr)
setwd('C:\\Users')
All_fullname_file <- dir(path='.',pattern = NULL,all.files = FALSE,full.names = TRUE,recursive = TRUE)

combine_data<-NULL
splitname<-NULL
total_data<-NULL
total_name<-NULL
for(i in 1:length(All_fullname_file))
{
  splitname[i]<-str_split(str_split(All_fullname_file[i],"/",simplify=TRUE)[2],"[.]",simplify = TRUE)[1]
  data1<-read.table(All_fullname_file[i],header = T,row.names = 1,sep='\t',check.names = FALSE,stringsAsFactors = F)
  data1<-data1[,3]
  logical_mark<-data1>0
  save_line=data1[logical_mark]
  log10rpkm<-data.frame(log10rpkm=log10(save_line))
  
  total_data<-c(total_data,log10(save_line))
  total_name<-c(total_name,rep(splitname[i],length(save_line)))
}

#Produce data frame
log10rpkm<-data.frame(log10rpkm=total_data,samplenames=total_name)
Sample=factor(total_name)

library(ggplot2)
m <- ggplot(log10rpkm, aes(factor(Sample), log10rpkm))+labs(title = "RPKM Distribution")
pb <- m + geom_boxplot(aes(fill=Sample)) + xlab("Sample") + ylab("log10(RPKM)")
pb <- pb + theme(axis.title.x = element_text(face="bold", size=14),
               axis.text.x  = element_text(face="bold", size=12),
               axis.title.y = element_text(face="bold", size=14),
               axis.text.y  = element_text(face="bold", size=12))

pb=pb+theme(legend.position='none',axis.text.x=element_text(angle = 30,vjust=1,hjust=1))

pb <- pb + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            plot.title = element_text(hjust = 0.5))

###draw density

pd<-ggplot(log10rpkm, aes(x=log10rpkm, fill=Sample)) + geom_density(alpha=.3)+labs(title = "RPKM Density Distribution")
pd<-pd+theme(axis.title.x = element_text(face="bold", size=14),
      axis.text.x  = element_text(face="bold", size=12),
      axis.title.y = element_text(face="bold", size=14),
      axis.text.y  = element_text(face="bold", size=12))
pd <- pd + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.title = element_text(hjust = 0.5))

?theme



