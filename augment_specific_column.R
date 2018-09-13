

#read file and tranfer to matrix
mydata<-as.matrix(read.table("All_gene_fpkm.list",header=F,sep="\t")) #read in T01, T02 and so on matrix
myhead_id<-mydata[1,] #get column name
mydata<-mydata[-1,]  #delete first row
myres<-matrix(NA,nrow=nrow(mydata),ncol=ncol(mydata))
for (i in 2:nrow(myres)){
  myres[1,1]<-colnames(c(myhead_id[1]),paste(myhead_id[2],myhead_id[3],sep="_"),
                       paste(myhead_id[4],myhead_id[5],paste(myhead_id[6],myhead_id[7])))  #get T01_T02 format
  myres[i,1]=mydata[i,1] #get gene_id
  myres[i,2]=as.numeric(mydata[i,2])+as.numeric(mydata[i,3])    #sum of two column
  myres[i,3]=as.numeric(mydata[i,4])+as.numeric(mydata[i,5])    #"as.matrix()"tranfer column string into numeric
  myres[i,4]=as.numeric(mydata[i,6])+as.numeric(mydata[i,7])
}

write.table(myres,file="result.txt",append=FALSE,quote=FALSE,col.names=TRUE,sep="\t")
