b<-as.matrix(read.table("All_gene_fpkm.list",header=T,row.name=1))
for (i in c(1:10)){
  c<-b[i,1]+b[i,2]+b[i,4]  #add ten line column[1,2,4] and mean them
  d<-c/3  #divided by three
  print(d) 
}


