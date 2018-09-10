b<-as.matrix(read.table("All_gene_fpkm.list",header=T,row.name=1))
for (i in c(1:10)){
  c<-b[i,1]+b[i,2]+b[i,4]  #add ten line column[1,2,4] and mean them
  d<-c/3  #divided by three
  print(d) 
}

#regular expression 
#check c vector whether having character a or u or not
a<-grep("[au]",c("Equator","North pole","South pole")) #return index,regular expression
c("Equator","North pole","South pole")[a]  #extraction corresponding element of index 











