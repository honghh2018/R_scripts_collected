#!/usr/bin/Rscript
#header=T indicate your column name just a name,no to calculate.same as below row.name's mean
#as.mitrix mean transfer frame to mitrix,frame coule use infile1$T1,but matrix.use matrix mothed as infile[1,2] mean first row,second column
infile1<-as.matrix(read.table("All_gene_fpkm.list",header=T,row.name=1)) #row.name=1 state that make sure your form's name deleted(row name)
sum(infile1[c(1,2,4),c(1,2,4)]) #mean sum the first,second,fourth row and first,second,fourth column
sum(file1[,1])  #sum all row of first coulmn 

#print stairs
for (i in 1:10)
 print(1:i)
 
 #
 stats::rnorm(10) #right
rnorm(10) #right
for(n in c(10,20,30,40,50)){
  x<-stats::rnorm(n)
  cat(n,":",sum(x^2),"\n",sep="-->")
}



#loop read file:
c<-file("All_gene_fpkm.list","r")
while(TRUE){
  line<-readLines(c,n=1)   #n=1 read one line at a time (n=2 read two lines at a time)
  if(length(line)==0){  #when length of string was zero, print and break
    print("reached the file end")
    break
  }else{
    print(line)
  }
}


#calculte data frame row
line<-nrow(infile1) #infile1 must be read.table
print(line)

#statistic line and print STOUT
c<-file("All_gene_fpkm.list","r")
count<-0
while(TRUE){
  line<-readLines(c,n=1)
  if(length(line)==0){
    print("reached the file end")
    break
  }else{
    print(line)
    count <-count+1
  }






