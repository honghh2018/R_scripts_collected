#!/usr/bin/Rscript
args<-commandArgs(T)
infile1<-read.table(args[1],header=T,row.name=1)
infile2<-read.table(args[2],header=T,row.name=1)
result1<-sum(infile1$T3)
print result1
result2<-sum(infile2$T3)
print result2



#USAGE:
#Rscript sum_specific_column.r a.list b.list


