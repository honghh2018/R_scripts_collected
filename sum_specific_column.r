#!/usr/bin/Rscript
args<-commandArgs(T)
infile1<-read.table(args[1],header=T,row.name=1)
infile2<-read.table(args[2],header=T,row.name=1)
result1<-sum(infile1$T3)
print (result1)    #bracket must be given
result2<-sum(infile2$T3)
print (result2)   #bracket must be given,auto newline，but cat not auto newline,you should add "\n"
cat("this is a test\n")
para<-length(args)
cat("para\n");  #output para and "\n"
cat(para)  #no auto newline,output value for para
#comment:double quotation represent a string not a variable value

#USAGE:
#Rscript sum_specific_column.r a.list b.list


