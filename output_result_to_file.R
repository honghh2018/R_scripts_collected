what if you want to print the last result into your specified file,the follow scripts you let to know
sink("C:\\R_workshop\\test.txt",append=FALSE,split=FALSE)  #for windows platform,double slash must be given,if no,wrong
#comment:split　imply　that output no on green
a<-c(1,2)
a<-a*5
print(a)
sink()
#output style showed below:
[1]5 10

data<-read.table("test_test.txt",header=T,row.names=1)  #same as perl input handle
data<-data+10　　＃every element add ten via this scripts
write.table(data,file="this_is_a_test.txt",row.names=F,col.names=F) #same as perl output handle

sometime,you have lot of things  would interrupt youself that you should let it go by no mercy,because those things inevitable keep your live stuck.
so put them behind you when you move on.
this was not a scripts,just wrote something i want to say on trivial　daily live day by day.
in any case,your copy this scripts for your work,just used # ignored it.good luck my friends














