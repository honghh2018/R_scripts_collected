what if you want to print the last result into your specified file,the follow scripts you let to know
sink("C:\\R_workshop\\test.txt",append=FALSE,split=FALSE)  #for windows platform,double slash must be given,if no,wrong
#comment:split　imply　that output no on green
a<-c(1,2)
a<-a*5
print(a)
sink()
#output style showed below:
[1]5 10











