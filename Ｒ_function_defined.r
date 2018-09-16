/usr/bin/Rscript
sum<-function(x){
  mysum<-0  #the activating effect area
  for(i in 1:length(x)){
    mysum<-mysum+x[i]   #accumulating the values
  }
  print(mysum)
}
sum(c(1,2,3,6)) #invoking the function sum

##########################
usage<-function(x){
  cat(x)
  #q(status=1)
}

c<-"hello";
usage(c)








