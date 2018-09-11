 c<-"hello"
  d<-nchar(c)  # calculate length of string
  group<-"T01,T02,T03;T04,T05,T06;T09,T08,T07"
  c<-strsplit(group,split=";")  #based on ";" splited string
  c[[1]][1]  #access first element
  c[[1]][2]  #access second element
  d<-strsplit(c[[1]][1],split=",")  #based on "," splited string
  d[[1]][1]  #access first element
  d[[1]][2]  #access second element
  d[[1]][3]  ##access third element
  
  if(d[[1]][1] =="T01"){
   print("The element equal T01")
   break
 }
  
  










