library(getopt)
spec=matrix(c(
  'conf', 'c', 1, "string",
  'data', 'd', 1, "string",
  'help', 'h', 0, "logical"
),byrow=TRUE,ncol=4);
#command
opt = getopt(spec);
if(!is.null(opt$conf)||!is.null(opt$data)||!is.null(opt$help)){
  #err<-"your input was wrong!,please try again!"
 #cat(err)
  cat("1")
}

#usage<-function(x){
#  cat(x)
 # q(status=1)
#}



####comment

library(getopt) #loading getopt
#?getopt  #get help for getopt

spec = matrix(c(
  'help', 'h', 0, 'logical',
  'data', 'd', 1, 'integer',
  'file', 'f', 1, 'integer'  #comma not gived at last row
),byrow=TRUE,ncol=4); #must be semicolon
#command
opt = getopt(spec);  #must be semicolon
if(!is.null(opt$conf)||!is.null(opt$data)||!is.null(opt$help)){
  err<-"your input was wrong!,please try again!"
  usage(err)
}

usage<-function(x){
  cat(x)
  q(status=1)
}
