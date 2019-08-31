rm(list=ls())
setwd('C:\\Users\\xiaohui\\Desktop\\R_study')
library(reshape2)
library(ggplot2)
#All_fullname_file <- dir('C:\\Users\\xiaohui\\Desktop\\*',pattern = NULL,all.files = FALSE,full.names = TRUE,recursive = TRUE)
All_fullname_file1<-Sys.glob(file.path('C:/Users/xiaohui/Desktop','*.xls'),dirmark =FALSE)
for(i in 1:length(All_fullname_file1)){
  cat(paste('basename->',basename(All_fullname_file1[i]),'\n',sep=''))
}
