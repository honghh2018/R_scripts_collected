rm(list=ls())
library(dplyr)
setwd('C:/Users/xiaohui/Desktop/R_study/')
myexpress<-Sys.glob(paths = 'C:\\Users\\*geneExpression.xls',dirmark = FALSE)
myexpress<-myexpress[order(myexpress)]
#head(myexpress)
#order(c(3,2,5,8),decreasing = T,method=c('auto'))
#rank(c(3,2,5,8)) #returning index of element，
#?order
i<-1
myallfpkm<-list()
myallcount<-list()
count_table<-read.delim(myexpress[i],sep = '\t',header = T,
                         check.names = F,
                         quote = '',
                         comment.char = '')[,c(1,2)]
while(i<=length(myexpress)){
  cat(paste0(myexpress[i],'\n'))
  cat(paste0(sub('[.]geneExpression[.]xls','',basename(myexpress[i])),'\n'))
  mydata<-read.delim(myexpress[i],sep = '\t',header = T,
                                  check.names = F,
                                  quote = '',
                                  comment.char = '')[,c(1,5)]
  colnames(mydata)<-sub('_FPKM','',colnames(mydata))
  myallfpkm[[i]]<-mydata
  mydata_count<-read.delim(myexpress[i],sep = '\t',header = T,
                     check.names = F,
                     quote = '',
                     comment.char = '')[,c(1,6)]
  colnames(mydata_count)<-sub('_Count','',colnames(mydata_count))
  myallcount[[i]]<-mydata_count
  i=i+1
}
?merge
j<-2
fpkm_table<-myallfpkm[[1]]
while(j<=length(myallfpkm)){
  fpkm_table<-merge(fpkm_table,myallfpkm[[j]],by.x='#GeneID',by.y='#GeneID')
  j=j+1
}
for(k in 1:length(myallcount)){
  count_table<-merge(count_table,myallcount[[k]],by.x='#GeneID',all = TRUE)
}
names(count_table)
count_table<-count_table %>% select('#GeneID',c(3:(length(myexpress)+2)),Length)

write.table(fpkm_table,file='Combined_FPKM.xls',sep='\t',
                      row.names = F,
                      col.names = T,
                      quote=FALSE)

write.table(count_table,file='Combined_count.xls',sep='\t',
            row.names = F,
            col.names = T,
            quote=FALSE)
