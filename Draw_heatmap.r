require(gplots)
library(pheatmap)
library(RColorBrewer)
color <- colorRampPalette(c('green','black','red'))(10)
heatmap_data <- read.delim(file.choose(),header=T,row.names=1,stringsAsFactors=F,sep="\t")  #input data

my_log<-log10(heatmap_data)  #data processing with log10
pheatmap(my_log,method=pheatmap,color=color, cellwidth = 60,cellheight =0.2, cluster_rows = TRUE,cluster_cols = FALSE,annotation_names_row = FALSE,show_rownames = F)
