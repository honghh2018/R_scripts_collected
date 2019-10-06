rm(list=ls())
options(Warnings=F)
if(!requireNamespace('export')){
  install.packages("export")
}else{
  library(export)
}
setwd('C:\\Users\\R_study\\powerpoint_format')
library(ggplot2)
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))     
graph2ppt(file="ggplot2_plot.pptx", width=6, height=5) 
