
rm(list=ls())
setwd('C:\\Users\\Desktop\\R_study')
### mouse count data dowload

library(dplyr)
library(tibble)
raw_data<-read.table('GSE60450_Lactation-GenewiseCounts.txt.gz',header = T,sep = '\t',check.names = F,
                                                            comment.char = '',strip.white = TRUE)
fileName <- "edgeR/sampleinfo.txt"  ####read.table's strip.white=TRUE can replace multiple space into one
sampleinfo<-read.table(fileName,header = T,check.names = F,stringsAsFactors = F,strip.white = TRUE)
sampleinfo$SampleName<-gsub('[.]','-',sampleinfo$SampleName)
raw_data<-raw_data[,-2]
raw_data<-raw_data%>%column_to_rownames('EntrezGeneID')
rownames(raw_data)<-paste('Gene',rownames(raw_data),sep='')
### manipulatting sample info
sampleinfo$FileName<-substr(sampleinfo$FileName,start=1,stop=7)
colnames(raw_data)<-substr(colnames(raw_data),start=1,stop=7)
table(colnames(raw_data)==sampleinfo$SampleName)
### Filtering to remove lowly expressed genes
library(edgeR) ### including cpm function
library(Glimma)
library(gplots)

myCPM <- cpm(raw_data) # obtainning cpm from raw_data count by cpm function within edgeR package
cutoff<-myCPM>0.5
### we would like to keep genes that have at least 2 TRUES in each row of cutoff
keep <- rowSums(cutoff) >= 2
summary(keep)
raw_data_keep <- raw_data[keep,]
### testting 
# Let us limit the x and y-axis so we can actually look to see what is happening at the smaller counts
plot(myCPM[,1],raw_data[,1],ylim=c(0,50),xlim=c(0,3))
# Add a vertical line at 0.5 CPM
abline(v=0.2) # drawing vertical line
abline(h=5) # drawing horizontal line
### Using edgeR to generate DEGlist obj
temp.1 <- DGEList(raw_data_keep)
names(temp.1) ### [1] "counts"  "samples"
### Quality control
# Library sizes and distribution plots
temp.1$samples$lib.size
barplot(temp.1$samples$lib.size,names=colnames(temp.1),las=2)
# Add a title to the plot
title("Barplot of library sizes")
# Get log2 counts per million
log_counts <- cpm(temp.1,log=TRUE)
# Check distributions of samples using boxplots
boxplot(log_counts, xlab="", ylab="Log2 counts per million",las=2)
# Let's add a red horizontal line that corresponds to the median logCPM
abline(h=median(log_counts),col="red")
title("Boxplots of logCPMs (unnormalised)")

# Multidimensional scaling plots
plotMDS(temp.1)
# We specify the option to let us plot two plots side-by-sde
par(mfrow=c(1,2))
# Let's set up colour schemes for CellType
# How many cell types and in what order are they stored?
levels(factor(sampleinfo$CellType))
sampleinfo$CellType<-factor(sampleinfo$CellType)
sampleinfo$Status<-factor(sampleinfo$Status)
## Let's choose purple for basal and orange for luminal
col_cell <- c("purple","orange")[sampleinfo$CellType]
data.frame(sampleinfo$CellType,col_cell)

### 
# Redo the MDS with cell type colouring
plotMDS(temp.1,col=col_cell)
# Let's add a legend to the plot so we know which colours correspond to which cell type
#legend("topright",fill=c("purple","orange"),legend=levels(sampleinfo$CellType))
legend('right',fill=c("purple","orange"),legend=levels(sampleinfo$CellType))
# Add a title
title("Cell type")
### Normalisation for composition bias
# Apply normalisation to DGEList object
temp.2 <- calcNormFactors(temp.1)
### 
par(mfrow=c(1,2))
plotMD(log_counts,column = 7) # draw 7 column sample 
abline(h=0,col="grey")
plotMD(log_counts,column = 11) # draw 11 column sample
abline(h=0,col="grey")
### Differential expression with limma-voom
library(limma)
# Specify a design matrix without an intercept term
design <- model.matrix(~ 0 + sampleinfo$Status) #construct deg matrix 
colnames(design)<-levels(sampleinfo$Status)

### Voom transform the data
par(mfrow=c(1,1))
v <- voom(temp.2,design,plot = TRUE)
v$E
v$design
### We can repeat the box plots for the normalised data to compare to before normalisation. The expression values in v$E are already log2 values so we don’t need to log-transform.
par(mfrow=c(1,2))
boxplot(log_counts, xlab="", ylab="Log2 counts per million",las=2,main="Unnormalised logCPM")
## Let's add a blue horizontal line that corresponds to the median logCPM
abline(h=median(log_counts),col="blue")
boxplot(v$E, xlab="", ylab="Log2 counts per million",las=2,main="Voom transformed logCPM")
## Let's add a blue horizontal line that corresponds to the median logCPM
abline(h=median(v$E),col="blue")


### Testing for differential expression
# Fit the linear model
fit_result <- lmFit(v) # v including design embracing group
names(fit_result)

cont_matrix <- makeContrasts(B.PregVsLac=pregnant - lactate,levels=design)
### fit 
fit_cont <- contrasts.fit(fit_result, cont_matrix)
### The final step is to call the eBayes function
fit_cont <- eBayes(fit_cont)
dim(fit_cont)
### We can use the limma decideTests function to generate a quick summary of DE genes for the contrasts.
summa_fit <- decideTests(fit_cont)
summary(summa_fit) 
#B.PregVsLac
#Down            28
#NotSig       15693
#Up              83
eventual_result<-topTable(fit_cont,coef="B.PregVsLac",sort.by="p") ###top 10
### all result
tempOutput=topTable(fit_cont,coef=1,n=Inf) #getting all results
nrDEG=na.omit(tempOutput) #filtering na value

