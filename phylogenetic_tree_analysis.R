library("ape")
library("ggplot2")
library("ggtree")
library("treeio")
setwd('C:/Users/Desktop/R_study/')
dir.create('./phylogenetic_tree',mode = 0777)
setwd(paste('C:/Users/xiaohui/Desktop/R_study/','phylogenetic_tree',sep=''))
.libPaths()

nhx<-system.file("extdata/NHX/",'phyldog.nhx',package="treeio") ### get full pathway through partial directory and packages
###format of file
#(((Prayidae_D27SS7@2825365:0.0682841[&&NHX:Ev=S:S=58:ND=0],(Kephyes_ovata@2606431:0.0193941[&&NHX:Ev=S:S=69:ND=1],Chuniphyes_multidentata@1277217:0.0121378[&&NHX:Ev=S:S=70:ND=2]):0.0217782[&&NHX:Ev=S:S=60:ND=3]):0.0607598[&&NHX:Ev=S:S=36:ND=4],((Apolemia_sp_@1353964:0.11832[&&NHX:Ev=S:S=31:ND=9],(((Bargmannia_amoena@263997:0.0144549[&&NHX:Ev=S:S=37:ND=10],Bargmannia_elongata@946788:0.0149723[&&NHX:Ev=S:S=38:ND=11]):0.0925388[&&NHX:Ev=S:S=33:ND=12],Physonect_sp_@2066767:0.077429[&&NHX:Ev=S:S=61:ND=13]):0.0274637[&&NHX:Ev=S:S=24:ND=14],(Stephalia_dilata@2960089:0.0761163[&&NHX:Ev=S:S=52:ND=15],((Frillagalma_vityazi@1155031:0.0906068[&&NHX:Ev=S:S=53:ND=16],Resomia_ornicephala@3111757:1e-06[&&NHX:Ev=S:S=54:ND=17]):1e-06[&&NHX:Ev=S:S=45:ND=18],((Lychnagalma_utricularia@2253871:0.120851[&&NHX:Ev=S:S=65:ND=19],Nanomia_bijuga@717864:0.133939[&&NHX:Ev=S:S=71:ND=20]):1e-06[&&NHX:Ev=S:S=56:ND=21],Cordagalma_sp_@1525873:0.0693814[&&NHX:Ev=S:S=64:ND=22]):1e-06[&&NHX:Ev=S:S=46:ND=23]):0.0333823[&&NHX:Ev=S:S=40:ND=24]):1e-06[&&NHX:Ev=S:S=35:ND=25]):0.0431861[&&NHX:Ev=D:S=24:ND=26]):1e-06[&&NHX:Ev=S:S=19:ND=27],Rhizophysa_filiformis@3073669:0.22283[&&NHX:Ev=S:S=26:ND=28]):0.0292362[&&NHX:Ev=S:S=17:ND=29]):0.185603[&&NHX:Ev=D:S=17:ND=8],(Hydra_magnipapillata@52244:0.0621782[&&NHX:Ev=S:S=16:ND=5],Ectopleura_larynx@3556167:0.332505[&&NHX:Ev=S:S=15:ND=6]):0.185603[&&NHX:Ev=S:S=12:ND=7])[&&NHX:Ev=S:S=9:ND=30];
###
nhx <-read.nhx(nhx) ###read.nhx reading nhx function
ggtree(nhx, ladderize=F) + geom_tiplab() + geom_point(aes(color=S), size=8, alpha=.3) +
  theme(legend.position="right") +
  geom_text(aes(label=branch.length, x=branch), vjust=-.5) +
  xlim(NA, 0.3) 

###RAxML bootrap analysis
raxml_file <-system.file("extdata/RAxML", "RAxML_bipartitionsBranchLabels.H3", package="treeio")
raxml <-read.raxml(raxml_file)
ggtree(raxml) + geom_text(aes(label=bootstrap, color=bootstrap)) +
  scale_color_gradient(high='red', low='darkgreen') +
  theme(legend.position='right') 



###C:\Users\xiaohui\Documents\R\win-library\3.6\ggtree\examples
tree_suffix<-system.file('examples','MCC_FluA_H3.tree',package = 'ggtree')
beast_tree <- read.beast(tree_suffix)
###file pattern
# #NEXUS
# 
# Begin taxa;
# Dimensions ntax=76;
# Taxlabels
# A/Hokkaido/30-1-a/2013
# A/New_York/334/2004
# A/New_York/463/2005
# A/New_York/452/1999
# A/New_York/238/2005
# A/New_York/523/1998
ggtree(beast_tree, mrsd="2013-01-01") + theme_tree2()
###nag.txt
NAG_file <- system.file("examples/NAG_inHA1.txt", package="ggtree")
NAG.df <- read.table(NAG_file, sep="\t", header=FALSE, 
                     stringsAsFactors = FALSE)
NAG <- NAG.df[,2]
names(NAG) <- NAG.df[,1]
View(NAG)
## separate the tree by host species
tip <- get.tree(beast_tree)$tip.label

beast_tree <- groupOTU(beast_tree, tip[grep("Swine", tip)], 
                       group_name = "host")

p <- ggtree(beast_tree, aes(color=host), mrsd="2013-01-01", 
            yscale = "label", yscale_mapping = NAG) + 
  theme_classic() + theme(legend.position='none') +
  scale_color_manual(values=c("blue", "red"), 
                     labels=c("human", "swine")) +
  ylab("Number of predicted N-linked glycoslyation sites")

###


phy<-system.file('extdata/','sample.phy',package = 'treeio')
phylip <- read.phylip(phy) ###phy implying sequence
ggtree(phylip) + geom_tiplab()
msaplot(phylip, offset=1)
###file format:
# 15 41
# A  
# ATGGAAGACTTTGTGCGACAATGCTTCAATCCAATGATCGT
# B  
# ATGGAAGACTTTGTGCGACAATGCTTCAATCCAATGATCGTCGAGCTTGCGGAAAAG
nwk<-system.file('extdata','sample.nwk',package = 'treeio')
### file form
#(((((((A:4,B:4):6,C:5):8,D:6):3,E:21):10,((F:4,G:12):14,H:8):13):13,((I:5,J:2):30,(K:11,L:11):2):17):4,M:56);
write.table(nwk,file = 'phylogeneticTree.nwk',row.names = F,col.names = F,quote = F)
tree <- read.tree(file=nwk)
# build a ggplot with a geom_tree
ggplot(tree) + geom_tree() + theme_tree()

# This is convenient shorthand
ggtree(tree)
# add a scale
ggtree(tree) + geom_treescale()

# or add the entire scale to the x axis with theme_tree2()
ggtree(tree) + theme_tree2()

ggtree(tree, branch.length="none")

#######

# create the basic plot
p <- ggtree(tree)

# add node points
p + geom_nodepoint()

# add tip points
p + geom_tippoint()

# Label the tips
p + geom_tiplab()

###internal node and annotation
ggtree(tree) + geom_text(aes(label=node), hjust=-.3)

###clades specified
ggtree(tree) + 
  geom_tiplab() + 
  geom_cladelabel(node=17, label="Some random clade", 
                  color="red2", offset=.8, align=TRUE) + 
  geom_cladelabel(node=21, label="A different clade", 
                  color="blue", offset=.8, align=TRUE) + 
  theme_tree2() + 
  xlim(0, 70) + 
  theme_tree()+geom_text(aes(label=node),hjust=-.9)
### clades added colours
library(export)
ggtree(tree) + 
  geom_tiplab() + 
  geom_hilight(node=17, fill="gold") + 
  geom_hilight(node=21, fill="purple")
graph2ppt(file='phylogenetic_tree_in_nwk.pptx',width=10,heigh=15)
###draw circular phylogenetic tree
ggtree(tree, layout="circular") + ggtitle("(Phylogram) circular layout")
###
