rm(list=ls())
setwd('C:\\Users\\R_study\\t-SNE')
#data(iris)
#save(iris,file='iris_dimension_reduced.RData')
load('iris.RData')
#load('iris_dimension_reduced.RData')
### T-sne analysis
str(iris)
iris$Species<-factor(iris$Species)
colors = rainbow(length(unique(iris$Species)))
show_col(rainbow(length(unique(iris$Species))),labels=T)
### remove duplicated row
iris1<-iris[!duplicated.data.frame(iris),] #all working
unique1<-unique.data.frame(iris)

### analysis, if necessary,you should remove duplicated row in dataset of iris
tsne <- Rtsne(iris1[,1:ncol(iris)-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
### Plotting 
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=iris1$Species, col=colors[iris1$Species])

###PCA
pca_iris<-prcomp(iris1[,1:ncol(iris)-1])
### plotting
plot(pca_iris$x, t='n', main="pca")
text(pca_iris$x, labels=iris1$Species, col=colors[iris1$Species])

Finally,T-sne algorithm was better than pca algorithm for multiple dataset reducing


Supplementary:
MNIST dataset convertting code:
#!/usr/bin/env python
def convert(imgf, labelf, outf, n):
    f = open(imgf, "rb")
    o = open(outf, "w")
    l = open(labelf, "rb")

    f.read(16)
    l.read(8)
    images = []

    for i in range(n):
        image = [ord(l.read(1))]
        for j in range(28*28):
            image.append(ord(f.read(1)))
        images.append(image)

    for image in images:
        o.write(",".join(str(pix) for pix in image)+"\n")
    f.close()
    o.close()
    l.close()

if __name__=='__main__':
        convert("train-images-idx3-ubyte", "train-labels-idx1-ubyte","mnist_train.csv", 60000)
