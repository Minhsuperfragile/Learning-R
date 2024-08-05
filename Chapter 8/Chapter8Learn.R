brand.ratings <- read.csv("http://goo.gl/IQl8nc")
write.csv(brand.ratings, file="brand-rating.csv")

setwd("/home/tminh/Documents/R/Learning-R/Chapter 8 /")
head(brand.ratings)
tail(brand.ratings)
library(car)
some(brand.ratings)

brand.ratings$brand = factor(brand.ratings$brand)
summary(brand.ratings)
str(brand.ratings)

scatterplotMatrix(brand.sc[brand.ratings$brand == "b",-10])

brand.sc = data.frame(scale(brand.ratings[,-10]))
brand.sc$brand = brand.ratings$brand
summary(brand.sc)

library(corrplot)
corrplot.mixed(cor(brand.sc[,-10]), order="hclust")

brand.mean = aggregate(. ~ brand, data=brand.sc, mean)
brand.mean

rownames(brand.mean) = brand.mean[,1]
brand.mean = brand.mean[,-1]

library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean), 
          col = brewer.pal(9,"GnBu"), trace = "none",
          key = FALSE, dendrogram = "none")

#perform PCA
brand.pca = prcomp(brand.sc[,-10])
summary(brand.pca)

plot(brand.pca, type='l')
biplot(brand.pca)

brand.mean.pc = prcomp(brand.mean, scale=TRUE)
summary(brand.mean.pc)
biplot(brand.mean.pc, main="Brand positioning", cex=c (1.5, 1))

library(nFactors)
nScree(brand.sc[,-10])
eigen(cor(brand.sc[,-10])) #pick number of eigenvalue > 1

factanal(brand.sc[,-10], factors= 2)
factanal(brand.sc[,-10], factors= 3)

library(GPArotation)
brand.fa.ob = factanal(brand.sc[,-10], factors=3, rotation = "oblimin")
brand.fa.ob

library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings, 
          col= brewer.pal(9,"GnBu"), trace="none", key = F, 
          dendrogram = "none", Colv=FALSE , cexCol = 1.2)

library(semPlot)
semPath(brand.fa.ob, what="est", residual=FALSE, cut=0.3, 
        posCol=c("white","darkgreen"), negCol=c("white","red"),
        edge.label.cex=0.75, nCharNodes=7)

brand.fa.ob = factanal(brand.sc[,-10], factors=3, rotation = "oblimin",
                       scores = "Bartlett")
brand.scores = data.frame(brand.fa.ob$scores)
brand.scores$brand = brand.sc$brand
head(brand.scores)

brand.fa.mean = aggregate(. ~ brand, data=brand.scores, mean)
rownames(brand.fa.mean) = brand.fa.mean[,1]
brand.fa.mean = brand.fa.mean[,-1]
names(brand.fa.mean) = c("Leader", "Value", "Latest")
brand.fa.mean

heatmap.2(as.matrix(brand.fa.mean), col = brewer.pal(9,"GnBu"), 
          trace = "none", key = F, dendrogram = "none",
          cexCol =1.2, main ="\n\n\n\n\ nMean factor score by brand")

brand.dist = dist(brand.mean[,-10])
brand.mds = cmdscale(brand.dist)
plot(brand.mds, type="n")
text(brand.mds , rownames( brand.mds), cex=2)

#non metric data
brand.rank <- data.frame(lapply(brand.mean , function(x) ordered( rank (x))))
str(brand.rank)
library(cluster)
brand.dist.daisy = daisy(brand.rank ,metric = "gower")

library(MASS)
brand.mds.nonmetric = isoMDS(brand.dist.daisy)
plot(brand.mds.nonmetric$points, type="n")
text(brand.mds.nonmetric$points, levels(brand.sc$brand), cex=2)
