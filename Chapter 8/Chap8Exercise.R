prst.df = read.csv("https://goo.gl/z5P8ce")
write.csv(prst.df, file="prst.csv")

summary(prst.df)
prst.df$Brand = factor(prst.df$Brand)

library(car)
scatterplotMatrix(prst.df[,-10])

prst.sc = data.frame(scale(prst.df[,-10]))
prst.sc$Brand = prst.df$Brand

scatterplotMatrix(prst.sc)
library(corrplot)
corrplot.mixed(cor(prst.sc[,-10]),order = "hclust" ,upper="ellipse")

prst.mean = aggregate(. ~ Brand, data=prst.sc, mean)
prst.mean

rownames(prst.mean) = prst.mean[,1]
prst.mean = prst.mean[,-1]

library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(prst.mean), 
          col = brewer.pal(9,"GnBu"), trace = "none",
          key = FALSE, dendrogram = "none")

#pca
prst.pca = prcomp(prst.mean[,-10])
summary(prst.pca)
#need 3 PCs to present the data
plot(prst.pca, type='l')

biplot(prst.pca, choices = 2:3)

#efa
library(nFactors)
nScree(prst.sc[,-10])
eigen(cor(prst.sc[,-10])) #-> use 3 factors

factanal(prst.sc[,-10], factors = 3) #independent factors
library(GPArotation)
prst.ob = factanal(prst.sc[,-10], factors=3, rotation = "oblimin")
prst.ob
#they are pretty much the same -> no correlation

heatmap.2(prst.ob$loadings, 
          col= brewer.pal(9,"GnBu"), trace="none", key = F, 
          dendrogram = "none", Colv=FALSE , cexCol = 1.2)
prst.ob.scores = factanal(prst.sc[,-10], factors=3, rotation = "oblimin",
                          scores = "Bartlett")
prst.scores = data.frame(prst.ob.scores$scores)
str(prst.scores)
prst.scores$Brand = prst.sc$Brand
prst.scores.mean = aggregate(. ~ Brand, data=prst.scores, mean)

rownames(prst.scores.mean) = prst.scores.mean[,1]
prst.scores.mean = prst.scores.mean[,-1]
names(prst.scores.mean) = c("Intuitive", "Exciting", "Best Value")
View(prst.scores.mean)

heatmap.2(as.matrix(prst.scores.mean), 
          col = brewer.pal(9,"GnBu"), trace = "none",
          key = FALSE, dendrogram = "none",
          Colv=FALSE , cexCol = 1.2)

#mds
prst.dist = dist(prst.mean)
prst.mds = cmdscale(prst.dist)
plot(prst.mds, type='n')
text(prst.mds , rownames( prst.mds), cex=1)

