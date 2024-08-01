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
