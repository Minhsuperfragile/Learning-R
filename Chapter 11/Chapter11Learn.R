seg.raw <- read.csv("seg-raw.csv", stringsAsFactors = T)
seg.df <- seg.raw[ , c(-1,-8)]

seg.summ <- function(data, group) {
  aggregate(data, list(group), function(x) mean(as.numeric(x)) )
}

seg.summ(seg.df, seg.raw$Segment)

#hclust clustering
library(cluster)
seg.dist = daisy(seg.df)
as.matrix(seg.dist)[1:5,1:5]

seg.hc = hclust(seg.dist, method = "complete")
plot(seg.hc) #plot dendrogram (tree)

plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]]) #cut dendrogram into smaller piece
cor(cophenetic(seg.hc), seg.dist) #metric for model, >0.7 is very good

