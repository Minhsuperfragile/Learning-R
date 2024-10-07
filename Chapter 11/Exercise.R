seg.ex.raw <- read.csv2("music-segment.csv", stringsAsFactors = T)[,-1]
seg.ex = seg.ex.raw
seg.ex$Segment = NULL

seg.summ <- function(data, group) {
  aggregate(data, list(group), function(x) mean(as.numeric(x)) )
}

library(cluster)
seg.dist = daisy(seg.ex)
as.matrix(seg.dist)[1:5,1:5]
seg.hc = hclust(seg.dist, method = "complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=0.48)$lower[[1]])

plot(seg.hc)
rect.hclust(seg.hc, k=3, border = 'red' )
seg.hc.kgroup = cutree(seg.hc, k=3)
table(seg.hc.kgroup)
plot(jitter(as.numeric(seg.ex$musicEnthuse)) ~
       jitter(as.numeric(seg.ex$subscribeToMusic)),
     col=seg.hc.kgroup , yaxt="n", xaxt="n", ylab="", xlab=""
)
axis(1, at=c(1, 2), labels =c(" Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2, 3, 4,5,6,7), labels =levels(seg.ex$musicEnthuse))

