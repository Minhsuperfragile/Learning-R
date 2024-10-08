seg.raw <- read.csv("seg-raw.csv", stringsAsFactors = T)[,-1]
seg.df <- seg.raw[ , -8]

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

#cut dendogram for k groups
plot(seg.hc)
rect.hclust(seg.hc, k=4, border = 'red' )

seg.hc.kgroup = cutree(seg.hc, k=4)
table(seg.hc.kgroup) # 4 group with size 124, 136, 18,22

seg.summ(seg.df, seg.hc.kgroup)

plot(jitter(as.numeric(seg.df$gender)) ~
     jitter(as.numeric(seg.df$subscribe)),
     col=seg.hc.kgroup , yaxt="n", xaxt="n", ylab="", xlab=""
     )
axis(1, at=c(1, 2), labels =c(" Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels =levels(seg.df$gender))

#k-mean clustering
#only used for numeric so we try to coerce the data to numeric
seg.df.num = seg.df
seg.df.num$gender = ifelse(seg.df.num$gender == "Male",0,1)
seg.df.num$ownHome = ifelse(seg.df.num$ownHome == "ownNo", 0,1)
seg.df.num$subscribe = ifelse(seg.df.num$subscribe == "subNo", 0,1)

set.seed(2710)
seg.k = kmeans(seg.df.num, centers=4)
seg.summ(seg.df, seg.k$cluster)

boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")

clusplot(seg.df, seg.k$ cluster , color=TRUE , shade=TRUE ,
         labels=4, lines=0, main="K-means cluster plot")

#mclust 
library(mclust)
seg.mc = Mclust(seg.df.num)
summary(seg.mc)

seg.mc4 = Mclust(seg.df.num, G=4)
summary(seg.mc4)

BIC(seg.mc, seg.mc4)
mclustBIC(seg.df.num)
seg.summ(seg.df.num ,seg.mc$classification)

clusplot(seg.df, seg.mc$class , color=TRUE , shade=TRUE ,
         labels=4, lines=0, main="Model - based cluster plot")

#polca
seg.df.cut = seg.df
seg.df.cut$age = factor(ifelse(seg.df$age < median(seg.df$age), 1, 2))
seg.df.cut$income = factor(ifelse(seg.df$income < median(seg.df$income), 1, 2))
seg.df.cut$kids =  factor(ifelse(seg.df$kids < median(seg.df$kids), 1, 2))

summary(seg.df.cut)

seg.f = with(seg.df.cut , cbind(age , gender , income , kids , ownHome , subscribe)~1)
library(poLCA)

seg.lca3 = poLCA(seg.f, data=seg.df.cut, nclass = 3)
seg.lca4 = poLCA(seg.f, data=seg.df.cut, nclass = 4)

seg.summ(seg.df, seg.lca3$predclass)

#compare solution
library(mclust)
mapClass(seg.lca3$predclass , seg.lca4$predclass)
adjustedRandIndex(seg.lca3$predclass, seg.lca4$predclass)

#compare to true segments
adjustedRandIndex(seg.raw$Segment, seg.lca4$predclass)

#naive bayes
set.seed(95)
train.prop = 0.65
train.cases = sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train = seg.raw[train.cases , ]
seg.df.test =  seg.raw[-train.cases , ]

library(e1071)
(seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train))

#give predict
(seg.nb.class = predict(seg.nb, seg.df.test))

prop.table(table(seg.nb.class))
clusplot(seg.df.test[, -7], seg.nb.class , color=TRUE , shade=TRUE ,
         labels=4, lines=0, 
         main = "naive bayes classification, test data")

mean(seg.df.test$ Segment==seg.nb.class) # accuracy 
adjustedRandIndex(seg.nb.class , seg.df.test$Segment) 
table(seg.nb.class , seg.df.test$Segment) #confusion matrix

#random forest classification
library(randomForest)
set.seed(2710)
seg.rf = randomForest(Segment ~ ., data=seg.df.train, ntree=3000)

seg.rf.class = predict(seg.rf, seg.df.test)
clusplot(seg.df.test[, -7], seg.rf.class , color=TRUE , shade=TRUE ,
         labels=4, lines=0, 
         main = "random forest classification, test data")

seg.rf.class.all <- predict(seg.rf, seg.df.test , predict.all=TRUE)
apply(seg.rf.class.all$individual [1:5,], 1,
      function(x) prop.table(table(x)))
mean(seg.df.test$Segment==seg.rf.class)
table(seg.df.test$ Segment , seg.rf.class)
adjustedRandIndex(seg.df.test$Segment , seg.rf.class)

# variables importance
set.seed(1234)
(seg.rf <- randomForest(Segment ~ ., data=seg.df.train , ntree =3000, importance = TRUE))
importance(seg.rf)
varImpPlot(seg.rf, main=" Variable importance by segment")

library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[,1:4]),
          col=brewer.pal(9, "Blues"),
          dendrogram = "none", trace="none", key = FALSE,
          margins = c(10,10), main = "Variables importance by segment")
