customers <- read.csv("bikeshops.csv")
products <- read.csv("bikes.csv") 
orders <- read.csv("orders.csv") 

library(dplyr)
orders.extended <- merge(orders, customers, by.x = "customer.id", by.y="bikeshop.id")
orders.extended <- merge(orders.extended, products, by.x = "product.id", by.y = "bike.id")

orders.extended <- orders.extended %>%
  mutate(price.extended = price * quantity) %>%
  select(order.date, order.id, order.line, bikeshop.name, model,
         quantity, price, price.extended, category1, category2, frame) %>%
  arrange(order.id, order.line)

library(tidyr)  
customerTrends <- orders.extended %>%
  group_by(bikeshop.name, model, category1, category2, frame, price) %>%
  summarise(total.qty = sum(quantity))

customerTrends <- customerTrends %>%
  spread(bikeshop.name, total.qty)
customerTrends[is.na(customerTrends)] <- 0

library(Hmisc)  #to use cut2
customerTrends$price <- cut2(customerTrends$price, g=2)  

customerTrends.mat <- as.matrix(customerTrends[,-(1:5)])  # bỏ 5 cột đầu tiên
customerTrends.mat <- prop.table(customerTrends.mat, margin = 2)  # column-wise pct
customerTrends <- bind_cols(customerTrends[,1:5], as.data.frame(customerTrends.mat))

kmeansDat <- customerTrends[,-(1:5)]
kmeansDat.t <- t(kmeansDat)

library(cluster)
set.seed(11)
km.res <- kmeans(kmeansDat.t, centers = 3, nstart = 50)
print(km.res)

km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 4      # số cụm nhỏ nhất là 4
maxClust <- 8      # số cụm lớn nhất là 8

km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 4      # số cụm nhỏ nhất là 4
maxClust <- 8      # số cụm lớn nhất là 8

for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # bắt đầu i = 1
  set.seed(11) 
  km.out[i] <- list(kmeans(kmeansDat.t, centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(kmeansDat.t)))
  # tính silhouette average widths
  x[i] = centr  
  y[i] = summary(sil.out[[i]])[[4]] 
}

library(ggplot2)
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Số cụm") +
  ylab("Silhouette") +
  ggtitle("Silhouette khi thay đổi số cụm")

maxSilRow <- which.max(y)          # Row number of max silhouette value
optimalClusters <- x[maxSilRow]    # Number of clusters
km.out.best <- km.out[[maxSilRow]] # k-means output of best cluster

# Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for (clustr in 1:optimalClusters) {
  clusterNames[clustr] <- paste0("X", clustr)
  clusterList[clustr] <- list(
    names(
      km.out.best$cluster[km.out.best$cluster == clustr]
    )
  )
}
names(clusterList) <- clusterNames

print(clusterList)

custSegmentCntrs <- t(km.out.best$centers)  # Get centroids for groups
colnames(custSegmentCntrs) <- make.names(colnames(custSegmentCntrs))
customerTrends.clustered <- bind_cols(customerTrends[,1:5], as.data.frame(custSegmentCntrs))

attach(customerTrends.clustered)
customerTrends.clustered[order(-X1), c(1:5, 6)]
