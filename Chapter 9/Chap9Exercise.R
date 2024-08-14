sales.data.raw <- read.csv('sale-data-raw.csv', stringsAsFactors = T)
sales.data.raw$coupon = factor(sales.data.raw$coupon)
sales.data.raw$purchase = factor(sales.data.raw$purchase)

summary(sales.data.raw)

#high correlation
library(car)
scatterplotMatrix(sales.data.raw[,-9])

library(corrplot)
corrplot(cor(sales.data.raw[,-9]), upper = 'ellipse')
# satisfactions are highly correlated -> use pca

linear.m1 = lm(spendMonth ~ ., data = sales.data.raw)
vif(linear.m1)

pc.sat = prcomp(sales.data.raw[,c(5,6,7,8)])
sales.data.rmcor = sales.data.raw[,c(1:4,9:11)]
sales.data.rmcor$sat = pc.sat$x[,1]
sales.data.sc = data.frame(scale(sales.data.rmcor[,-5]))
sales.data.sc$region = sales.data.raw$region

linear.m2 = lm(spendMonth ~ ., data = sales.data.sc)
summary(linear.m2)
vif(linear.m2)

autoTransform <- function(x) { 
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

sales.data.bc = sales.data.raw
sales.data.bc[,-9] = lapply(sales.data.raw[,-9], autoTransform)
sales.data.bc = sales.data.bc[,c(1:4,9:11)]
sales.data.bc$sat = pc.sat$x[,1]

linear.m3 = lm(spendMonth ~ ., data = sales.data.bc)
summary(linear.m3)
vif(linear.m3)

#logistic regression


log.m1 = glm(data = )


