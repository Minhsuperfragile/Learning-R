#Handling high correlated data
cust.df = read.csv(file = "cust-df.csv", stringsAsFactors = TRUE)
write.csv(cust.df, file="cust-df.csv")

summary(cust.df)

spend.m1 = lm(online.spend ~ ., data = subset(cust.df[,-1], online.spend > 0))
summary(spend.m1)

autoTransform = function (x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

cust.df.bc = cust.df[complete.cases(cust.df), -1]
cust.df.bc = subset(cust.df.bc, online.spend > 0)
numcols = which(colnames(cust.df.bc) != "email")
cust.df.bc[,numcols] = lapply(cust.df.bc[,numcols], autoTransform)

summary(cust.df.bc)
library(car)
scatterplotMatrix(cust.df.bc[,-1])
#we observe that transaction and visit are highly correlated
#this cause the linear model to perform poorly

spend.m2 = lm(online.spend ~ . , data = cust.df.bc[,-1])
summary(spend.m2)

vif(spend.m2)
# VIF > 5 -> need to reduce colinearity


