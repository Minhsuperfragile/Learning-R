sales.data.raw <- read.csv('sale-data-raw.csv', stringsAsFactors = T)
sales.data.raw$coupon = factor(sales.data.raw$coupon)
sales.data.raw$purchase = factor(sales.data.raw$purchase)
sales.data.raw = sales.data.raw[,-1]
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
sales.data.bc[,c(-9,-10,-11)] = lapply(sales.data.raw[,c(-9,-10,-11)], autoTransform)
sales.data.bc = sales.data.bc[,c(1:4,9:11)]
sales.data.bc$sat = pc.sat$x[,1]

linear.m3 = lm(spendMonth ~ ., data = sales.data.bc)
summary(linear.m3)
vif(linear.m3)

#logistic regression

log.m1 = glm(purchase ~ coupon,data = sales.data.bc, family = binomial)
summary(log.m1)

log.m2 = glm(purchase ~ coupon + region + sat + spendToDate, data = sales.data.bc, family = binomial)
summary(log.m2)

log.m3 = glm(purchase ~ coupon*sat, data = sales.data.bc, family = binomial)
summary(log.m3)

exp(coef(log.m3)) #odd ratio

#hierarchical model
rm(list=ls())

conjoint.df <- read.csv("https://goo.gl/gEKSQt", stringsAsFactors = T)
summary(conjoint.df)

bag.lm <- lm(rating ~ price + color + zipper + finish, data=conjoint.df)
summary(bag.lm)

library(lme4)
bag.hlm1 = lmer(rating ~ price + color + zipper + finish + 
                  (price + color + zipper + finish | resp.id), 
                data=conjoint.df,
                control=lmerControl (optCtrl= list(maxfun =100000)))
summary(bag.hlm1)

fixef(bag.hlm1)                  # which coefs do we want?
sum(fixef(bag.hlm1)[c(1:6)]) 

str(ranef(bag.hlm1))
# find cutoffs for the top 5% and bottom 5%
quantile(ranef(bag.hlm1)$resp.id$colornavy, pr=0:20/20)
# get those IDs
which(ranef(bag.hlm1)$resp.id$colornavy < -1.361 | ranef(bag.hlm1)$resp.id$colornavy > 1.897)
