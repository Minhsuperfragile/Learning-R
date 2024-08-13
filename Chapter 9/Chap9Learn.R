#Handling high correlated data
cust.df = read.csv("http://goo.gl/PmPkaG")
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
# VIF > 5 -> need to reduce co linearity

#omit some variables
spend.m4 = lm(online.spend ~ . - online.trans - store.trans,
              data = cust.df.bc)
vif(spend.m4)

#use PCA
pc.online = prcomp(cust.df.bc[,c('online.visits','online.trans')]) 
cust.df.bc$online = pc.online$x[,1]
pc.store = prcomp(cust.df.bc[,c('store.spend', 'store.trans')])
cust.df.bc$store = pc.store$x[,1]

spend.m5 = lm(online.spend ~ email + age + credit.score +
                distance.to.store + sat.service + 
                sat.selection + online + store, 
              data = cust.df.bc)
summary(spend.m5)
vif(spend.m5)

#Logistic regression (binary outcome)
pass.df = read.csv("http://goo.gl/J8MH6A", stringsAsFactors = T)
write.csv(pass.df, file='pass.csv')
summary(pass.df)

pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list( Channel=c("Mail", "Park", "Email"),
                             Promo =c("Bundle", "NoBundle"),
                             Pass =c("YesPass", "NoPass") )
pass.tab

library(vcdExtra)

pass.df = expand.dft(pass.tab)
pass.df$Promo = factor(pass.df$Promo , levels =c("NoBundle", "Bundle"))
pass.df$Channel = factor(pass.df$Channel)
pass.df$Pass = factor(pass.df$Pass)
str(pass.df)
View(pass.df)
table(pass.df$Pass, pass.df$Promo)

pass.m1 = glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)

exp(0.3888) # Bundle increase the purchase likelihood by 47,5%
exp(coef(pass.m1))
exp(confint(pass.m1)) #confident interval

library(vcd)
doubledecker(table(pass.df))

pass.m3 = glm(Pass ~ Promo + Channel + Promo:Channel, 
              data = pass.df, family = binomial)
summary(pass.m3)
exp(confint(pass.m3))

#Hierarchical model
conjoint.df <- read.csv(file="conj-df.csv", stringsAsFactors = TRUE)[,-1]
conjoint.df$speed = factor(conjoint.df$speed)
conjoint.df$height = factor(conjoint.df$height)
str(conjoint.df)
summary(conjoint.df)

by(conjoint.df$rating, conjoint.df$height, mean)
std.lm = lm(rating ~ speed + height + const + theme, data=conjoint.df)
summary(std.lm)

library(lme4)
ride.hlm1 = lmer(rating ~ speed + height + const + theme + (1 | resp.id), data=conjoint.df)
summary(ride.hlm1)

fixef(ride.hlm1)
head(ranef(ride.hlm1)$resp.id)
head(coef(ride.hlm1)$resp.id)

ride.hlm2 = lmer(rating ~ speed + height + const + theme +
                   (speed + height + const + theme | resp.id), 
                 data = conjoint.df, 
                 control = lmerControl(optCtrl= list(maxfun =100000)))
summary(ride.hlm2)

fixef(ride.hlm2)
head(ranef(ride.hlm2)$resp.id)
head(coef(ride.hlm2)$resp.id)
