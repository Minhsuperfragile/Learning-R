sat.df <- read.csv("http://goo.gl/HKnl74")
save(sat.df, file="sat-df-saved.RData")

summary(sat.df)
sat.df$weekend = factor(sat.df$weekend)

library(car)
scatterplotMatrix(sat.df)

sat.df$logdist = log(sat.df$distance)

library(corrplot)
corrplot.mixed(cor(sat.df[,c(2,4:9)]), upper="ellipse" )

#create a linear model
#with overall is y and rides is x
m1 = lm(overall ~ rides, data = sat.df)

#plot the model
plot(sat.df$rides,sat.df$overall, ylab="Rides", xlab="Overall")
abline(m1,col="blue")

#inspect the model
str(m1)
summary(m1)
m1$coefficients

#calculate 95% confidence interval
confint(m1)

#check the model
par(mfrow=c(2,2))
plot(m1)
#In the 2 left plots, the data points should be crammed together
#In the top right plot, the data should follow a line
#The bottom right plot is used to determine out liner, the Cook's distance should be a straight line

#create multi variable model
model.all = lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(model.all)
#higher R-sq = better model, less RSS = better model

#inspect the coefficient
library(coefplot)
coefplot(model.all, intercept=FALSE, outerCI = 1.96, lwdOuter = 1.5,
         ylab="Rating of feature",
         xlab="Association with Overall Satisfaction")
model.all$coefficients

#give prediction
coef(model.all)%*%c(1,100,100,100,100) #1 Is for intercept
predict(model.all,sat.df[1:10,])

#standardize variables
scale(sat.df$rides)

sat.std = sat.df[, -3]
sat.std[, 3:8] = scale(sat.std[, 3:8])
summary(sat.std)

#model with factors
model.factor = lm(overall ~ rides + games + wait + clean + logdist + weekend + num.child, data=sat.std)
summary(model.factor)

#factorize num.child
sat.std$num.child = factor(sat.std$num.child)
m4 = lm(overall ~ rides + games + wait + clean + logdist + weekend + num.child, data=sat.std)
summary(m4)        

#factorize children into has child or not
sat.std$has.child = factor(sat.df$num.child > 0)
m5 = lm(overall ~ rides + games + wait + clean + logdist + has.child, data=sat.std)
summary(m5)

#add interaction to model
m6 = lm(overall ~ . - num.child - weekend + wait:has.child, data=sat.std)
summary(m6)

coefplot(m6, intercept = FALSE, outerCI = 1.96, lwdOuter = 1.5,
         ylab="Rating of feature",
         xlab="Association with Overall Satisfaction")
#we can see that wait has a strong interaction with has child