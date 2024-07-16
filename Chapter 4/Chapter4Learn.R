setwd("/home/tminh/Documents/R/Learning-R/Chapter 4")
setwd("G:/Code/R/Chapter 4")

#set random seed as usual
set.seed(27104)
nCustomer = 1000
cust.df = data.frame(cust.id=as.factor(c(1:nCustomer)))

#set other variables
cust.df$age = rnorm(n=nCustomer, mean=35, sd=5)
cust.df$credit.score = rnorm(n=nCustomer, mean=3*cust.df$age+620, sd=50)
cust.df$email = factor(sample(c("yes","no"), size=nCustomer, replace=TRUE, prob=c(0.8,0.2)))
cust.df$distance.to.store = exp(rnorm(n=nCustomer, mean=2, sd=1.2))
summary(cust.df)

#set online data
cust.df$online.visit = rnbinom(n=nCustomer, size=0.3,
                               mu=15 + ifelse(cust.df$email=="yes",15,0) 
                               - 0.7*(cust.df$age-median(cust.df$age))) #mu=mean
cust.df$online.trans = rbinom(n=nCustomer, size=cust.df$online.visit, prob=0.3)
cust.df$online.spend = exp(rnorm(n=nCustomer, mean=3, sd=0.1)) * cust.df$online.trans

#set offline data
cust.df$store.trans = rnbinom(n=nCustomer, size=5,
                              mu= 3/sqrt(cust.df$distance.to.store))
cust.df$store.spend = exp(rnorm(n=nCustomer, mean=3.5, sd=0.4)) * cust.df$store.trans
summary(cust.df)

#satisfaction simulation
sat.overall = rnorm(n=nCustomer, mean=3.1, sd=0.7)
sat.service = floor(sat.overall + rnorm(n=nCustomer, mean=0.6, sd=0.2))
sat.selection = floor(sat.overall + rnorm(n=nCustomer, mean=-0.2, sd=0.3))
summary(cbind(sat.service,sat.selection))
#set the range for satisfaction level
sat.selection[sat.selection < 1] = 1
sat.selection[sat.selection > 5] = 5
#do the same for sat.service, but the summary show that we don't need to

#simulate no response
no.response = as.logical(rbinom(n=nCustomer, size=1,prob=cust.df$age/100))
sat.overall[no.response] = NA
sat.selection[no.response] = NA
sat.service[no.response] = NA
summary(cbind(sat.selection, sat.service))

#finally add sat to df
cust.df$sat.service = sat.service
cust.df$sat.selection = sat.selection

#start analysis
str(cust.df)
plot(x=cust.df$age, y=cust.df$credit.score, main="Plot of age vs credit score",
     ylim=c(500,900), xlim=c(15,55),
     col="blue", xlab="Customer's ages", ylab="Customer's credit score")
abline(v=mean(cust.df$age), col="darkblue", lty="dotted")
abline(h=mean(cust.df$credit.score), col="darkblue", lty="dotted")

plot(x=cust.df$online.spend, y=cust.df$store.spend,
     main = "Offline vs Online spend",
     xlab = "Online spend",
     ylab = "Store Spend",
     col="green",
     cex=0.7)
hist(x=cust.df$store.spend, main = "In-store spend counts",
     breaks = (0:ceiling(max(cust.df$store.spend/10)))*10,
     xlab="Spend value($)", 
     ylab="Count")

#symbol and color for plot
my.col = c("black", "green3")
my.pch = c(1,19) #symbol type, see ?points
#plot different color and type for customer with or without email
plot(x=cust.df$store.spend + 1, y=cust.df$online.spend + 1, 
     main = "Online vs offline spend",
     xlab = "Offline spend", 
     ylab = "Online spend",
     col = my.col[cust.df$email], pch = my.pch[cust.df$email],
     cex=0.7,
     log="xy") #perform log normalize

#add legend
legend(x="bottomleft", legend = paste("email on file: ", levels(cust.df$email)),
       col=my.col, pch=my.pch)
#plot many plot at once using par(mfrow=c(2,2))

#plot matrices
pairs(formula = ~ age + credit.score + email + distance.to.store
      + online.visit + online.trans + online.spend + store.spend + store.trans,
      data = cust.df)
library(car)
scatterplotMatrix(formula = ~ age + credit.score + email + distance.to.store
                  + online.visit + online.trans + online.spend + store.spend + store.trans,
                  data = cust.df)

#Covariance
cov(cust.df[,c(2,3,5:12)],use="complete.obs")
#correlation
cor(cust.df[,c(2,3,5:12)],use="complete.obs")

#cor plot
library(corrplot)
library(gplots)
corrplot.mixed(corr=cor(cust.df[,c(2,3,5:12)], use="complete.obs"),
               upper="ellipse", tl.pos = "lt",
               upper.col = colorpanel(50,"red","grey60","blue4"))

#transform data before plot
par(mfrow=c(1,2))
plot(x=cust.df$distance.to.store, y=cust.df$store.spend)
plot(x=1/sqrt(cust.df$distance.to.store), y=cust.df$store.spend)

#power transform (x^)
lambda = coef(powerTransform(cust.df$distance.to.store))
bcPower(cust.df$distance.to.store, lambda)
hist(cust.df$distance.to.store,
     main="original data distribution",
     xlab = "distance",
     ylab = "counts")
hist(bcPower(cust.df$distance.to.store,lambda), main = "transformed to normal distribution", 
     xlab = "distance", ylab = "count")

#transform both var give a strong correlation
l.dist = coef(powerTransform(cust.df$distance.to.store))
l.store = coef(powerTransform(cust.df$store.spend+1))
cor(bcPower(cust.df$distance.to.store,l.dist), bcPower(cust.df$store.spend+1,l.store))

#discrete variables
plot(x=cust.df$sat.selection, y=cust.df$sat.service,
     main = "Original",
     xlab = "selection",
     ylab = "service")
plot(x=jitter(cust.df$sat.selection), y= jitter(cust.df$sat.service),
     main = "Jittered",
     xlab = "selection",
     ylab = "service") #use jitter to add some noise

#polychoric is a complex function
