k.stores = 20 # use k for constant
k.weeks = 104 # 2 years

#create data frame with blank slots to hold data later
store.df = data.frame(matrix(NA, ncol = 10, nrow = k.stores*k.weeks))
names(store.df) = c("storeNum", "Year", "Week", "p1sales", "p2sales",
                    "p1price", "p2price", "p1prom", "p2prom", "country")
head(store.df)
dim(store.df) #simplest summary of store.df (see the dimension)

#create 2 vectors to fill df
store.num = 101:(100+k.stores)
store.country = c(rep("US" , 3) , rep("DE" , 5) , rep("GB" , 3) , rep("BR" , 2),
                  rep("JP" , 4) , rep("AU" , 1) , rep("CN" , 2))
length(store.country) # 20 countries for 20 stores

#replacing
store.df$storeNum = rep(store.num, each=k.weeks)
store.df$country = rep(store.country, each=k.weeks)

#we do the same for week and year
store.df$Week = rep(1:52, times=k.stores*2)
store.df$` Year`= rep(rep(1:2,each=k.weeks/2),times=k.stores)

#check the overall structure
str(store.df)

#set store.number and store.country as factors
#we do this to set it as categorical variables, not just number or character
store.df$storeNum = factor(store.df$storeNum)
store.df$country = factor(store.df$country)

#now we want to create random value for the rest
#set a seed for random generator
set.seed(98250)

#set a random promotion chance from binomial 
store.df$p1prom = rbinom(n=nrow(store.df),size=1,p=0.1)
store.df$p2prom = rbinom(n=nrow(store.df),size=1,p=0.15)

#get a random price from 5 distinct prices
store.df$p1price = sample(c(2.19, 2.29, 2.49, 2.79, 2.99),
                          size=nrow(store.df),replace=TRUE)
store.df$p2price = sample(c(2.29, 2.49, 2.59, 2.99, 3.19),
                          size=nrow(store.df),replace=TRUE)

#create sale data using poisson distribution
#set lambda=mean of sale
tmp.sale1 = rpois(n=nrow(store.df), lambda=120)
tmp.sale2 = rpois(n=nrow(store.df), lambda=100)

#scale up with a log algorithm, we assume that sales and prices have inverse ratio
tmp.sale1 = tmp.sale1*log(store.df$p2price)/log(store.df$p1price)
tmp.sale2 = tmp.sale2*log(store.df$p1price)/log(store.df$p2price)

#we assume sale get a 30% or 40% boost when the product is promoted
#and use floor() to keep sales an integer
store.df$p1sales = floor(tmp.sale1*(1+store.df$p1prom*0.3))
store.df$p2sales = floor(tmp.sale2*(1+store.df$p2prom*0.4))

#count frequency of each price in p1
p1.table = table(store.df$p1price)
plot(p1.table)

#cross tables of price and promotion (how often each product was promoted at each price)
p1.crossTable = table(store.df$p1price, store.df$p1prom)
p1.crossTable[,2] / (p1.crossTable[,1]+p1.crossTable[,2])

#quantile at different level
quantile(store.df$p1sales,probs=0:10/10)

#create a summary 
mysummary.df = data.frame(matrix(NA,nrow=2, ncol=2))
names(mysummary.df) = c("Median sales", "IQR")
rownames(mysummary.df) = c("Product 1" , "Product 2")
mysummary.df["Product 1", "Median sales"] = median(store.df$p1sales)
mysummary.df["Product 2", "Median sales"] = median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] = IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] = IQR(store.df$p2sales)
mysummary.df

#default summary
summary(store.df)

#describe from psych package
install.packages("psych")
library(psych)
describe(store.df) #recommended to use with discrete data 
describe(store.df[, c(2, 4:9)]) #skip some columns

#plot data
#histogram
hist(store.df$p1sales,
     main="Product 1 weekly sales frequency, all stores",
     xlab="Product 1 sales (Unit)",
     ylab="Frequency",
     breaks=30,
     col="lightgreen",
     freq=F,
     xaxt="n")
axis(side=1, at=seq(60,300,by=20))
lines(density(store.df$p1sales, bw=10), 
      type="l",
      col="darkred",
      lwd=2)

#box plot
boxplot(store.df$p2sales ~ store.df$p2prom,
        main="Product 2 weekly sales by promotion",
        xlab="Weekly sales",
        ylab="In-store promotion?",
        horizontal=TRUE,
        las=1,
        yaxt="n")
axis(side=2,at=c(1,2), labels=c("No","Yes"))
#bean plot (similar to box plot)
beanplot(p2sales ~ p2prom, data=store.df,
         what=c(1,1,1,0), log="", side="second", #side=second to compute density
         main="Product 2 weekly sales by promotion",
         xlab="Weekly sale",
         ylab="In-store promotion?",
         horizontal=TRUE,
         las=1,
         yaxt="n")
#QQ plot (quantile-quantile)
qqnorm(store.df$p1sales) #compare with normal distribution
qqline(store.df$p1sales)
#we observe a strange upward curve => not normally distributed
#use log transform
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))
#now everything look good

#cumulative distribution
plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of p1 weekly sales",
     ylab="Cumulative proportion",
     xlab=c("P1 weekly sales of all store","90% of weeks sole <= 174 units"),
     yaxt="n")
axis(side=2, at=seq(0,1,by=0.1), las=1,
     labels=paste(seq(0,100,by=10),"%", sep=""))
abline(h=0.9,lty=3) # create a line at 90th percentile
abline(v=quantile(store.df$p1sales,probs=0.9),lty=3)

#by() divide data into subgroup 
by(store.df$p1sales, list(store.df$storeNum,store.df$Year), FUN=mean)
#use aggregate() to return a formatted result
p1sale.sumByCountry = aggregate(store.df$p1sales, list(country=store.df$country), sum)

#create map
p1sales.map = joinCountryData2Map(p1sale.sumByCountry, joinCode="ISO2",
                                  nameJoinColumn = "country")
mapCountryData(mapToPlot = p1sales.map, 
               nameColumnToPlot = "x",
               mapTitle="Total P1 sales by country",
               colourPalette = brewer.pal(7,"Greens"),
               catMethod = "fixedWidth", addLegend = F
               )
