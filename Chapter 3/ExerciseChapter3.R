setwd("/home/tminh/Documents/R/Learning-R/Chapter 3")
ecom.df = read.csv("ecommerce-data.csv")
#There are 1593 observations and 45 variables in ecom.df

#Compute a frequency table
View(ecom.df)
table(ecom.df$country)

#Compute a two-way table with intent to buy and user profile
View(table(ecom.df$intentWasPlanningToBuy , ecom.df$profile))
