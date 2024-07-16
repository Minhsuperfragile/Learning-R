ecom.df = read.csv("ecommerce-data.csv")
hist(x=ecom.df$behavNumVisits, main = "behavNumVisits",
     breaks = 200,
     xlab = "visits", ylab = "counts")
plot(log(table(ecom.df$behavNumVisits)),
     main = "Table with log transform",
     xlab = "Visits",
     ylab = "Counts",
     yaxt = "n")
#the plot is good but I think both of them are not very different
logbreaks <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
axis(side=2, at=log(logbreaks), labels=logbreaks, las=1)

pageViewInt = c(1:1593)
unique(ecom.df$behavPageviews)
pageViewInt[ecom.df$behavPageviews == "4 to 6"] = 5
pageViewInt[ecom.df$behavPageviews == "1"] = 1
pageViewInt[ecom.df$behavPageviews == "10+"] = 11
pageViewInt[ecom.df$behavPageviews == "7 to 9"] = 8
pageViewInt[ecom.df$behavPageviews == "2 to 3"] = 2
pageViewInt[ecom.df$behavPageviews == "0"] = 0

ecom.df$pageViewInt = pageViewInt

hist(pageViewInt)

#plot a scatter plot with page view and site visit
plot(x=jitter(log(ecom.df$behavNumVisits)) , y=jitter(ecom.df$pageViewInt),
     main = "Scatter plot of visits and page views",
     xlab = "visits", ylab="page views", xaxt="n"
     )
axis(side=1, at=log(logbreaks), labels = logbreaks)

cor(x=log(ecom.df$behavNumVisits), y=ecom.df$pageViewInt)
# Pearson's r is not good in this case because both variables are discrete and not normally distributed

#polychoric
library(psych)
polychoric(cbind(ecom.df$behavNumVisits[!is.na(ecom.df$behavNumVisits)], ecom.df$pageViewInt))

#load Salaries data
data("Salaries") #auto saved into environment

#scatter matrices
pairs(formula = ~rank + discipline + yrs.since.phd + yrs.service + sex + salary,
      data = Salaries)
scatterplotMatrix(formula = ~rank + discipline + yrs.since.phd + yrs.service + sex + salary,
                  data = Salaries)
#calculate correlation
cor(Salaries[,c(3,4,6)], use="complete.obs")
# => year since phd and year service are the most related 