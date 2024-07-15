setwd("/home/tminh/Documents/R/Learning-R/Chapter 3")
setwd("G:/Code/R/Chapter 3")
ecom.df = read.csv("ecommerce-data.csv")
#There are 1593 observations and 45 variables in ecom.df

#Compute a frequency table
View(ecom.df)
table(ecom.df$country)

#Compute a two-way table with intent to buy and user profile
(two_way_table = data.frame(table(ecom.df$intentWasPlanningToBuy , ecom.df$profile)))
View(two_way_table)

#sum of parents who intended to buy 
sum.parent = sum(two_way_table$Freq[(two_way_table$Var1 == "Yes" ) & (two_way_table$Var2 == "Parent")])
#sum of teachers who intended to buy
sum.teacher = sum(two_way_table$Freq[(two_way_table$Var1 == "Yes" ) & (two_way_table$Var2 == "Teacher")])
#sum of total people with answer
sum.total = sum(two_way_table$Freq[two_way_table$Var1 != ""])
#proportion of teacher who intended to buy
sum.teacher/sum.total
sum.parent/sum.total

#which state has the most visitor
ecom.df.US = ecom.df[ecom.df$country=="United States",c("region")]
max(table(ecom.df.US))
#TX is the state with highest visitors, 94

#using which.max to get index of the maximum value in freq of regions
table(ecom.df.US)[which.max(table(ecom.df.US))]

#draw a histogram of behavNumVisits and add a density line
hist(ecom.df$behavNumVisits,
     breaks = 200,col = "lightblue", border = "grey50",
     freq = F
     )
lines(density(ecom.df$behavNumVisits), lwd=1, col="black" )

#draw a horizontal box plot 
boxplot(ecom.df$behavNumVisits, horizontal = TRUE)
# the histogram plot is easier to see the smaller part but box plot is better to see out-liner

par(mar=c(3, 12, 2, 2)) # set plot margin
boxplot(ecom.df$behavNumVisits ~ ecom.df$profile, horizontal = TRUE, yaxt="n")
axis(side=2,at = c(8:1), labels = c("P","PwA", "T","HP","O","R","F","0"))
par(mar=c(5, 4, 4, 2) + 0.1)
unique(ecom.df$profile)

MeanMedDiff = function(x) {
  (tmp.mean = mean(x))
  (tmp.med = median(x))
  return(abs(tmp.med-tmp.mean))
  }
MeanMedDiff(ecom.df$behavNumVisits)
#exclude the person with the most visits
MeanMedDiff(ecom.df$behavNumVisits[-which.max(ecom.df$behavNumVisits)])

#use apply() to Mean Med Diff
apply(ecom.df[38:45], MARGIN = 2, FUN=MeanMedDiff)

#use with anonymous function
apply(ecom.df[38:45], MARGIN=2, FUN= function(x){return(abs(mean(x)-median(x)))})
# I would prefer named version, since they are easier to read or maintain if needed