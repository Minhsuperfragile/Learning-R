#create a Months vector with all months
Months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
Months

#create a summer vector with numeric index of summer months
summer <- 6:9
summer

#use summer vector to index summer months in Months
Months[summer]

#multiply summer by 3
tmp.summer <- summer*3
Months[tmp.summer] # result: NA NA NA NA
# => because there is no value, or missing value at position indexed by tmp.summer

#take mean of summer
(mean.summer <- mean(summer))
Months[mean.summer] # result: july => because R rounded down 7.5 to 7 and we got july

#use floor() and ceiling() to get limits of Months for average summer month
Months[floor(mean.summer)]
Months[ceiling(mean.summer)]

#using store.df to find out how many visits did Bert's store have
load("store-df-saved.RData", stringsAsFactors=F)
store.df$store.visits[store.df$store.manager == "Bert"]

#how can you confirm previous answer is actually from Bert's store
store.df[2,]

#create a function to return area of a pie
PieArea <- function(x) { 
  return(pi*x^2)  
}

#what are area of pies with length 4.0, 4.5, 5.0, 6.0
pie.radius <- c(4,4.5,5,6)
PieArea(pie.radius)

#rewrite the previous function with lambda function
pie.radius.list <- list(pie.radius)
lapply(pie.radius.list,function(x){x*pi*x})
