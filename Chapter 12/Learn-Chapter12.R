library(arules)
data("Groceries")
summary(Groceries)
inspect(head(Groceries))

groc.rules = apriori(Groceries, parameter = list(supp=0.01, conf=0.3, target='rule'))
inspect(subset(groc.rules, lift>3))

load("retail-raw.RData")
#save(retail.raw, file="retail-raw.RData")

head(retail.raw)
tail(retail.raw)
summary(retail.raw)

retail.list = strsplit(retail.raw, split = " ")
names(retail.list) = paste("Trans" , 1:length(retail.list), sep="")
str(retail.list)
rm(retail.raw)

retail.trans = as(retail.list, "transactions")
summary(retail.trans)

retail.rules = apriori(retail.trans, parameter = list(supp=0.001, conf=0.4))

library(arulesViz)
plot(retail.rules, interaction=TRUE)

retail.hi = sort(retail.rules)
inspect(retail.hi)

plot(retail.hi , method="graph", control= list(type="items"))

# simulate margin and cost
retail.itemnames = sort(unique(unlist(as(retail.trans, 'list'))))
head(retail.itemnames)

set.seed(2710)
retail.margin = data.frame(margin=rnorm(length(retail.itemnames), mean = 0.3, sd = 0.3))
quantile(retail.margin$margin)

rownames(retail.margin) = retail.itemnames
head(retail.margin)
tail(retail.margin)

#access margin data frame
(basket.items <- as(retail.trans[3], "list")[[1]])
retail.margin[basket.items, ]
sum(retail.margin[basket.items, ])

retail.margsum = function(items, itemMargins) {
  # Input  "items" == item names , rules or transactions in arules format
  #         "itemMargins", a data frame of profit margin indexed by name
  # Output: look up the item margins , and return the sum 
  
  if (class(items) == "rules"){
    tmp.items = as(items(items), "list") # rules -> list
  } else if (class(items) == "transactions") {
    tmp.items = as(items, 'list') # transaction -> list
  } else if (class(items) == "list") {
    tmp.items = items # already a list
  } else if (class(items) == "character") {
    tmp.items = list(items)
  } else {
    stop("Unknown data type")
  }
  
  good.items <- unlist(lapply(tmp.items , function (x)
    all(unlist(x) %in% rownames(itemMargins )))) # check if any of item in list is in margin 
  
  if (!all(good.items)){
    tmp.items = tmp.items[good.items]
  }
  
  # and add them up
  return(unlist(lapply(tmp.items , function(x) sum(itemMargins[x, ]))))
}

retail.margsum(c("39", "48"), retail.margin)
load("../Chapter 5/seg-df-saved.RData")

summary(seg.df)
seg.fac = seg.df
seg.fac$age = cut(seg.fac$age, breaks = c(0,25,35,55,65,100), 
                  labels=c("19-24", "25-34", "35-54", "55-64", "65+"),
                  right = FALSE, ordered_result = TRUE)
seg.fac$income = cut(seg.fac$income, 
                     breaks = c(-100000, 40000, 70000, 1000000),
                     labels = c("Low", "Medium", "High"),
                     right = FALSE, ordered_result = TRUE)
seg.fac$kids  = cut(seg.fac$kids, 
                   breaks = c(0,1,2,3,100),
                   labels = c("No kid", "1 kid", "2 kids", "3+ kids"), 
                   right = FALSE, ordered_result = TRUE)
summary(seg.fac)
seg.fac$`kids ` = NULL

library(arules)
library(arulesViz)
seg.trans = as(seg.fac, "transactions")
summary(seg.trans)

seg.rules = apriori(seg.trans, parameter = list(supp=0.1, conf=0.4, target='rules'))
summary(seg.rules)
plot(seg.rules)

seg.hi = head(sort(seg.rules, by='lift'), 35)
inspect(seg.hi)
plot(seg.hi , method="graph", control= list(type="items"))