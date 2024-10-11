#retail.raw <- readLines("https://goo.gl/wi8KHg")
#save(retail.raw, file="retail-raw.RData")
load('retail-raw.RData')
retail.margin <- read.csv("retail-margin.csv")[,-1]

margin.short <- data.frame(retail.margin$margin)
rownames(margin.short) <- retail.margin$item

library(arules)
library(arulesViz)
retail.list = strsplit(retail.raw, split = ",")
names(retail.list) = paste("Trans" , 1:length(retail.list), sep="")
str(retail.list)

retail.trans = as(retail.list, "transactions")
summary(retail.trans)

retail.rules = apriori(retail.trans, parameter = list(supp=0.003, conf=0.2, maxlen=5))
summary(retail.rules)

retail.hi = head(sort(retail.rules, by="lift"), 30)
plot(retail.hi, method = "graph", control=list(type="items"))

retail.margsum <- function(items, itemMargins) {
  # Input: "items" == item names, rules or transactions in arules format
  #        "itemMargins", a data frame of profit margin indexed by name
  # Output: look up the item margins, and return the sum
  library(arules)
  
  # check the class of "items" and coerce appropriately to an item list
  if (class(items) == "rules") {
    tmp.items <- as(items(items), "list")       # rules ==> item list
  } else if (class(items) == "transactions") {
    tmp.items <- as(items, "list")              # transactions ==> item list
  } else if (class(items) == "list") {
    tmp.items <- items                          # it's already an item list!
  } else if (class(items) == "character") {
    tmp.items <- list(items)                    # characters ==> item list
  } else {
    stop("Don't know how to handle margin for class ", class(items))
  }
  # make sure the items we found are all present in itemMargins
  good.items <- unlist(lapply(tmp.items, function (x) 
    all(unlist(x) %in% rownames(itemMargins))))
  
  if (!all(good.items)) {
    warning("Some items not found in rownames of itemMargins. ", 
            "Lookup failed for element(s):\n",
            which(!good.items), "\nReturning only good values.")
    tmp.items <- tmp.items[good.items]
  }
  
  # and add them up
  return(unlist(lapply(tmp.items, function(x) sum(itemMargins[x, ]))))
}

basket.margin = retail.margsum(retail.trans, margin.short)
inspect(retail.trans[head(sort(baskets.margin, decr=TRUE), 10)])

basket.margin.hi = sort(basket.margin, decreasing = TRUE)
retail.trans.200 = retail.trans[basket.margin > 200]
items.freq = itemFrequency(retail.trans.200)
items.freq.10 = items.freq[items.freq > 0.1]
plot(sort(items.freq.10), xlab='items', ylab='proportion', xaxt='n')

axis(side=1, at=1:length(items.freq.10),
     labels = names(sort(items.freq.10)),
     cex.axis=0.7, las = 3)

retail.margin.prop <- retail.margin$margin / retail.margin$price
hist(retail.margin.prop, breaks = 30)
retail.margin.inv  <- 1/retail.margin.prop
hist(retail.margin.inv, breaks=30)
