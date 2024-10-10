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
