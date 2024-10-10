retail.raw <- readLines("https://goo.gl/wi8KHg")
retail.margin <- read.csv("retail-margin.csv")[,-1]

margin.short <- data.frame(retail.margin$margin)
rownames(margin.short) <- retail.margin$item

library(arules)
retail.trans = as(strsplit(retail.raw, split = " "), 'transactions')
summary(retail.trans)

retail.rules = apriori(retail.trans, parameter = list(supp=0.0001, conf=0.4, maxlen=100))
summary(retail.rules)
