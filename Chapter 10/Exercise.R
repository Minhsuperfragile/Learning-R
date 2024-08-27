prst2 <- read.csv("https://goo.gl/BTxyFB")
write.csv(prst2, file="prst2.csv")

library(corrplot)
corrplot(cor(prst2), order = "hclust")

library(lavaan)
factanal(prst2, factors = 3)

prst.model = "Medium =~ Adaptable + 1.0 * BestValue
              High =~ CuttingEdge + Delightful + Exciting + Generous
              Low =~ Friendly + Helpful + Intuitive + Adaptable"
prst.fit = cfa(prst.model , data = prst2)

#use semplot to plot

factanal(prst2, factors = 2)

prst.model2 = " One =~ Adaptable + Friendly + Helpful + Intuitive
                Two =~ CuttingEdge + Delightful + Exciting + Generous"
prst.fit2 = cfa(prst.model2, data = prst2 ) 

library(semTools)
summary(compareFit(prst.fit , prst.fit2))

intent.df <- read.csv("https://goo.gl/6U5aYr")
write.csv(intent.df, file = "intent-df.csv")
View(intent.df)
names(intent.df)
