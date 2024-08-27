prst2 <- read.csv("prst2.csv")[,-1]
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
library(semPlot)
semPaths(prst.fit, what="est", fade = FALSE, residuals = FALSE, edge.label.cex = 0.75 )

factanal(prst2, factors = 2)

prst.model2 = " One =~ Adaptable + Friendly + Helpful + Intuitive
                Two =~ CuttingEdge + Delightful + Exciting + Generous"
prst.fit2 = cfa(prst.model2, data = prst2 ) 

library(semTools)
summary(compareFit(prst.fit , prst.fit2))

intent.df <- read.csv("intent-df.csv")[,-1]
write.csv(intent.df, file = "intent-df.csv")
View(intent.df)
names(intent.df)

#SEM
semModel = "Rating =~ iCuttingEdge + iEaseOfUse + iBestValue
            Interest =~ Rating + iPreviousModelRating
            Intent =~ Interest + iCost + iPurchaseIntent"

sem.fit = sem(semModel, data = intent.df, std.lv = TRUE)
semPaths(sem.fit, what="est", fade = FALSE, residuals = FALSE, edge.label.cex = 0.75)

semModel2 = "Rating =~ iBestValue + iEaseOfUse
            Intent =~ Rating + iCost +iPurchaseIntent"
sem.fit2 = sem(semModel2, data = intent.df, std.lv = TRUE)

semPaths(sem.fit2, what="est", fade = FALSE, residuals = FALSE, edge.label.cex = 0.75)
summary(sem.fit2, fit.measures = TRUE)
