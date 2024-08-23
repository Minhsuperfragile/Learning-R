setwd("/home/tminh/Documents/R/Learning-R/Chapter 10")
#CFA
library(lavaan)
library(semTools)

piesModel = " General =~ i1 + i2 + i3
              Feature =~ i4 + i5 + i6 + i7
              Image =~ i8 + i9 + i10 + i11
              PIES =~ General + Feaure + Image"

piesDataModel <- "General =~ 0.9*i1 + 0.7*i2 + 0.5*i3
                  Feature =~ 0.3*i3 + 0.7*i4 + 0.9*i5 + 0.5*i6 + 0.9*i7
                  Image =~ 0.2*i3 + 0.8*i8 + 0.9*i9 + 0.8*i10 + 0.7*i11
                  PIES =~ 0.7* General + 0.8*Feature + 0.8*Image"

set.seed(2710)
piesSimData = simulateData(piesDataModel, sample.nobs = 3600)
print(head( piesSimData), digits=2)

piesSimData = data.frame(lapply(piesSimData, 
                                function(x) {cut(x, breaks = 7, labels = FALSE )}))

library(car)
some(piesSimData)
summary(piesSimData)
library(psych)
describe(piesSimData)

library(RColorBrewer)
scatterplotMatrix(piesSimData[c(1,2,4,5,8,9)],
                  col=brewer.pal(3, "Paired"), ellipse=TRUE)

factanal(piesSimData, factors = 3)