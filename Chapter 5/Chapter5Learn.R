segVar = c("age", "gender","income", "kids", "ownHome", "subscribe")
segVarType = c("norm", "binom", "norm", "pois", "binom", "binom")
# norm -> continuous data
# binom -> yes/no data
# poisson -> count
segNames = c("Suburb mix", "Urban hip", "Travelers", "Moving up")
segSize = c(100 , 50, 80, 70)

segMean = matrix(c(40, .5, 55000, 2, .5, .1, #we need mean for those distribution function
                   24, .7, 21000, 1, .2, .2,
                   58, .5, 64000, 0, .7, .05,
                   36, .3, 52000, 2, .3, .2), ncol = length(segVar), byrow = T)
segSDs = matrix(c( 5 , NA , 12000 , NA , NA , NA, # continuous variables also needs standard deviation
                   2 , NA , 5000 , NA , NA , NA, 
                   8 , NA , 21000 , NA , NA , NA,
                   4 , NA , 10000 , NA , NA , NA), ncol = length(segVar), byrow = T)

#start create random data
seg.df = NULL
set.seed(2710)

for (i in seq_along(segNames)){
  cat(i, segNames[i], "\n")
  this.seg = data.frame(matrix(data=NA, nrow=segSize[i], ncol=length(segVar)))
  
  for (j in seq_along(segVar)){
    if (segVarType[j] == "norm"){
      this.seg[,j] = rnorm(n=segSize[i], mean=segMean[i,j], sd=segSDs[i,j])
    }
    
    else if (segVarType[j] == "binom") {
      this.seg[,j] = rbinom(n=segSize[i], size=1, prob = segMean[i,j])
    }
    
    else if (segVarType[j] == "pois") {
      this.seg[,j] = rpois(n=segSize[i], lambda = segMean[i,j])
    } 
    
    else{
      print("Bad distribution type")
      stop()
    }
  }
   
  seg.df = rbind(seg.df, this.seg)
  #rm(this.seg)
}
rm(this.seg, i,j)

names(seg.df) = segVar
seg.df$Segment = factor(rep(segNames, times=segSize))
seg.df$gender = factor(seg.df$gender, labels = c("Female","Male"))
seg.df$ownHome = factor(seg.df$ownHome, labels = c("ownNO", "ownYes"))
seg.df$subscribe = factor(seg.df$subscribe, labels = c("subNo", "subYes"))

#inspect the data
summary(seg.df)

seg.income.mean = aggregate(seg.df$income, list(seg.df$Segment), mean) # this have an advantage compare to by because it return a data frame
seg.df$segIncome = seg.income.mean[seg.df$Segment, 2] #use seg.income.mean as a lookup table
View(seg.df)
seg.df$segIncome = NULL

aggregate(income ~ Segment, data=seg.df, mean)
aggregate(income ~ Segment + ownHome, data=seg.df , mean)

with(seg.df, table(Segment, ownHome)) #cross table
with(seg.df, table(kids, Segment))

#count how many kids
xtabs(kids ~ Segment, data=seg.df)

#plot proportion relatively
library(lattice)
histogram(~subscribe | Segment, data=seg.df) 
#plot real counts
histogram(~subscribe | Segment, data=seg.df,
          type="count", layout=c(4,1), col=c("burlywood", "darkolivegreen"))

#proportion with table
prop.table(table(seg.df$subscribe, seg.df$Segment),margin = 2)
#plot proportion with a bar chart
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment),margin = 2)[2,],
         xlab="Proportion of subscribed in each segment")

#plot continuous variables (income by segment) -> we plot mean value
seg.income.mean = aggregate(income ~ Segment, data=seg.df, mean)
barchart(income ~ Segment, data=seg.income.mean, col='grey')

seg.income.mean.2 = aggregate(income ~ Segment + ownHome, data=seg.df, mean)
barchart(income ~ Segment, data=seg.income.mean.2, groups = ownHome, auto.key=TRUE,
         par.settings=simpleTheme(col=terrain.colors(2)))

#box plot -> to see the distribution (quantile)
boxplot(income~Segment, data=seg.df, yaxt='n', ylab="Income")
ax.seq <- seq(from =0, to=120000 , by=20000)
axis(side=2, at=ax.seq, labels = paste(ax.seq/1000, "k", sep=" "), las=1)

#a better box plot
bwplot(Segment ~ income , data=seg.df , horizontal=TRUE , xlab = "Income")
#conditioning with  own home
bwplot(Segment ~ income |ownHome , data=seg.df , horizontal=TRUE , xlab = "Income")
