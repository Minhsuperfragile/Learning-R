seg.df <- read.csv("seg-df-saved.csv")
load("seg-df-saved.RData")

#test if there is any difference in count of a variable
chisq.test(table(seg.df$Segment)) #there are different in segment size
#test if a variable is independent from one another or not
chisq.test(table(seg.df$subscribe,seg.df$ownHome)) #with a high p-value, 
# we must accept the null hypothesis -> these 2 variables are unrelated

#binomial test to see if there is real p% of existence in a group
binom.test(260,500,p=0.5)

#t-test
#many statistical test require the data to be normal distributed
#so we check by plot them out with histogram
hist(seg.df$income)
with(seg.df, hist(income[ownHome == "ownNO"]))
with(seg.df, hist(income[ownHome == "ownYes"]))
#they are normally distributed
t.test(income ~ ownHome, data=seg.df)
#use subset to filter only data from segment traveler
t.test(income ~ ownHome, data=subset(seg.df, Segment == "Travelers"))

#anova table
seg.aov.own = aov(income ~ ownHome, data=seg.df)
anova(seg.aov.own)
#anova table compare income difference on segments
seg.aov.seg = aov(income ~ Segment, data=seg.df)
anova(seg.aov.seg)
#anova table compare income difference on segments and own home
anova(aov(income ~ Segment + ownHome, data=seg.df))
#we see that own home now have a high p-value -> segment and own home are not
#independent and segment show major part of difference

#anova table with interaction (:) and both main factor and interaction (*)
anova(aov(income ~ Segment * ownHome, data=seg.df))

#plot the confidence interval
library(multcomp)
seg.aov = aov(income ~-1 + Segment, data=seg.df)
par(mar=c(6,10,2,2))
plot(glht(seg.aov),
     xlab="income", main="Average income by segment, 95% CIs")

#use step to select better relationship
seg.aov.step = step(aov(income ~  . ,data=seg.df))
