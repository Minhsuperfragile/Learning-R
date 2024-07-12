hdp.df = read.csv("HDP.csv")
head(hdp.df)

#convert some to factor
hdp.df$Sex = factor(hdp.df$Sex)
hdp.df$Chest.pain.type = factor(hdp.df$Chest.pain.type)
hdp.df$FBS.over.120 = factor(hdp.df$FBS.over.120)
hdp.df$EKG.results = factor(hdp.df$EKG.results)
hdp.df$Exercise.angina = factor(hdp.df$Exercise.angina)
hdp.df$Slope.of.ST = factor(hdp.df$Slope.of.ST)
hdp.df$Thallium = factor(hdp.df$Thallium)
hdp.df$Number.of.vessels.fluro = factor(hdp.df$Number.of.vessels.fluro)
hdp.df$Heart.Disease = factor(hdp.df$Heart.Disease)

dim(hdp.df)
head(hdp.df)
tail(hdp.df)
library(car)
some(hdp.df)

#summary with default function
summary(hdp.df)
describe(hdp.df[,c(1,4,5,8,10)])
