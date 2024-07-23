seg.df <- read.csv("seg-df-saved.csv")
load("seg-df-saved.RData")

#test if there is any difference in count of a variable
chisq.test(table(seg.df$Segment)) #there are different in segment size
#test if a variable is independent from one another or not
chisq.test(table(seg.df$subscribe,seg.df$ownHome)) #with a high p-value, 
# we must accept the null hypothesis -> these 2 variables are unrelated

