#Among teacher and parent, which group has more count in product knew what wanted

with(subset(ecom.df, profile %in% c("Teacher", "Parent") & productKnewWhatWanted %in% c("Yes","No")),
     prop.table(table(profile, productKnewWhatWanted), margin=1))

with(subset(ecom.df, profile %in% c("Teacher", "Parent")), 
     prop.table(table(profile, productKnewWhatWanted), margin=1))

#visualization
plot(table(ecom.df$profile, ecom.df$productKnewWhatWanted))
ecom.df.PT = subset(ecom.df, profile %in% c("Teacher","Parent"))

with(ecom.df.PT, plot(table(profile,productKnewWhatWanted)))

#is there any different in product knew what wanted between parent and teacher
chisq.test(table(ecom.df.PT$profile[ecom.df.PT$productKnewWhatWanted == "Yes"]))
# -> yes there is significant different

ecom.df.PTH = subset(ecom.df, profile %in% c("Teacher", "Parent", "Health Professional"))
aggregate(ecom.df.PTH$pageViewInt, list(ecom.df.PTH$profile), sum)

#check if there are real 60% of yes for parent
table(ecom.df.PT$profile[ecom.df.PT$productKnewWhatWanted == "No"], ecom.df.PT$productKnewWhatWanted[ecom.df.PT$productKnewWhatWanted == "No"])
binom.test(172, 172+111, 0.6) #-> yes 60% is real proportion with 95% CI = 0.54;0.66
#check if there are real 60% of yes for teacher
table(ecom.df.PT$productKnewWhatWanted, ecom.df.PT$profile)
binom.test(48,48+38, 0.55) #-> yes 55% is real with 95% CI = 0.44;0.66

aggregate(ecom.df.PT$pageViewInt, list(ecom.df.PT$profile), mean)
#parent have higher mean in page view int
t.test(pageViewInt ~ profile, data=ecom.df.PT)
#-> the null hypothesis is rejected -> the difference is statistic significant 
#-> 95% CI: 0.29;1.48

#compare for all profile
ecom.df.aov = aov(pageViewInt ~ profile, data=ecom.df)
anova(ecom.df.aov)
# the p-value is small -> reject the null hypothesis -> there is difference
#plot 
library(multcomp)
par(mar=c(6,10,2,2))
ecom.df.aov = aov(pageViewInt ~ -1 + profile, data=ecom.df)
plot(glht(ecom.df.aov), xlab = "Page Views", main="Page View On Profiles")

ecom.df.PT.aov = aov(pageViewInt ~ -1 + profile, data = ecom.df.PT)
plot(glht(ecom.df.PT.aov), xlab="Page Views", main="Page view on teacher and parent")
