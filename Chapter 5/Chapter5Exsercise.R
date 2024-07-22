load(file="ecomd-df-saved.RData")

aggregate(pageViewInt ~ profile, data=ecom.df, sum )

pageView.sum = data.frame(matrix(0, nrow=length(unique(ecom.df$profile)), ncol = 2,))
names(pageView.sum) = c("profile", "count")
pageView.sum$profile = unique(ecom.df$profile)
for (i in seq_along(ecom.df$pageViewInt)){
  pageView.sum$count[pageView.sum$profile == ecom.df$profile[i]] = pageView.sum$count[pageView.sum$profile == ecom.df$profile[i]] + ecom.df$pageViewInt[i]
}
View(pageView.sum)

with(ecom.df[ecom.df$gender=="Female" | ecom.df$gender=="Male", ], 
     prop.table(table(profile, gender), margin=1))

View(with(ecom.df, table(profile, purchasedWhen)))
library(lattice)
histogram(~profile | purchasedWhen  ,data=ecom.df, scales=list(x=list(rot=45)))

aggregate(behavAnySale ~ profile + gender, data=ecom.df, sum )
histogram(~behavAnySale | profile + gender, data=ecom.df)


table(ecom.df$gender,ecom.df$profile,ecom.df$behavAnySale)
prop.table(table(ecom.df$gender, ecom.df$profile, ecom.df$behavAnySale),margin = 3)
