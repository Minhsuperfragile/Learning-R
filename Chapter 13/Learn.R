cbc.df <- read.csv2("cvc-df.csv", stringsAsFactors = T)[,-1]
head(cbc.df)

summary(cbc.df)

xtabs(choice ~ price , data =cbc.df)
xtabs(choice ~ cargo , data =cbc.df)

library(mlogit)
cbc.mlogit = mlogit.data(data = cbc.df, choice='choice', shape='long',
                         varying = c("carpool", 'seat', 'cargo', 'eng','price'), 
                         alt.levels = paste('pos', 1:3),
                         id.var = "resp.id")
m1 = mlogit(choice ~ 0 + seat + cargo + eng + price , data = cbc.mlogit)
summary(m1)

m2 = mlogit(choice ~ 1 + seat + cargo + eng +price, data=cbc.mlogit) 
summary(m2)

lrtest(m1,m2)

m3 = mlogit(choice ~ 0 + seat + cargo + eng + as.character(price),
            data = cbc.mlogit)
summary(m3)

lrtest(m1,m3)
