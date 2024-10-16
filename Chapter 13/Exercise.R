sportscar <- read.csv("sportcar.csv", stringsAsFactors = T)[,-1]
sportscar$seat = as.factor(sportscar$seat)
#sportscar$price = as.factor(sportscar$price)

summary(sportscar)
tail(sportscar)

xtabs(choice ~ trans, data=sportscar)

sportscar.mlogit.data = mlogit.data(data = sportscar, choice='choice', shape='long',
                                    varying = 5:8, alt.var = 'alt')

m1 = mlogit(choice ~ 0 + seat + trans + convert + price , data = sportscar.mlogit.data)
summary(m1)
# -> ideal car = 5 seats + auto transition + convertible + $30k

coef(m1)["convertyes"]/(-coef(m1)["price"]/1000)

newcars <- data.frame (seat= factor (c("2","4", "5")),
                         trans= factor (c("manual", "auto", "auto")),
                         convert= factor (c("no", "yes", "no")),
                         price=c(40, 37, 35))

predict.mnl(m1, newcars)

attrib <- list(seat = c("2", "4", "5"),
               trans = c("manual", "auto"),
               convert = c("no", "yes"), 
               price = c(30, 35, 40))
sensitivity.mnl(m1, attrib, base.data = newcars[1,],newcars[c(2,3),] )

racer.cars = sportscar[sportscar$segment == "racer", ]
racer.mlogit.data = mlogit.data(data = racer.cars, choice='choice', shape='long',
                                    varying = 5:8, alt.var = 'alt')

m2 = mlogit(choice ~ 0 + seat + trans + convert + price , data = racer.mlogit.data)
predict.mnl(m2, newcars)

m1.rpar = rep("n", length = length(m1$coefficients))
names(m1.rpar) = names(m1$coefficients)
m1.rpar

m1.hier = mlogit(choice ~ 0 + seat + trans + convert + price,
                 data = sportscar.mlogit.data,
                 , rpar = m1.rpar, correlation = T)

summary(m1.hier)
stdev(m1.hier)
cov2cor(cov.mlogit(m1.hier))
