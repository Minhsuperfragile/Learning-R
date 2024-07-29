hotel.df <- read.csv("https://goo.gl/oaWKgt")
write.csv(hotel.df, file="hotel-df-saved.csv")

#visualize the distributions of the variables 
summary(hotel.df)
hotel.df$visitPurpose = factor(hotel.df$visitPurpose)
hotel.df$eliteStatus = factor(hotel.df$eliteStatus)
library(car)
scatterplotMatrix(hotel.df[,c(1:18)]) #satisfaction
scatterplotMatrix(hotel.df[,c(19,20,22,23,24)]) #other
scatterplotMatrix(hotel.df[,c(26:28)])

# nights stayed, distance traveled and food spend per night have a left skew dist
# -> log distribution
hotel.df$logdist = log(hotel.df$distanceTraveled)
hotel.df$logNS = log(hotel.df$nightsStayed)
hotel.df$logFood = log(hotel.df$avgFoodSpendPerNight+1)
scatterplotMatrix(hotel.df[,c(26,27,28)])

#correlation pattern
library(corrplot)
corrplot.mixed(cor(hotel.df[,c(-21,-25)]), upper="ellipse" )
#There is no high correlation (>0.8), this data is acceptable 
#The highest correlation with overall is front staff
plot(satOverall ~ satFrontStaff, data=hotel.df)
m1 = lm(satOverall ~ satFrontStaff, data=hotel.df )
abline(m1, col="blue")

#we use polychoric correlation for ordinal responses (have a relationship in a way)
cor(hotel.df[ , 1:3])
library(psych)
polychoric(with(hotel.df, cbind(satCleanRoom, satCleanBath, satCleanCommon)))

#check the relationship between sat overall and sat perk
plot(satOverall ~ satPerks, data=hotel.df)
m1 = lm(satOverall ~ satPerks, data=hotel.df )
abline(m1, col="blue")
cor(hotel.df$satPerks, hotel.df$satOverall)

#control the model to fit with sat city and sat front staff
m1 = lm(satOverall ~ satPerks + satFrontStaff + satCity, data=hotel.df)
m1$coefficients
summary(m1)

hotel.GP = hotel.df[hotel.df$eliteStatus %in% c("Gold", "Platinum"),]
corrplot.mixed(cor(hotel.GP[,c(1:18)]), upper="ellipse")
#perks and sat recognition has the highest correlation -> we should invest in perks
m2 = lm(satRecognition ~ satCleanRoom + satFrontStaff + satPoints+ satPerks,
        data= hotel.GP)
summary(m2)
#sat points has the highest coef => we should invest sat points

#investigate with food per night ~ elite status + sat dining price
m3 = lm(logFood ~ eliteStatus + satDiningPrice, data=hotel.df)
summary(m3)

m4 = lm(logFood ~ satDiningPrice  , data=hotel.df)
summary(m4)
# -> positive linear related

#plot the predicted food spend per night as a function of night stayed
m5 = lm(logFood ~ logNS ,data=hotel.df)
plot(jitter(exp(hotel.df$logNS)) , jitter(exp(fitted(m5,hotel.df$logNS))),
     xlab = "Night stayed", ylab = "Prediction on food spend ")

#limit the data to only group platinum, and plot the difference
m6 = lm(logFood ~ logNS, data=hotel.df[hotel.df$eliteStatus == "Platinum",])
summary(m6)
summary(m5)

plot(jitter(exp(hotel.df$logNS)) , jitter(exp(fitted(m5,hotel.df$logNS))),
     xlab = "Night stayed", ylab = "Prediction on food spend ")

points(jitter(exp(hotel.df$logNS[hotel.df$eliteStatus == "Platinum"])),
       jitter(exp(fitted(m6,hotel.df$logNS))), col="blue")
