epa.df = readRDS("rintro-chapter14-epa.rds")
summary(epa.df)

plot(epa.df$bytes)
plot(log(epa.df$bytes))
plot(table(log(epa.df$bytes)))
