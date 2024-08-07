customers <- read.csv("bikeshops.csv")
products <- read.csv("bikes.csv") 
orders <- read.csv("orders.csv") 

library(dplyr)
orders.extended <- merge(orders, customers, by.x = "customer.id", by.y="bikeshop.id")
orders.extended <- merge(orders.extended, products, by.x = "product.id", by.y = "bike.id")

orders.extended <- orders.extended %>%
  mutate(price.extended = price * quantity) %>%
  select(order.date, order.id, order.line, bikeshop.name, model,
         quantity, price, price.extended, category1, category2, frame) %>%
  arrange(order.id, order.line)


