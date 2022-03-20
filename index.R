# install.packages("ggplot2")
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)

# order_df <- read.csv(file ='./db/orders_dataset.csv',sep=";",header=TRUE)
# points:- in which years, month the late delivery occours most. Is there any pattern? 

orders_dataset <- read.csv(file ='./db/orders_dataset.csv',header=TRUE)
colnames(orders_dataset)
# order_id and customer_id is not necessary 
order_df <- orders_dataset[,c("order_status","order_purchase_timestamp","order_approved_at","order_delivered_carrier_date","order_delivered_customer_date","order_estimated_delivery_date")]



summary(order_df)
sum(is.na(order_df))
mean(is.na(order_df))
order_df = na.omit(order_df)
summary(order_df)


# order_estimated_delivery_date = Shows the estimated delivery date that was informed to customer at the purchase moment.
# order_delivered_customer_date = Shows the actual order delivery date to the customer.
# order_delivered_carrier_date = Shows the order posting timestamp. When it was handled to the logistic partner.
# order_approved_at = Shows the payment approval timestamp.
# order_purchase_timestamp = Shows the purchase timestamp.



head(order_df)
unique(order_df["order_status"])
ggplot(data=order_df,aes(x=order_status))+
  geom_bar(stat = "count")




# As I am planning only to find out the late delivery thats why I am going to drop outhers value from order_status
order_df = filter(order_df, order_status %in% c("delivered"))
unique(order_df["order_status"])
head(order_df)




# %d -> Day
# %m -> Numeric Month
# %b -> Abbreviated Month
# %B -> Full Month
# %y -> 2-digit year
# %Y -> 4-digit year

# Now I am going to convert the Date related columns to DateTime format in order_df dataframe 
order_df$order_purchase_timestamp <- as.POSIXct(order_df$order_purchase_timestamp, format = "%Y-%m-%d %H:%M:%S")
order_df$order_approved_at <- as.POSIXct(order_df$order_approved_at, format = "%Y-%m-%d %H:%M:%S")
order_df$order_approved_at <- as.POSIXct(order_df$order_approved_at, format = "%Y-%m-%d %H:%M:%S")
order_df$order_delivered_carrier_date <- as.POSIXct(order_df$order_delivered_carrier_date, format = "%Y-%m-%d %H:%M:%S")
order_df$order_delivered_customer_date <- as.POSIXct(order_df$order_delivered_customer_date, format = "%Y-%m-%d %H:%M:%S")
order_df$order_estimated_delivery_date <- as.POSIXct(order_df$order_estimated_delivery_date, format = "%Y-%m-%d %H:%M:%S")

head(order_df)

typeof(order_df$order_purchase_timestamp)
class(order_df$order_purchase_timestamp)
# approval_time is time difference between the purchase time of the product  and the time taken to approve the order from the seller.
order_df["approval_delay"] = difftime(order_df$order_approved_at,order_df$order_purchase_timestamp,units="hours")
# ?boxplot
boxplot(order_df["approval_delay"])




# Order status column have only one value, So I am going to drop it now.
order_df$order_status <- NULL
# Shows the order posting timestamp. When it was handled to the logistic partner.
# carrier_delivered_interval is the time it was taken to be handled to the logistic partner after its approval by the seller.

order_df["carrier_delay"] = difftime(order_df$order_delivered_carrier_date ,order_df$order_approved_at,units="hours")

boxplot(order_df["carrier_delay"])
# carrier_delay has some negative values those are not possible at all. I am going to filter out the negative values
order_df = order_df %>% filter(order_df$carrier_delay > 0)

boxplot(order_df["carrier_delay"])


# order_delay is the time difference of the actual delivery time - the expected delivery date
order_df["order_delay"] = difftime(order_df$order_estimated_delivery_date ,order_df$order_delivered_customer_date,units="days")
colnames(order_df)
head(order_df$order_delay)
nrow(order_df$order_delay)
nrow(order_df$carrier_delivered_interval)
plot(order_df$carrier_delay,order_df$order_delay) 


# 
