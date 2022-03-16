# install.packages("ggplot2")
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)

# order_dt <- read.csv(file ='./db/orders_dataset.csv',sep=";",header=TRUE)
# points:- in which years, month the late delivery occours most. Is there any pattern? 

order_dt <- read.csv(file ='./db/orders_dataset.csv',header=TRUE)
colnames(order_dt)
order_dt <- order_dt[,c("order_status","order_purchase_timestamp","order_approved_at","order_delivered_carrier_date","order_delivered_customer_date","order_estimated_delivery_date")]

# order_estimated_delivery_date = Shows the estimated delivery date that was informed to customer at the purchase moment.
# order_delivered_customer_date = Shows the actual order delivery date to the customer.
# order_delivered_carrier_date = Shows the order posting timestamp. When it was handled to the logistic partner.
# order_approved_at = Shows the payment approval timestamp.
# order_purchase_timestamp = Shows the purchase timestamp.

head(order_dt)
unique(order_dt["order_status"])

ggplot(data=order_dt,aes(x=order_status))+
  geom_bar(stat = "count")

# As I am planning only to find out the late delivery thats why I am going to drop outhers value from order_status
order_dt = filter(order_dt, order_status %in% c("delivered"))

unique(order_dt["order_status"])

head(order_dt)

# %d -> Day
# %m -> Numeric Month
# %b -> Abbreviated Month
# %B -> Full Month
# %y -> 2-digit year
# %Y -> 4-digit year

# Now I am going to convert the Date related columns to DateTime format in order_dt dataframe 

order_dt$order_purchase_timestamp <- as.POSIXct(order_dt$order_purchase_timestamp, format = "%Y-%m-%d %H:%M:%S")
order_dt$order_approved_at <- as.POSIXct(order_dt$order_approved_at, format = "%Y-%m-%d %H:%M:%S")
order_dt$order_approved_at <- as.POSIXct(order_dt$order_approved_at, format = "%Y-%m-%d %H:%M:%S")
order_dt$order_delivered_carrier_date <- as.POSIXct(order_dt$order_delivered_carrier_date, format = "%Y-%m-%d %H:%M:%S")
order_dt$order_delivered_customer_date <- as.POSIXct(order_dt$order_delivered_customer_date, format = "%Y-%m-%d %H:%M:%S")
order_dt$order_estimated_delivery_date <- as.POSIXct(order_dt$order_estimated_delivery_date, format = "%Y-%m-%d %H:%M:%S")

head(order_dt)

typeof(order_dt$order_purchase_timestamp)
class(order_dt$order_purchase_timestamp)
