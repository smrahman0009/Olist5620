# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("ROSE")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)


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
# approval_delay is time difference between the purchase time of the product  and the time taken to approve the order from the seller.
order_df["approval_delay"] = difftime(order_df$order_approved_at,order_df$order_purchase_timestamp,units="hours")
# ?boxplot

boxplot(order_df["approval_delay"])

# There are outliers. I will delete the approval_delay whose values are more than 150 hours. 
order_df = order_df %>% filter(order_df$approval_delay < 150)
boxplot(order_df["approval_delay"])



# Order status column have only one value which is order_status, So I am going to drop it now.
order_df$order_status <- NULL
# Shows the order posting timestamp. When it was handled to the logistic partner.
# carrier_delay is the time it was taken to be handled to the logistic partner after its approval by the seller.

order_df["carrier_delay"] = difftime(order_df$order_delivered_carrier_date ,order_df$order_approved_at,units="hours")

boxplot(order_df["carrier_delay"])
# carrier_delay has some negative values those are not possible at all. I am going to filter out the negative values
order_df = order_df %>% filter(order_df$carrier_delay > 0 & order_df$carrier_delay <800)

boxplot(order_df["carrier_delay"],main = "carrier delay in hours")


# order_delay is the time difference of the actual delivery time - the expected delivery date
order_df["order_delay"] = difftime(order_df$order_estimated_delivery_date ,order_df$order_delivered_customer_date,units="days")
boxplot(order_df['order_delay'], main="order delayed in days")
# I will consider +50 and -150 days as outliers and remove them to get more precise models

order_df = order_df %>% filter(order_df$order_delay > -150 & order_df$order_delay <50)
boxplot(order_df['order_delay'], main="order delayed in days")


 
order_df['late_delivery'] <- ifelse(order_df['order_delay']  <= 0,"no","yes")

table(order_df['late_delivery']) / nrow(order_df['late_delivery']) *100
summary(order_df)

ggplot(data=order_df,aes(x=late_delivery))+
  geom_bar(stat = "count")

# par(mfrow=c(1,2))

colnames(order_df)
prop.table(table(order_df$late_delivery))



# ################### Undersampling   #####################################

# Index of the values with Yes and NO
# Sample the indices
# Create a new dataset with the sampled indices

Yes <- which(order_df$late_delivery == "yes")
No <- which(order_df$late_delivery == "no")
length(No)


no_sampled_index <- sample(Yes,length(No))
df_balanced <- order_df[c(no_sampled_index,No),]

table(df_balanced$late_delivery)


df_balanced$late_delivery = as.factor(df_balanced$late_delivery)
##### testing and splitting datasets code #################


set.seed(3456)
partion_index <- createDataPartition(df_balanced$late_delivery, p = .8,
                                  list = FALSE,
                                  times = 1)
train_df <-df_balanced[partion_index,]
test_df <-df_balanced[-partion_index,]

train_df$order_delay <- NULL
test_df$order_delay <- NULL 

# ggplot(data=model_df,aes(x=late_delivery))+
#   geom_bar(stat = "count")


################## modeling_part ############################
###################### rpart #################################

colnames(train_df)

glm.model = glm(data = df_balanced,late_delivery~.,family = binomial())


summary(glm.model)

?glm




head(order_df$order_delay)
nrow(order_df$order_delay)
nrow(order_df$carrier_delivered_interval)
plot(order_df$carrier_delay,order_df$order_delay) 


summary(orders_dataset)
