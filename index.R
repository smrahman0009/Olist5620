# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("ROSE")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

######################## Loading && JOINING ALL OLIST DATASETS ##############################
# orders_dataset <- read.csv(file ='./db/orders_dataset.csv',header=TRUE)
# customers_dataset <- read.csv(file ='./db/customers_dataset.csv',header=TRUE)
# order_items_dataset <- read.csv(file ='./db/order_items_dataset.csv',header=TRUE)
# products_dataset <- read.csv(file ='./db/products_dataset.csv',header=TRUE)
# sellers_dataset <- read.csv(file ='./db/sellers_dataset.csv',header=TRUE)
# order_payments_dataset <- read.csv(file ='./db/order_payments_dataset.csv',header=TRUE)


# geolocations datasets is not required as sellers_dataset and products_dataset already contains sellers and customers geolocations. 


## merge_syntax merge(x, y, by.x, by.y,all.x,all.y, sort = TRUE)

# olist_df_test = merge(x=olist_df_test,y=sellers_dataset,by="seller_id")
# olist_df_test = merge(x=olist_df_test,y=order_payments_dataset,by="order_id")

########################## __________SAVE_THE_MERGE__DATASET___________________
# save the new merged olist datasets as olist_df_test
# write.csv(olist_df_test,"./db/olist_df.csv", row.names = FALSE)



######################### __________LOAD_THE_MERGE_DATASET____##################

olist_df <- read.csv(file ='./db/olist_df.csv',header=TRUE)

colnames(olist_df)
head(olist_df,4)
# Checking the instances of null values in the newly created dataset. 
is.null(olist_df_test)
######################### __________CHECK_NUMBER_OF_ROWS_COLUMNS_NULL_VALUES____##################

summary(olist_df)
nrow(olist_df)
ncol(olist_df)
colnames(olist_df)



# number of row is 117601
# number of col is 33





#########################__________DATA_PREPERATIONS__/___REMOVE_UNNECESARY_COLUMNS____#############################################

# unique(olist_df_test['order_item_id'])
# olist_df_test['product_name_lenght']




# #### I am removing the seller_id,product_id,order_id,customer_id,customer_unique_id,order_item_id,product_name_lenght,product_description_lenght,product_photos_qty

olist_df['seller_id'] <- NULL
olist_df['product_id'] <- NULL
olist_df['order_id'] <- NULL
olist_df['customer_id'] <- NULL
olist_df['customer_unique_id'] <- NULL
olist_df['order_item_id'] <- NULL
olist_df['product_name_lenght'] <- NULL
olist_df['product_description_lenght'] <- NULL
olist_df['product_photos_qty'] <- NULL

olist_df['product_weight_g'] <- NULL
olist_df['product_length_cm'] <- NULL
olist_df['product_height_cm'] <- NULL
olist_df['product_width_cm'] <- NULL


################### _________SAVE_CLENED_VERSION_OF_OLIST_DF______________ #########
# write.csv(olist_df,"./db/olist_df_cleaned.csv", row.names = FALSE)

# order_id and customer_id is not necessary 
# olist_df <- orders_dataset[,c("order_status","order_purchase_timestamp","order_approved_at","order_delivered_carrier_date","order_delivered_customer_date","order_estimated_delivery_date")]



# order_estimated_delivery_date = Shows the estimated delivery date that was informed to customer at the purchase moment.
# order_delivered_customer_date = Shows the actual order delivery date to the customer.
# order_delivered_carrier_date = Shows the order posting timestamp. When it was handled to the logistic partner.
# shipping_limit_date = Shows the seller shipping limit date for handling the order over to the logistic partner.  ##
# order_approved_at = Shows the payment approval timestamp.
# order_purchase_timestamp = Shows the purchase timestamp.


# payment_sequential = a customer may pay an order with more than one payment method. If he does so, a sequence will be created to
# payment_type = method of payment chosen by the customer.
# payment_installments = number of installments chosen by the customer.

head(olist_df)
unique(olist_df["order_status"])
ggplot(data=olist_df,aes(x=order_status))+
  geom_bar(stat = "count")




# As I am planning only to find out the late delivery thats why I am going to drop outhers value from order_status
olist_df = filter(olist_df, order_status %in% c("delivered"))
unique(olist_df["order_status"])
head(olist_df)




# %d -> Day
# %m -> Numeric Month
# %b -> Abbreviated Month
# %B -> Full Month
# %y -> 2-digit year
# %Y -> 4-digit year

# Now I am going to convert the Date related columns to DateTime format in olist_df dataframe 
olist_df$order_purchase_timestamp <- as.POSIXct(olist_df$order_purchase_timestamp, format = "%Y-%m-%d %H:%M:%S")
olist_df$order_approved_at <- as.POSIXct(olist_df$order_approved_at, format = "%Y-%m-%d %H:%M:%S")
olist_df$shipping_limit_date <- as.POSIXct(olist_df$shipping_limit_date, format = "%Y-%m-%d %H:%M:%S")
olist_df$order_delivered_carrier_date <- as.POSIXct(olist_df$order_delivered_carrier_date, format = "%Y-%m-%d %H:%M:%S")
olist_df$order_delivered_customer_date <- as.POSIXct(olist_df$order_delivered_customer_date, format = "%Y-%m-%d %H:%M:%S")
olist_df$order_estimated_delivery_date <- as.POSIXct(olist_df$order_estimated_delivery_date, format = "%Y-%m-%d %H:%M:%S")


############################ ______CONVERTING_CHAR_TO_FACTOR______ ######################

olist_df$late_delivery = as.factor(olist_df$late_delivery)
olist_df$customer_city = as.factor(olist_df$customer_city)
olist_df$customer_state = as.factor(olist_df$customer_state)
olist_df$product_category_name = as.factor(olist_df$product_category_name)
olist_df$seller_city = as.factor(olist_df$seller_city)
olist_df$seller_state = as.factor(olist_df$seller_state)
olist_df$payment_type  = as.factor(olist_df$payment_type)



head(olist_df)

typeof(olist_df$order_purchase_timestamp)
class(olist_df$order_purchase_timestamp)
# approval_delay is time difference between the purchase time of the product  and the time taken to approve the order from the seller.
olist_df["approval_delay"] = difftime(olist_df$order_approved_at,olist_df$order_purchase_timestamp,units="hours")
# ?boxplot

boxplot(olist_df["approval_delay"])

# There are outliers. I will delete the approval_delay whose values are more than 150 hours. 
olist_df = olist_df %>% filter(olist_df$approval_delay < 150)
boxplot(olist_df["approval_delay"])



# Order status column have only one value which is order_status, So I am going to drop it now.
olist_df$order_status <- NULL
# Shows the order posting timestamp. When it was handled to the logistic partner.
# carrier_delay is the time it was taken to be handled to the logistic partner after its approval by the seller.

olist_df["carrier_delay"] = difftime(olist_df$order_delivered_carrier_date ,olist_df$order_approved_at,units="hours")

boxplot(olist_df["carrier_delay"])
# carrier_delay has some negative values those are not possible at all. I am going to filter out the negative values
olist_df = olist_df %>% filter(olist_df$carrier_delay > 0 & olist_df$carrier_delay <800)

boxplot(olist_df["carrier_delay"],main = "carrier delay in hours")


# order_delay is the time difference of the actual delivery time - the expected delivery date
olist_df["order_delay"] = difftime(olist_df$order_estimated_delivery_date ,olist_df$order_delivered_customer_date,units="days")
boxplot(olist_df['order_delay'], main="order delayed in days")
# I will consider +50 and -150 days as outliers and remove them to get more precise models

olist_df = olist_df %>% filter(olist_df$order_delay > -150 & olist_df$order_delay <50)
boxplot(olist_df['order_delay'], main="order delayed in days")


 
olist_df['late_delivery'] <- ifelse(olist_df['order_delay']  < 0,"yes","no")

table(olist_df['late_delivery']) / nrow(olist_df['late_delivery']) *100
summary(olist_df)

ggplot(data=olist_df,aes(x=late_delivery))+
  geom_bar(stat = "count")

# par(mfrow=c(1,2))

colnames(olist_df)
prop.table(table(olist_df$late_delivery))



# ################### Undersampling   #####################################

# Index of the values with Yes and NO
# Sample the indices
# Create a new dataset with the sampled indices

Yes <- which(olist_df$late_delivery == "yes")
No <- which(olist_df$late_delivery == "no")
length(No)


no_sampled_index <- sample(No,length(Yes))
df_balanced <- olist_df[c(no_sampled_index,Yes),]

table(df_balanced$late_delivery)
# write.csv(df_balanced,"./db/olist_df_cleaned.csv", row.names = FALSE)

##### ___________________ testing and splitting balanced_df datasets code__________ #################


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


################## modeling_part with balanced_df ############################
###################### rpart #################################


model_balanced <- rpart(late_delivery~.,data = train_df,method = "class")
summary(model)

# rpart.plot(model)


pred_ = predict(model_balanced,test_df,type = "class")
acc = sum(pred_ == test_df$late_delivery)/nrow(test_df)
print(acc)

conf_matrix <- table(test_df$late_delivery,pred_)
conf_matrix


accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)

# conf_matrix
# pred_
#       no  yes
# no  1164  602
# yes  484 1282
# accuracy: 0.6925255


##### ___________________ testing and splitting balanced_df datasets code__________ #################

selected_col = c("customer_city","order_approved_at","order_delivered_carrier_date","shipping_limit_date","order_purchase_timestamp",
                 "order_estimated_delivery_date","customer_state","order_delivered_customer_date","customer_zip_code_prefix","carrier_delay",
                 "freight_value","seller_city","product_category_name","late_delivery")
df_selected = df_balanced[,selected_col]


set.seed(3456)
partion_index <- createDataPartition(df_selected$late_delivery, p = .8,
                                     list = FALSE,
                                     times = 1)
train_df <-df_selected[partion_index,]
test_df <-df_selected[-partion_index,]

train_df$order_delay <- NULL
test_df$order_delay <- NULL 

# ggplot(data=model_df,aes(x=late_delivery))+
#   geom_bar(stat = "count")


################## modeling_part with balanced_df ############################
###################### rpart #################################


model_selected <- rpart(late_delivery~.,data = train_df,method = "class")
summary(model_selected)

# rpart.plot(model)


pred_ = predict(model_selected,test_df,type = "class")
acc = sum(pred_ == test_df$late_delivery)/nrow(test_df)
print(acc)

conf_matrix <- table(test_df$late_delivery,pred_)
conf_matrix


accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)

# conf_matrix
# pred_
#       no  yes
# no  1293  473
# yes  537 1229

# 
# print(accuracy)
# 0.714043
############ ___________plotting_confusion_matrix________ ###########





confusion_matrix <- as.data.frame(table(test_df$late_delivery,pred_))

ggplot(data = confusion_matrix,
       mapping = aes(x = "ar1",
                     y = "Var2")) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") # if your results aren't quite as clear as the above example





















##### _____________________testing and splitting with olist_df __________ #################


set.seed(3456)
partion_index <- createDataPartition(olist_df$late_delivery, p = .8,
                                     list = FALSE,
                                     times = 1)
train_df <-olist_df[partion_index,]
test_df <-olist_df[-partion_index,]

train_df$order_delay <- NULL
test_df$order_delay <- NULL 

# ggplot(data=model_df,aes(x=late_delivery))+
#   geom_bar(stat = "count")


################## modeling_part with balanced_df ############################
###################### rpart #################################


model_df <- rpart(late_delivery~.,data = train_df,method = "class")
summary(model_df)

# rpart.plot(model)


pred_ = predict(model_df,test_df,type = "class")
acc = sum(pred_ == test_df$late_delivery)/nrow(test_df)
print(acc)

conf_matrix <- table(test_df$late_delivery,pred_)
conf_matrix
typeof(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)

# > print(acc)
# [1] 0.9248706
# > conf_matrix
# pred_
# no   yes
# no  20618   217
# yes  1481   285
