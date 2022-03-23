# install.packages("nlme")
library(nlme)
library(dplyr)
print(Gun$Physique)
summary(Gun)

?nlme
?Gun
team_list = unique(Gun$Team)
Gun %>% filter(Method == 'M1')
Gun %>% filter(Team == 'T1S' | Team == 'T2S')



gun.lme = lme(rounds~Team,data = Gun,random = ~1|Method)
summary(gun.lme)
colnames(Gun)

gun.lme = lme(rounds~Physique,data = Gun,random = ~1|Method)
summary(gun.lme)
colnames(Gun)



gun.lme = lme(rounds~Physique,data = Gun)
summary(gun.lme)
# 
# ##################### ___QUESTION_5______ #####################
install.packages("ISLR")
install.packages("caret")
install.packages("rpart")

library(ISLR)
library(caret)
library(rpart)
library(rpart.plot)
# help(Carseats)

head(Carseats)

Carseats$sales_target <- ifelse(Carseats$Sales  > 8,TRUE,FALSE)

# Split the datasets randomly 
set.seed(3456)
trainIndex <- createDataPartition(Carseats$sales_target, p = .8,
                                  list = FALSE,
                                  times = 1)
# create train and test datasets
train.Carseats <-Carseats[trainIndex,]
test.Carseats <-Carseats[-trainIndex,]

# 
train.Carseats$Sales <- NULL
test.Carseats$Sales <- NULL

model.rpart <- rpart(sales_target~.,data = train.Carseats,method = "class")
summary(model.rpart)

rpart.plot(model.rpart)


pred.rpart = predict(model.rpart,test.Carseats,type = "class")

acc = sum(pred.rpart == test.Carseats$sales_target)/nrow(test.Carseats)

print(acc)



colnames(test.Carseats)
