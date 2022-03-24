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
# install.packages("ISLR")
# install.packages("caret")
# install.packages("rpart")
# install.packages("tree")
# install.packages("randomForest")
library(randomForest)
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

# ########################   ____QUESTION__5__C____ #################################
train.Carseats$sales_target <- as.character(train.Carseats$sales_target)
train.Carseats$sales_target <- as.factor(train.Carseats$sales_target)

model.forest <- randomForest(sales_target~.,data = train.Carseats)

pred.forest = predict(model.forest,test.Carseats,type = "class")

acc = sum(pred.forest  == test.Carseats$sales_target)/nrow(test.Carseats)

print(acc)
print(test.Carseats)
# rm(train.Carseats)
# rm(test.Carseats)
# rm(Carseats)
# #######################################

pred.rpart.train = predict(model.rpart,train.Carseats,type = "class")

acc.train = sum(pred.rpart.train == train.Carseats$sales_target)/nrow(train.Carseats)

print(acc.train)

# ############################ ____Question__5__b_____ #######################
# cp= model.rpart$cptable[which.min(model.rpart$cptable[,"xerror"]),"CP"]
# I am using the printcp() to examine the cross-validation error results. I am using this fragment of code to select
# the to automatically select the complexity parameter associated with the smallest cross-validation error. 

pruned.rpart<- prune(model.rpart, cp= model.rpart$cptable[which.min(model.rpart$cptable[,"xerror"]),"CP"])
pred.pruned.rpart = predict(pruned.rpart,test.Carseats,type = "class")
pruned.test.acc = sum(pred.pruned.rpart == test.Carseats$sales_target)/nrow(test.Carseats)
print(pruned.test.acc)


pruned.rpart<- prune(model.rpart, cp= model.rpart$cptable[which.min(model.rpart$cptable[,"xerror"]),"CP"])
pred.pruned.rpart = predict(pruned.rpart,train.Carseats,type = "class")
pruned.train.acc = sum(pred.pruned.rpart == train.Carseats$sales_target)/nrow(train.Carseats)
print(pruned.train.acc)


####################### ______QUESTION__2__a____ #####################################
library(gamair)
data(hubble)
head(hubble,10)
summary(hubble)

hub.mod <-gam(y~s(x), data=hubble)

summary(hub.mod)

plot(hubble$x,hubble$y,xlab="Distance", ylab="Velocity")
abline(0,hub.mod$coefficients, col="blue")
