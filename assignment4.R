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

head(Carseats)

Carseats$Target <- ifelse(Carseats$Sales  > 8,TRUE,FALSE)


head(Carseats)
dt = sort(sample(nrow(Carseats), nrow(Carseats)*.7))

train.Carseats <-Carseats[dt,]
test.Carseats <-Carseats[-dt,]


head(test.Carseats)
# rm(Carseats)
