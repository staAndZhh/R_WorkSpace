# InfoGain
# Gini  index
# Chi-Squareed
# gain ratio

credit_rand <- credit[order(runif(1000)),]
credit_train <- credit_rand[1:900,]
credit_test <- credit_rand[901:1000,]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

library(C50)
credit_model <- C5.0(credit_train[-17],credit_train$default)
summary(credit_model)
credit_pred <- predict(credit_model,credit_test)
library(gmodels)
CrossTable(credit_test$default,credit_pred,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn=c('actual default','predicted default'))

# C4.5
credit_boost10 <- C5.0(credit_train[-17],credit_train$default,trials=10)
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10,credit_test)
CrossTable(credit_test$default,credit_boost_pred10,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn=c('actual default','predicted default'))

# cost matrix
error_cost <- matrix(c(0,1,4,0),nrow=2)
credit_cost <- C5.0(credit_train[-17],credit_train$default,costs=error_cost)
credit_cost_pred <- predict(credit_cost,credit_test)
CrossTable(credit_test$default,credit_cost_pred,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn=c('actual default','predicted default'))

# rule learning
#  ZeroR
mushrooms <- read.csv('mushrooms.csv',stringsAsFactors = TRUE)
mushrooms$veil_type <- NULL
table(mushrooms$type)

library(RWeka)
mushroom_1R <- OneR(type~.,data=mushrooms)
summary(mushroom_1R)

# RIPPER
mushroom_classifier <- JRip(type~odor + cap_color,data = mushrooms)
summary(mushroom_classifier)

