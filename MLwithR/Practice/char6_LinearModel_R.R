reg <- function(y,x){
  x<- as.matrix(x)
  x<- cbind(Intercept=1,x)
  solve(t(x)%*%x)%*% t(x) %*% y
}

insurance <- read.csv('xx',stringsAsFactors = TRUE)
cor(insurance[c('age','bmi','children','charges')])
pairs(insurance[c('age','bmi','children','charges')])

library(psych)
pairs.panels(insurance[c('age','bmi','children','charges')])

ins_model <- lm(charges~.,data= insurance)
summary(ins_model)

insurance$age2 <- insurance$age^2
insurance$bim30 <- ifelse(insurance$bmi>=30,1,0)
lm(charges~bmi30 + smokeryes + bim30:smokeryes,data= insurance)
ins_model2 <- lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region,data= insurance)

# CART
wine <- read.csv('whitewines.csv')
hist(wine$quality)
wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

library(rpart)
m.rpart <- rpart(quantile~.,data=wine_train)
m.rpart
summary(m.rpart)

library(rpart.plot)
rpart.plot(m.rpart,digits = 3)
rpart.plot(m.rpart,digits = 3,digits=4,fallen.leaves = TRUE,type=3,extra=101)
p.rpart <- predict(m.rpart,wine_test)
cor(p.rpart,wine_test$quality)

MAE <- function(actual,predicted){
  mean(abs(actual-predicted))
}
MAE(p.rpart,wine_test$quality)

# Model Tree
library(RWeka)
m.m5p <- M5P(quality~.,data=wine_train)
summary(m.m5p)

p.m5p <- predict(m.m5p,wine_test)
summary(p.m5p)
cor(p.m5p,wine_test$quality)
MAE(wine_test$qulity,p.m5p)