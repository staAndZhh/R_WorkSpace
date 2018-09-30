# lm demo1 
data(women)
lm.model <- lm(weight~height-1,data = women)
summary(lm.model)
coefficients(lm.model) # 参数估计值
confint(lm.model,parm = 'speed',level = 0.95)
fitted(lm.model) # 模型的预测值
anova(lm.model) # 方差分析
vcov(lm.model) # 协方差
residuals(lm.model) # 模型残差
influence(lm.model)
AIC(lm.model)
par(mfrow = c(2,2))
plot(lm.model) # 评价拟合的诊断图
# predict
newdata <- data.frame(height = c(110,120,130))
predict(lm.model,newdata)

# lm
data("anscombe")
plot(y2~x1,data = anscombe)
lmfit2 <- lm(y2~poly(x1,2),data = anscombe)
lines(sort(anscombe$x1),lmfit2$fitted.values[order(anscombe$x1)],col = 'red')

# lm demo2
house <- read.csv('house_rental.csv',header = TRUE)
lmfit <- lm(Price~Sqft,data = house)
plot(Price~Sqft,data = house)
abline(lmfit,col = 'red')

newdata <- data.frame(Sqft = c(800,900,1000))
predict(lmfit,newdata)
lmfit$coefficients[1]
lmfit$coefficients[2]
c(800,900,1000)*lmfit$coefficients[2]+ lmfit$coefficients[1]
predict(lmfit,newdata,interval = 'prediction')
plot(Price~Sqft,data = house)
pred.res <- predict(lmfit,data = house,interval = 'prediction')
pred.conf <- predict(lmfit,data = house,interval = 'confidence')
lines(pred.conf[,'fit']~house$Sqft,col = 'red',lty = 1, lwd = 3)
lines(pred.conf[,'lwr']~house$Sqft,col = 'blue',lty = 3, lwd = 3)
lines(pred.conf[,'upr']~house$Sqft,col = 'blue',lty = 3, lwd = 3)
lines(pred.conf[,'lwr']~house$Sqft,col = 'green',lty = 3, lwd = 3)
lines(pred.conf[,'upr']~house$Sqft,col = 'green',lty = 3, lwd = 3)


predicted <- predict(lmfit,data = house)
