# lm
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

getwd()
