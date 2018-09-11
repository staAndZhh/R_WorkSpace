library(zoo)           ###时间格式预处理
library(xts)            ###同上
library(timeSeires)      ###同上
library(urca)           ###进行单位根检验
library(tseries)         ###arma模型
library(fUnitRoots)     ###进行单位根检验
library(FinTS)         ###调用其中的自回归检验函数
library(fGarch)        ###GARCH模型
library(nlme)          ###调用其中的gls函数
library(fArma)        ###进行拟合和检验
library(TTR)
library(forecast)
library(fUnitRoots)
# 分析流程----
# 读取数据，绘制图形
# 分解时间序列：分解非季节,分解季节性,季节性修正
# 指数平滑法预测：简单指数平滑，holt指数平滑，holt-winter指数平滑
# ARIMA模型：时间序列差分，合适的arima模型，arima模型预测
# read data----
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births,frequency = 12,start = c(1946,1))
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))

plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)

logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)
# 分解非季节数据
# 移动平均 ----
# 非季节性时间序列的趋势部分，使之能够用相加模型进行描述，最常用的方法便是平滑法
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA8)
# 分解季节性数据----
# 相加模型的趋势和季节性部分,decompose()函数,估计时间序列中的趋势,季节和不规则部分：必须是相加模型
#  seansonal trend random
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$trend
birthstimeseriescomponents$seasonal
birthstimeseriescomponents$random
plot(birthstimeseriescomponents)
# 季节性因素调整----
# 相加模型：估计季节性部分修正时间序列，也可以从原始序列中去除掉估计得季节性部分。我们可以通过“decompose()” 函数使用估计出的季节性部分进行计算
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
# 指数平滑----
# 时间序列数据的短期预测

# 简单指数平滑法----
# 相加模型，恒定水平和没有季节性变动的时间序列，你可以使用简单指数平滑
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- HoltWinters(rainseries,beta = FALSE,gamma =FALSE)
rainseriesforecasts$fitted  # 默认情况holtwinters仅给出原始时间序列覆盖时期的预测
plot(rainseriesforecasts)  # 画出实际值和预测值
rainseriesforecasts$SSE  # 样本内的预测误差的误差平方和
HoltWinters(rainseries,beta = FALSE,gamma =FALSE,l.start = 23.56)
rainseriesforecasts2 <- forecast(rainseriesforecasts,h=8)  # 预测未来的8期数据
plot(rainseriesforecasts2)
acf(rainseriesforecasts2$residuals,lag.max = 20)
acf(rainseriesforecasts2$residuals[2:100],lag.max = 20) # 样本内预测误差的相关性检验
Box.test(rainseriesforecasts2$residuals,lag=20,type = 'Ljung-Box')  #查看之后非0相关是否显著,0.6,不足以证明预测误差非0自相关
plot.ts(rainseriesforecasts2$residuals)# 正态分布，0均值，方差不变检验
plotForecastErrors <- function(forecasterrors)
{
  # make a red histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) + mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # mybins <- seq(mymin, mymax, mybinsize)
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(rainseriesforecasts2$residuals)
plotForecastErrors(rainseriesforecasts2$residuals[2:100])

# 霍尔特指数平滑法----
# 增长或降低趋势的、没有季节性的相加模型
# 估计当前时间点的水平和斜率。 其平滑化是由两个参数控制的，alpha，用于估计当前时间点的水平，beta，用于估计当前时间点趋势部分的斜率。 正如简单指数平滑法一样，alpha 和 beta 参数都介于 0 到 1之间，并且当参数越接近 0，大多数近期的观测则将占据预测更小的权重
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start = c(1886))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries,gamma =FALSE)
skirtsseriesforecasts$SSE
plot(skirtsseriesforecasts)
# 默认初始值:水平值：第一个值，斜率：第二个值减去第一个
HoltWinters(skirtsseries,gamma =FALSE,l.start = 608, b.start = 9)
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts2)
acf(skirtsseriesforecasts2$residuals,lag.max = 20)
acf(skirtsseriesforecasts2$residuals[!is.na(skirtsseriesforecasts2$residuals)],lag.max = 20)  # acf检验
Box.test(skirtsseriesforecasts2$residuals[!is.na(skirtsseriesforecasts2$residuals)],lag = 20,type = 'Ljung-Box') #box检验
plot.ts(skirtsseriesforecasts2$residuals) # 方差不变检验
plotForecastErrors(skirtsseriesforecasts2$residuals)  # 零均值检验

# 霍尔特指数平滑法----
#  有一个增长或降低趋势并存在季节性可被描述成为相加模型的时间序列：霍尔特-温特指数平滑
# 估计当前时间点的水平，斜率和季节性部分。平滑化依靠三个参数来控制：alpha，beta 和 gamma，分别对应当前时间点上的水平，趋势部分的斜率和季节性部分。 参数 alpha，beta 和 gamma的取值都在 0 和 1 之间，并且当其取值越接近 0 意味着对未来的预测值而言最近的观测值占据相对较小的权重
logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts,h=48)
plot(souvenirtimeseriesforecasts2)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot(souvenirtimeseriesforecasts2$residuals)
plotForecastErrors(souvenirtimeseriesforecasts2$residuals[!is.na(souvenirtimeseriesforecasts2$residuals)]) 
# 时间序列上面连续的值之间相关性没有要求。但是，如果你想使用指数平滑法计算出预测区间，那么预测误差必须是不相关的，而且必须是服从零均值、方差不变的正态分布
# ARIMA----
# 数据平稳并检验
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)
skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)
urdfTest(skirtsseriesdiff2)
unitrootTest(skirtsseriesdiff2)

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)
urdfTest(kingtimeseriesdiff1)
unitrootTest(kingtimeseriesdiff1)

#参数p,q的确定
acf(kingtimeseriesdiff1, lag.max=20)
acf(kingtimeseriesdiff1, lag.max=20,plot = FALSE)
pacf(kingtimeseriesdiff1, lag.max=20)
pacf(kingtimeseriesdiff1, lag.max=20,plot = FALSE)

# 自动化建模
auto.arima(kings)

volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)
acf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20)
auto.arima(volcanodust,ic='bic')

# 预测
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1))
kingstimeseriesforecasts <- forecast(kingstimeseriesarima,h=5,level = c(99.5))
plot(kingstimeseriesforecasts)

acf(kingstimeseriesforecasts$residuals, lag.max=20) # 误差相关性
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box") # 大于0.05 误差不自相关
plot.ts(kingstimeseriesforecasts$residuals)  # 等方差且方差为0的数据
plotForecastErrors(kingstimeseriesforecasts$residuals) # 等方差且方差为0的数据

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31)
plot(volcanodustseriesforecasts)

acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(volcanodustseriesforecasts$residuals)
plotForecastErrors(volcanodustseriesforecasts$residuals) 
mean((volcanodustseriesforecasts$residuals))
