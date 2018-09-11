library(zoo)           ###ʱ���ʽԤ����
library(xts)            ###ͬ��
library(timeSeires)      ###ͬ��
library(urca)           ###���е�λ������
library(tseries)         ###armaģ��
library(fUnitRoots)     ###���е�λ������
library(FinTS)         ###�������е��Իع���麯��
library(fGarch)        ###GARCHģ��
library(nlme)          ###�������е�gls����
library(fArma)        ###������Ϻͼ���
library(TTR)
library(forecast)
library(fUnitRoots)
# ��������----
# ��ȡ���ݣ�����ͼ��
# �ֽ�ʱ�����У��ֽ�Ǽ���,�ֽ⼾����,����������
# ָ��ƽ����Ԥ�⣺��ָ��ƽ����holtָ��ƽ����holt-winterָ��ƽ��
# ARIMAģ�ͣ�ʱ�����в�֣����ʵ�arimaģ�ͣ�arimaģ��Ԥ��
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
# �ֽ�Ǽ�������
# �ƶ�ƽ�� ----
# �Ǽ�����ʱ�����е����Ʋ��֣�ʹ֮�ܹ������ģ�ͽ�����������õķ�������ƽ����
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA8)
# �ֽ⼾��������----
# ���ģ�͵����ƺͼ����Բ���,decompose()����,����ʱ�������е�����,���ںͲ����򲿷֣����������ģ��
#  seansonal trend random
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$trend
birthstimeseriescomponents$seasonal
birthstimeseriescomponents$random
plot(birthstimeseriescomponents)
# ���������ص���----
# ���ģ�ͣ����Ƽ����Բ�������ʱ�����У�Ҳ���Դ�ԭʼ������ȥ�������Ƶü����Բ��֡����ǿ���ͨ����decompose()�� ����ʹ�ù��Ƴ��ļ����Բ��ֽ��м���
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
# ָ��ƽ��----
# ʱ���������ݵĶ���Ԥ��

# ��ָ��ƽ����----
# ���ģ�ͣ��㶨ˮƽ��û�м����Ա䶯��ʱ�����У������ʹ�ü�ָ��ƽ��
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- HoltWinters(rainseries,beta = FALSE,gamma =FALSE)
rainseriesforecasts$fitted  # Ĭ�����holtwinters������ԭʼʱ�����и���ʱ�ڵ�Ԥ��
plot(rainseriesforecasts)  # ����ʵ��ֵ��Ԥ��ֵ
rainseriesforecasts$SSE  # �����ڵ�Ԥ���������ƽ����
HoltWinters(rainseries,beta = FALSE,gamma =FALSE,l.start = 23.56)
rainseriesforecasts2 <- forecast(rainseriesforecasts,h=8)  # Ԥ��δ����8������
plot(rainseriesforecasts2)
acf(rainseriesforecasts2$residuals,lag.max = 20)
acf(rainseriesforecasts2$residuals[2:100],lag.max = 20) # ������Ԥ����������Լ���
Box.test(rainseriesforecasts2$residuals,lag=20,type = 'Ljung-Box')  #�鿴֮���0����Ƿ�����,0.6,������֤��Ԥ������0�����
plot.ts(rainseriesforecasts2$residuals)# ��̬�ֲ���0��ֵ����������
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

# ������ָ��ƽ����----
# �����򽵵����Ƶġ�û�м����Ե����ģ��
# ���Ƶ�ǰʱ����ˮƽ��б�ʡ� ��ƽ�������������������Ƶģ�alpha�����ڹ��Ƶ�ǰʱ����ˮƽ��beta�����ڹ��Ƶ�ǰʱ������Ʋ��ֵ�б�ʡ� �����ָ��ƽ����һ����alpha �� beta ���������� 0 �� 1֮�䣬���ҵ�����Խ�ӽ� 0����������ڵĹ۲���ռ��Ԥ���С��Ȩ��
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start = c(1886))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries,gamma =FALSE)
skirtsseriesforecasts$SSE
plot(skirtsseriesforecasts)
# Ĭ�ϳ�ʼֵ:ˮƽֵ����һ��ֵ��б�ʣ��ڶ���ֵ��ȥ��һ��
HoltWinters(skirtsseries,gamma =FALSE,l.start = 608, b.start = 9)
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts2)
acf(skirtsseriesforecasts2$residuals,lag.max = 20)
acf(skirtsseriesforecasts2$residuals[!is.na(skirtsseriesforecasts2$residuals)],lag.max = 20)  # acf����
Box.test(skirtsseriesforecasts2$residuals[!is.na(skirtsseriesforecasts2$residuals)],lag = 20,type = 'Ljung-Box') #box����
plot.ts(skirtsseriesforecasts2$residuals) # ��������
plotForecastErrors(skirtsseriesforecasts2$residuals)  # ���ֵ����

# ������ָ��ƽ����----
#  ��һ�������򽵵����Ʋ����ڼ����Կɱ�������Ϊ���ģ�͵�ʱ�����У�������-����ָ��ƽ��
# ���Ƶ�ǰʱ����ˮƽ��б�ʺͼ����Բ��֡�ƽ���������������������ƣ�alpha��beta �� gamma���ֱ��Ӧ��ǰʱ����ϵ�ˮƽ�����Ʋ��ֵ�б�ʺͼ����Բ��֡� ���� alpha��beta �� gamma��ȡֵ���� 0 �� 1 ֮�䣬���ҵ���ȡֵԽ�ӽ� 0 ��ζ�Ŷ�δ����Ԥ��ֵ��������Ĺ۲�ֵռ����Խ�С��Ȩ��
logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts,h=48)
plot(souvenirtimeseriesforecasts2)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot(souvenirtimeseriesforecasts2$residuals)
plotForecastErrors(souvenirtimeseriesforecasts2$residuals[!is.na(souvenirtimeseriesforecasts2$residuals)]) 
# ʱ����������������ֵ֮�������û��Ҫ�󡣵��ǣ��������ʹ��ָ��ƽ���������Ԥ�����䣬��ôԤ���������ǲ���صģ����ұ����Ƿ������ֵ����������̬�ֲ�
# ARIMA----
# ����ƽ�Ȳ�����
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

#����p,q��ȷ��
acf(kingtimeseriesdiff1, lag.max=20)
acf(kingtimeseriesdiff1, lag.max=20,plot = FALSE)
pacf(kingtimeseriesdiff1, lag.max=20)
pacf(kingtimeseriesdiff1, lag.max=20,plot = FALSE)

# �Զ�����ģ
auto.arima(kings)

volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)
acf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20)
auto.arima(volcanodust,ic='bic')

# Ԥ��
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1))
kingstimeseriesforecasts <- forecast(kingstimeseriesarima,h=5,level = c(99.5))
plot(kingstimeseriesforecasts)

acf(kingstimeseriesforecasts$residuals, lag.max=20) # ��������
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box") # ����0.05 �������
plot.ts(kingstimeseriesforecasts$residuals)  # �ȷ����ҷ���Ϊ0������
plotForecastErrors(kingstimeseriesforecasts$residuals) # �ȷ����ҷ���Ϊ0������

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31)
plot(volcanodustseriesforecasts)

acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(volcanodustseriesforecasts$residuals)
plotForecastErrors(volcanodustseriesforecasts$residuals) 
mean((volcanodustseriesforecasts$residuals))