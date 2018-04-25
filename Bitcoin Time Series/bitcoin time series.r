# Bitcoin Time Series Analysis
library(tseries)
library(forecast)

bitcoin <- read.csv("bitcoin history.csv")
head(bitcoin)

plot(as.Date(bitcoin$Date), bitcoin$Close, type='l', ylab='Dollars', xlab='Date',
     ylim=c(6000,20000), main='Bitcoin Closing Price from 12/17/2017-3/16/2018')


# convert to time series 
# graph history and ols model
bitcoin.ts <- ts(bitcoin$Close)

plot(bitcoin.ts, ylim=c(6000,20000), ylab="Dollars", main="Bitcoin Closing Price")

bitcoin$Time <- seq(1,90,1)
LR.model <- lm(Close~Time, data=bitcoin)
abline(LR.model, col='blue')    
legend('topright', c('History', 'OLS'), col=c('black','blue'), lty=1)

summary(LR.model)

# Looks like a downward trend so we will test using ADF and KPSS
# ADF (null = unit root exists (nonstationary))
# KPSS (null = data is trend-stationary)
adf.test(bitcoin.ts)   # fail to reject: unit root exists; nonstationary
kpss.test(bitcoin.ts)    # reject null: trend-nonstationary


# Use auto.arima to obtain best ARIMA model
auto.arima(bitcoin.ts)   # results indicate random walk


# first difference analysis
bitcoin.diff1 <- diff(bitcoin.ts, differences=1)

plot(bitcoin.diff1, main="First Difference")  # first differences are constant and stationary

acf(bitcoin.diff1, lag.max=20)
acf(bitcoin.diff1, lag.max=20, plot=FALSE)


# forecast with with 80% and 95% confidence intervals
bitcoin.fit <- arima(bitcoin.ts, order=c(0,1,0))

forecast(bitcoin.fit)
plot(forecast(bitcoin.fit))
