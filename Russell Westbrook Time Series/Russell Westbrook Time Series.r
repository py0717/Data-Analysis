##########################################################################################
# Time Series Analysis: Points Scored Each Game by Russell Westbrook in 2016-2017 Season #
# First Model = Simple Exponential Smoothing                                             #
# Second Model = ARIMA                                                                   #
##########################################################################################

# set up
library("TTR")
library("forecast")

getwd()

stats <- read.csv("OKC Guards Data.csv", row.names=NULL)
head(stats)

# data clean
RW <- stats[which(stats$Player=="Russell Westbrook"),]
RW <- RW[order(RW$Date),]

head(RW)
tail(RW)

RW <- RW[,c('PTS')]
RW <- ts(RW)

RW    # played 81 games in 2016-2017 season

# exploratory analysis and graphs
summary(RW)
boxplot(RW, main="Points Per Game")

hist <- hist(RW, col="gray", xlab="Points", ylab="Number of Games", main="Points Scored in Games")

plot(RW, xlab="Game Number", ylab="Points", main="Points in Games Through Season")

# smooth the data using various spans of simple of moving average and look for trends 
plot(SMA(RW, n=2), xlab="Game Number", ylab="Points", main="Points in Games Through Season")
plot(SMA(RW, n=5), xlab="Game Number", ylab="Points", main="Points in Games Through Season")
plot(SMA(RW, n=8), xlab="Game Number", ylab="Points", main="Points in Games Through Season")


######################################
# SIMPLE EXPONENTIAL SMOOTHING MODEL #
######################################
# plot original values against smoothing model forecast
RW.ts1 <- HoltWinters(RW, alpha=NULL, beta=FALSE, gamma=FALSE, l.start=32)
plot(RW.ts1)

# forecast next 5 games 
# plot forecast with confidence intervals
RWForecast <- forecast(RW.ts1, h=5)
summary(RWForecast)

plot(RWForecast, xlab="Game Number", ylab="Points")

# check to see that there are no correlations between forecast errors for successive predictions
acf(RWForecast$residuals, na.action=na.pass, lag.max=20)  # lags 2 and 3 exceed significance bounds

# Ljung-Box test 
Box.test(RWForecast$residuals, lag=20, type="Ljung-Box")


############### 
# ARIMA MODEL #
###############

# use auto.arima function to obtain best ARIMA model
auto.arima(RW)

# plot original values against ARIMA model forecast
RW.ts2 <- arima(RW, order=c(0,0,3))

plot(RW, col='red', xlab="Game Number", ylab="Points", main="Points in Games Through Season")
lines(fitted(RW.ts2), col='blue')

# forecast next 5 games 
# plot forecast with confidence intervals
RWForecast2 <- forecast(RW.ts2, h=5)
summary(RWForecast2)

plot(RWForecast2, xlab="Game Number", ylab="Points")

# check to see that there are no correlations between forecast errors for successive predictions
acf(RWForecast2$residuals, lag.max=20)  # no lags that exceed significance bounds

# Ljung-Box test 
Box.test(RWForecast2$residuals, lag=20, type="Ljung-Box")

