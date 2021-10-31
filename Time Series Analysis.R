rm(list=ls())
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip=3)
kings
tskings <- ts(kings)
tskings
no_of_births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
no_of_births
tsbirths <- ts(no_of_births, 1:100, frequency = 2)
tsbirths
birthplot <- plot.ts(tsbirths)
birthplot
?ts
plot.ts(no_of_births)

# decomposing time series
# non-seasonal data
install.packages("TTR")
library("TTR")
tsbirthsSMA <- SMA(no_of_births, n=2)
tsbirthsSMA
plot.ts(tsbirthsSMA)
tskingsSMA <- SMA(kings, n=2)
tskingsSMA
plot.ts(tskingsSMA)

# seasonal data
# decomposing the time series into its components i.e
# seasonal, trend, irregular components stored in named elements
# "seasonal", "trend" and "random" respectively
tsbirthscomp <- decompose(tsbirthsSMA)
tsbirthscomp
# You can print out estimated values of seasonal as
tsbirthscomp$seasonal
plot(tsbirthscomp) # a plot of the observed, and the 3 components

## Seasonal Adjusting
tsbirthss_adjust <- tsbirths - tsbirths$seasonal 

# Forecasts using Exponential Smoothing
# Simple Exponential Smoothing
precipitation <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
precipitation
tsprecipitation <- ts(precipitation, start = 1813, end = 1912, frequency = 1)
tsprecipitation
plot.ts(precipitation)
# make forecasts
# use the HoltWinter() funtion
# set beta = FALSE and gamma = FALSE
tsprecipitation_f <- HoltWinters(tsprecipitation, beta = FALSE, gamma = FALSE)
tsprecipitation_f
tsprecipitation_f$fitted
plot(tsprecipitation_f)
# SSEs for forecast errors for 
#the time period covered by our original time series
tsprecipitation_f$SSE
### TRY
## HoltWinter(tsprecipitation, beta= FALSE, gamma = FALSE, 1.start =23.56)
install.packages("forecast")
library(forecast)
tsprecipitation_f2 <- forecast(tsprecipitation, h=8)
tsprecipitation_f2
plot(tsprecipitation_f2)
??forecast

### Holt's Exponential Smoothing
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirts
tsskirts <- ts(skirts,start = c(1866))
tsskirts
plot.ts(tsskirts)
tsskirts_f <- HoltWinters(tsskirts, gamma = FALSE)
tsskirts_f
# SSE
tsskirts_f$SSE
plot(tsskirts_f)
# predictive model for Holt's exponential smoothing
HoltWinters(tsskirts, gamma = FALSE, l.start = 608, b.start = 9)
tsskirts_f2 <- forecast(tsskirts_f, h=19)
tsskirts_f2
plot(tsskirts_f2)
# Improving model performance
# We make a correlogram and carry out Ljung-Box test by typing:
acf(tsskirts_f2, lag.max = 20)
Box.test(tsskirts_f2$residuals, lag = 20, type = "Ljung-Box")
plot.ts(tsskirts_f2$residuals, y=NULL, plot.type = c("multiple","single")) # make a time plot

## HoltWinters exponential smoothing
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir
tssouvenir <- ts(souvenir, frequency = 12, start = c(1987,1))
tssouvenir
tssouvenir_f <- HoltWinters(tssouvenir, beta = FALSE, gamma = FALSE)
tsskirts_f
plot(tssouvenir_f)
tssouvenir_f$SSE
tssouvenir_f2 <- forecast(tssouvenir_f)
tsskirts_f2
plot(tssouvenir_f2)
# improving model performance
acf(tssouvenir_f2, lag.max = 20)
Box.test(tssouvenir_f2$residuals, lag = 20, type = "Ljung-Box")
plot.ts(tssouvenir_f2$residuals) # make a time plot
hist(tssouvenir_f2$residuals)

logtsskirts <- log(tsskirts)
logtsskirts
logtsskirts_f <- HoltWinters(logtsskirts, beta = FALSE, gamma = FALSE)
logtsskirts_f
install.packages("tidyverse")
install.packages("fpp2")
library(tidyverse)
library(fpp2)
goog.train <- window(goog, end=900)
goog.train
goog.test <- window(goog, start=901)
goog.test
holt.goog <- holt(goog.train, h = 100)
holt.goog
# plot forecasts
autoplot(holt.goog)
# In our model alpha=0.9967 meaning fast day-to-day  movements
# beta=0.0001 meaning slow learning for the trend
holt.goog$model
# checking accuracy of our model
accuracy(holt.goog, goog.test)
# identify optimal alpha parameter
beta <- seq(.0001, .5, by = .001)
RMSE <- NA
for(i in seq_along(beta)) {
  fit <- holt(goog.train, beta = beta[i], h = 100)
  RMSE[i] <- accuracy(fit, goog.test)[2,2]
}

# convert to a data frame and idenitify min alpha value
beta.fit <- data_frame(beta, RMSE)
beta.min <- filter(beta.fit, RMSE == min(RMSE))

# plot RMSE vs. alpha
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta.min, aes(beta, RMSE), size = 2, color = "blue")

### ARIMA models
tsskirtsdiff1 <- diff(tsskirts, differences = 1)
tsskirtsdiff1
plot.ts(tsskirtsdiff1)
tsskirtsdiff2 <- diff(tsskirts, differences = 2)
tsskirtsdiff2
tskingsdiff1 <- diff(tskings, differences = 1)
tskingsdiff1
plot.ts(tsskirtsdiff2)
# selecting a candidate ARIMA Model
# use the acf() and pacf()
acf(tskingsdiff1, lag.max = 20)  # plot a correlogram
acf(tskingsdiff1, lag.max = 20, plot = FALSE) # get autocorrelation values
pacf(tskingsdiff1, lag.max = 20)  # plot a partial correlogram
pacf(tskingsdiff1, lag.max = 20, plot = FALSE) # get the partial autocorrelation
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodust
tsvolcanodust <- ts(volcanodust, start = c(1500))
tsvolcanodust
plot.ts(tsvolcanodust)
acf(tsvolcanodust, lag.max = 20)  # plot a correlogram
acf(tsvolcanodust, lag.max = 20, plot = FALSE) # get autocorrelation values
pacf(tsvolcanodust, lag.max = 20) # plot a partial correlogram
pacf(tsvolcanodust, lag.max = 20, plot = FALSE) # get partial autocorrelation values
auto.arima(tsvolcanodust) # find an appropriate model using auto.arima()

## Forecasting using an ARIMA Model
# use the predictive model for making forecasts for future values of your time series
tskingsarima <- arima(tskings, order = c(0,1,1)) # fit an ARIMA(0,1,1)
tskingsarima
tskingsarima_f <- forecast(tskingsarima, h=5, level=c(99.5))
tskingsarima_f
plot(tskingsarima_f)
acf(tskingsarima_f$residuals, lag.max = 20)  # plot a correlogram
Box.test(tskingsarima_f$residuals, lag = 20, type = "Ljung-Box")
plot.ts(tskingsarima_f$residuals)
tsvolcanodustarima <- arima(tsvolcanodust, order = c(2,0,0))
tsvolcanodustarima
tsvolcanodust_f <- forecast(tsvolcanodustarima, h=31)
tsvolcanodust_f
plot(tsvolcanodust_f)
acf(tsvolcanodust_f$residuals, lag.max = 20)
Box.test(tsvolcanodust_f$residuals, lag = 20, type = "Ljung-Box")
plot.ts(tsvolcanodust_f$residuals) # plot a time plot of forecast errors
mean(tsvolcanodust_f$residuals)
