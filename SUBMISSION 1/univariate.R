library(readxl)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)

# reading data from excel
data <- read_excel("CSUSHPISA.xls", col_types = c("date", "numeric"), skip = 10)

# start date
startd <- data$observation_date[1]

# constructing timeseries
prices_ts <- ts(data$CSUSHPISA, 
                  start = c(year(startd), month(startd)),
                  frequency = 12)

# plotting time series shows a distinct trend present in the data
plot(prices_ts)

# this is confirmed by the Augmented Dickey-Fuller Test
# (for hypothesis of unit-root against alternative of stationarity)
# result is -3.1079 with p-value = 0.1093:
# cannot reject the hypothesis on unit root, the series is non-stationary
adf.test(prices_ts)


# using kpss test the hypothesis is trend-stationarity
#
# the test yields the following values:
# KPSS Trend = 0.397, Truncation lag parameter = 5, p-value = 0.01
#
# hence we cannot reject the hypothesis of trend-stationary
#
kpss.test(prices_ts, null = "Trend")

# as heither unit root existence nor trend-stationarity is rejected 
# it means the data is not very informative or the tests are affected by outliers
# so we elect to difference the data once 
d_ts <- diff(prices_ts)

# differenced time series still showing sign of being non-stationary
# however this time it is less so because of a trend in the data
# and more so because of the heteroscedasticity (variable volatility) in the data
plot(d_ts)

# shows multiple gradually reducing self-correlations at all lags
acf(d_ts) 

# shows one significant spike at 1 and 13 months, suggesting seasonality and AR-Signature
# also shows less significant spikes at periods 3, 5 and 7
# this prompts an arima model with p = 7 
pacf(d_ts) 

# lets build a model (7,1,0) with seasonal order of 2 
# (taking into account two previous years for seasonality adjustment)
arima_710 <- arima(prices_ts,
                   order = c(7,1,0), 
                   seasonal = list(order = c(2,0,0), period = 12))

# Ljung-Box test output doesn't allow to reject the null hypothesis of autocorrelation in model residuals
# Q* = 16.364, df = 15, p-value = 0.3583
checkresiduals(arima_710)

# AIC information criterion value is -9.969872
AIC(arima_710)

# with this we conclude the overall adequacy of the model built

# plotting the forecasts from the model 
fc <- forecast(arima_710)

# these are in-sample predictions
fc_in_sample <- fitted(arima_710)

# in_sample accuracy of the model is
# ME        RMSE      MAE       MPE         MAPE      MASE         ACF1
#0.01565473 0.2314025 0.1385282 0.01440401 0.1012788 0.2127796 -0.003648528
accuracy(arima_710)

# plot original series, fitted values (in-sample predictions) 
# and forecasted values with ranges
autoplot(prices_ts) +
  autolayer(fc, series="forecast arima_710") +
  autolayer(fc_in_sample) +
  ggtitle("forecasts from arima_710 model") +
  xlab("Month") + ylab("home prices") +
  guides(colour=guide_legend(title="Forecast"))

