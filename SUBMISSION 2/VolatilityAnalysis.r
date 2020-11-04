library(rugarch)
library(rmgarch)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)
library(quantmod)

apple = getSymbols("AAPL", from = "2002-01-01", to = Sys.Date(), auto.assign = FALSE)
close = apple$AAPL.Close
basicStats(close)
par(mfrow = c(1, 1))
plot(close, type = "l", ylab = "close price", main = "2002-2020 Daily Apple stock price", col = "red")

#display ACF/PACF plots
par(mfrow = c(1, 2))
acf(coredata(close), main = "ACF 2002 - 2020 Daily Apple stock price")
pacf(coredata(close), main = "PACF 2002 - 2020 Daily Apple stock price")

# Daily log returns
apple_log_returns = na.omit(log(close / lag(close, -1)))
par(mfrow = c(1, 1))
plot(apple_log_returns, type = "l", ylab = "log price", main = "2002-2020 Daily Apple log return", col = "red")
basicStats(apple_log_returns)

#display QQ plot
qqnorm(apple_log_returns, xlab = "", ylab = "")
qqline(apple_log_returns, col = 2)

#display histogram and empirical distribution of daily log returns
par(mfrow = c(1, 2))
hist(apple_log_returns, prob = TRUE, main = "Daily log return histogram", xlab = "Daily returns")
xFit = seq(from = -0.3, to = 0.2, length = 40)
yFit = dnorm(xFit, mean = mean(apple_log_returns), sd = sd(apple_log_returns))
lines(xFit, yFit, col = "red", lwd = 3)
plot(density(apple_log_returns), main = 'Log return empirical distribution')
lines(xFit, yFit, col = "red", lwd = 3)

#display ACF/PACF plots
par(mfrow = c(2, 2))
acf(apple_log_returns, main = "ACF 2002 - 2020 Daily Apple log return")
pacf(apple_log_returns, main = "PACF 2002 - 2020 Daily Apple log return")
acf(apple_log_returns^2, main = "ACF 2002 - 2020 Squared Daily Apple log return")
pacf(apple_log_returns^2, main = "PACF 2002 - 2020 Squared Daily Apple log return")

#GARCH modeling with the rugarch package . The following code specifies a simple GARCH(1,1) model
garch11.spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))

# fit this model to our data
aapl.garch11.fit = ugarchfit(spec = garch11.spec, data = apple_log_returns)

#parameter estimations
coef(aapl.garch11.fit)

#diagnostic tests
show(aapl.garch11.fit)

#leverage effect, or asymmetric effects in volatility
ni.garch11 = newsimpact(aapl.garch11.fit)
par(mfrow = c(1, 1))
plot(ni.garch11$zx, ni.garch11$zy, type = "l", lwd = 2, col = "blue", main = "GARCH(1,1)-News Impact", ylab = ni.garch11$yexpr, xlab = ni.garch11$xexpr)

#EGARCH model for capturing the leverage effect
egarch11.spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))

#fit the EGARCH model to our data.
aapl.egarch11.fit = ugarchfit(spec = egarch11.spec, data = apple_log_returns)

#parameter estimations
coef(aapl.egarch11.fit)

#diagnostic tests
show(aapl.egarch11.fit)

#leverage effect, or asymmetric effects in volatility
ni.egarch11 = newsimpact(aapl.egarch11.fit)
plot(ni.egarch11$zx, ni.egarch11$zy, type = "l", lwd = 2, col = "red", main = "EGARCH(1,1)-News Impact", ylab = ni.egarch11$yexpr, xlab = ni.egarch11$xexpr)

# Simulation and forcasting using the EGARCH model
aapl.egarch11.fit = ugarchfit(spec = egarch11.spec, data = apple_log_returns, out.sample = 20)
aapl.egarch11.fcst = ugarchforecast(aapl.egarch11.fit, n.ahead = 10, n.roll = 10)

plot(aapl.egarch11.fcst, which = 'all')

#forecast
aapl.egarch11.fcst