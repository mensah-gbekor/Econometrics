library(xlsx)
library(dplyr)
library(tidyr)
library(plotly)
library(tseries)
library(vars)
library(timeSeries)
library(tsDyn)

# Data used for model:
#   
# Consumer price index in the United Kingdom 
#   https://fred.stlouisfed.org/series/CPIUKQ
# 
# U.S./U.K. Foreign Exchange Rate in the United Kingdom 
#   https://fred.stlouisfed.org/series/USUKFXUKQ
# 
# Consumer Price Index: Total, All Items for the United States
#   https://fred.stlouisfed.org/series/CPALCY01USQ661N
# 
# Interest Rates and Price Indexes; Effective Federal Funds Rate (Percent), Level
#   https://fred.stlouisfed.org/series/BOGZ1FL072052006Q
# 
# Bank of England Policy Rate in the United Kingdom
#   https://fred.stlouisfed.org/series/BOERUKQ

# limit the data to the window available across all time series
# which represents modern history 
StartDate <- as.Date("1980-01-01")
EndDate <- as.Date("2016-10-01")

# function to read data from xls file and prepare it a bit
readE <- function(file, skip = 11, start = StartDate, ...) {
  readxl::read_excel(file, 
                     col_types = c("date","numeric"), 
                     col_names = c("date", "rate"),
                     skip = skip, 
                     ...) %>%
    tbl_df() %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= StartDate) %>%
    filter(date <= EndDate)
}

# a utility function to plot the rates
chartR <- function(df, ...) {
  plot_ly(df, x = ~date, y = ~rate, type="scatter", mode="lines+markers", ...)
}

# Bank of England Rate
ukRates <- readE("BOERUKQ.xls", 831) 

# Federal Funds Rate
usaRates <- readE("BOGZ1FL072052006Q.xls", 11)

# UK CPI
ukCPI <- readE("CPIUKQ.xls")

# US CPI
usCPI <- readE("CPALCY01USQ661N.xls")

# GBP USD rate
gbpusd <- readE("USUKFXUKQ.xls", 447)

chartR(ukRates)
chartR(usaRates)
chartR(ukCPI)
chartR(usCPI)
chartR(gbpusd)

plotACF <- function(ts, name) {
  par(mfrow = c(2, 1))
  
  acf(ts$rate, main = name)
  acf(diff(ts$rate), main = paste0("diff(",name,")"))
}

# Checking for unit root in all series
# based on Augmented Dickey-Fuller test all series are I(1)

plotACF(ukRates, "ukRates")
adf.test(ukRates$rate)       # Dickey-Fuller = -3.2322, Lag order = 5, p-value = 0.08525
adf.test(diff(ukRates$rate)) # Dickey-Fuller = -4.4717, Lag order = 5, p-value = 0.01

plotACF(usaRates, "usaRates")
adf.test(usaRates$rate, k = 12) # Dickey-Fuller = -3.212, Lag order = 12, p-value = 0.08863
adf.test(diff(usaRates$rate))   # Dickey-Fuller = -4.2534, Lag order = 5, p-value = 0.01

plotACF(ukCPI, "ukCPI")
adf.test(ukCPI$rate)       # Dickey-Fuller = -2.094, Lag order = 5, p-value = 0.5373
adf.test(diff(ukCPI$rate)) # Dickey-Fuller = -2.9765, Lag order = 5, p-value = 0.1697

plotACF(usCPI, "usCPI")
adf.test(usCPI$rate, k = 12)  # Dickey-Fuller = -2.8811, Lag order = 12, p-value = 0.2094
adf.test(diff(usCPI$rate))    # Dickey-Fuller = -4.2236, Lag order = 5, p-value = 0.01

plotACF(gbpusd, "gbpusd")  
adf.test(gbpusd$rate)       # Dickey-Fuller = -3.2385, Lag order = 5, p-value = 0.08421
adf.test(diff(gbpusd$rate)) # Dickey-Fuller = -4.3055, Lag order = 5, p-value = 0.01


# all of the processes are non-stationary, we should explore co-integration 

rates <- cbind(gbpusd$rate,ukCPI$rate,usCPI$rate,ukRates$rate,usaRates$rate)
colnames(rates) <- c("gbpusd", "ukcpi", "uscpi", "ukrate", "usrate")

# timeseries are strongly correlated
cor(rates)
# > cor(rates)
# gbpusd      ukcpi      uscpi     ukrate     usrate
# gbpusd  1.0000000 -0.2125212 -0.1533825  0.3408192  0.3559606
# ukcpi  -0.2125212  1.0000000  0.9918185 -0.9189104 -0.8841374
# uscpi  -0.1533825  0.9918185  1.0000000 -0.9111132 -0.8671667
# ukrate  0.3408192 -0.9189104 -0.9111132  1.0000000  0.8691303
# usrate  0.3559606 -0.8841374 -0.8671667  0.8691303  1.0000000


# unrestricted VAR model
# based on number of significant parameters it is possible to choose 
# the lag value
VAR_model <- VAR(rates, type = "none", lag.max = 12, ic = "AIC")
summary(VAR_model)


# Johansen test for co-integration
# shows two co-integration relationships
#
jotest1=ca.jo(rates, type="eigen", K = 12, ecdet="none", spec="longrun")
summary(jotest1)
# Test type: maximal eigenvalue statistic (lambda max) , with linear trend 
# 
# Eigenvalues (lambda):
#   [1] 0.387390555 0.236806448 0.129737301 0.061304398 0.006481249
# 
# Values of teststatistic and critical values of test:
#           test 10pct  5pct  1pct
# r <= 4 |  0.88  6.50  8.18 11.65
# r <= 3 |  8.60 12.91 14.90 19.19
# r <= 2 | 18.90 18.90 21.07 25.75
# r <= 1 | 36.75 24.78 27.14 32.14
# r = 0  | 66.64 30.84 33.32 38.78
# 

jotest2=ca.jo(rates, type="trace", K = 12, ecdet="none", spec="longrun")
summary(jotest2)
# Test type: trace statistic , with linear trend 
# 
# Eigenvalues (lambda):
#   [1] 0.387390555 0.236806448 0.129737301 0.061304398 0.006481249
# 
# Values of teststatistic and critical values of test:
#   
#   test 10pct  5pct  1pct
# r <= 4 |   0.88  6.50  8.18 11.65
# r <= 3 |   9.49 15.66 17.95 23.52
# r <= 2 |  28.39 28.71 31.52 37.22
# r <= 1 |  65.14 45.23 48.28 55.43
# r = 0  | 131.78 66.49 70.60 78.87

# we FAIL to reject hypothesis r <= 2 (at most two co-integration relationships),
# accepting the alternative of rank = 2 (exactly two co-integration relationships)
# there are 5 - 2 = 3 common trends in the data


# Fit cointegrated VECM
VECM_fit = VECM(rates, 10, r = 2, include = "const", estim = "ML", LRinclude = "none")
summary(VECM_fit)
#############
###Model VECM 
#############
# Full sample size: 148 	End sample size: 137
# Number of variables: 5 	Number of estimated slope parameters 265
# AIC -1589.288 	BIC -797.9733 	SSR 75.25595
# Cointegrating vector (estimated by ML):
#   gbpusd        ukcpi      uscpi     ukrate     usrate
# r1      1 1.626303e-19 0.01500144 0.05607569 0.03017534
# r2      0 1.000000e+00 0.87820216 4.36052670 5.83292473

# assessing the equilibrium exchange rate using eigen vector
# where the first coefficient is normalized (= 1.0)
#
# (1 1.626303e-19 0.01500144 0.05607569 0.03017534)

eigen = list(1, 1.626303e-19, 0.01500144, 0.05607569, 0.03017534)
names(eigen) <- c("gbpusd", "ukcpi", "uscpi", "ukrate", "usrate")

# this is the main linear part from equation of the model that represents 
# the equilibrium value of gbp_usd
equilib_gbpusd = ukCPI$rate * eigen$ukcpi  + 
                  eigen$uscpi * usCPI$rate +
                  eigen$ukrate * ukRates$rate +
                  eigen$usrate * usaRates$rate

# data frame with equilibrium eurusd rate and actual historical values
final_df <- tibble(date = gbpusd$date, gbpusd = gbpusd$rate, eq_gbpusd = equilib_gbpusd)

# plotting equilibrium GBPUSD vs actual GBPUSD
plot_ly(final_df,type = "scatter", mode = "line + markers") %>%
  add_trace( x = ~date, y = ~gbpusd, name = "gbpusd" ) %>%
  add_trace( x = ~date, y = ~eq_gbpusd, name = "equilibrium gbpusd") 
                

