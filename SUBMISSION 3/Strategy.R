library(BatchGetSymbols)
library(quantmod)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate) 
library(PerformanceAnalytics)
library(futile.logger)

# 
# momentum trading strategy applied to Gold daily data
#
# this is an application of MACD & Bolinger bands from the course, 
# with aim to experiment and research into optimization of the strategy 
# in the parameters space (finding the optimum parameters on the training set)
# and see if their predictive power is kept on the testing set
#

#
# since it is a momentum strategy, the gold was chosen as an instrument 
# that historically demonstrated clear trends on various time scales
#

# date parameters
first.date <- as.Date('2000-01-01')
last.date <- Sys.Date()
freq.data <- 'daily'
tickers <- c('GC=F') # Gold

#
# retrieving gold data from yahoo finance
#
raw.data <- BatchGetSymbols(
    tickers = tickers, 
    first.date = first.date,
    last.date = last.date, 
    freq.data = freq.data,
    cache.folder = file.path(tempdir(), 'batchgetsymbols.cache.tmp')
) 

#
# tidying the dataset, selecting the columns 
# that are going to be used for analysis
#
df <- raw.data$df.tickers %>% 
        rename(open = price.open,
               high = price.high,
               low = price.low,
               close = price.close,
               date = ref.date) %>%
        select(date, open, high, low, close, volume) %>%
        drop_na() %>%
        mutate(return.val = c(0, diff(close)),
               return.pct = replace_na(Delt(close, k = 1), 0)
               ) %>%
        select(date, close, return.val, return.pct)

#
# dividing the data into the training set and testing set
@

training_start <- as.Date('2010-01-01')
training_end <- as.Date('2017-12-31')
training <- df %>% filter(date >= training_start & date <= training_end) 
testing <- df %>% filter(date > training_end) 

#
# a function that does the backtesting of the strategy 
# given the data and the parameters of MACD and BBands
#
makeStrategy <- function(data_tibble,
                        ma.nFast = 12, ma.nSlow = 26, ma.nSig = 9,
                        bb.n = 20, bb.sd = 2) {
  
    # creating MACD based on the dat and parameters
    macd <- as_tibble(
              MACD(data_tibble$close, 
                   nFast = ma.nFast, 
                   nSlow = ma.nSlow, 
                   nSig = ma.nSig, 
                   maType = "SMA", percent = FALSE)
            ) %>%
            rename(macd.signal = signal)

    # Boulinger bands 
    bb <- as_tibble(
        BBands(data_tibble$close, n = bb.n, maType = "SMA", sd = bb.sd)
      ) %>%
      rename(bb.dn = dn, bb.mavg = mavg, bb.up = up, bb.pctB = pctB)
    
    # 
    # for future improvement of signal involving inner band
    #
    bb15 <- as_tibble(
      BBands(data_tibble$close, n = bb.n, maType = "SMA", sd = bb.sd*0.75)
    ) %>%
      rename(bb15.dn = dn, bb15.mavg = mavg, bb15.up = up, bb15.pctB = pctB)
    
    #
    # combinding the parts together and calculating the signal
    #
    combined <- 
      bind_cols(data_tibble, macd, bb, bb15) %>%
      drop_na() %>%
      mutate(
          #
          # one idea for improving the signal here 
          # is to make use of an inner band that is 3/4 of the width
          # to limit the "out of market" signal only when crossing 
          # from outer band into the inner band,  
          # but turning the trading signal ON only when breaking out of the wider band
          #
          signal = ifelse(
            close > bb.up & macd > macd.signal, 
            1,
            ifelse(close < bb.dn & macd < macd.signal,
                  -1, 
                  0
            )
          )
        ) %>%
        mutate(
          #
          # here we calculate the return of the strategy in absolute and percentage terms
          #
          # as well as other parameters (like maximum drawdown) that are useful for 
          # optimisation of parameters
          #
          t.return.pct = return.pct * replace_na(lag(signal), 0),
          t.return.val = return.val * replace_na(lag(signal), 0),
          t.return.cum = cumsum(t.return.val), 
          t.drawdown = t.return.cum - cummax(t.return.cum),
          t.return.cummax = cummax(t.return.cum),
          t.drawdown.pct = ifelse(t.return.cummax == 0, 
                                  ifelse(t.drawdown == 0, 0, -100),
                                  (t.drawdown/t.return.cummax)*100),
          t.drawdown.cummin = cummin(t.drawdown),
          t.drawdown.pct.cummin = cummin(t.drawdown.pct)
          # ,position.hold = cumsum(signal)
        )
    
    #
    # returning data, fitted indicators/signals, performance metrics and parameters
    #
    list(
      combined = combined,
      param = list(
        ma.nFast = ma.nFast, 
        ma.nSlow = ma.nSlow, 
        ma.nSig = ma.nSig,
        bb.n = bb.n, 
        bb.sd = bb.sd
      )
    )
}

#
# this function uses plotly to create a chart of all elements 
# including the prices, trade points, bolinger bands, MACD, cumulative return
# and drawdown
#

makeChart <- function(result) {
  strategy <- result$combined
  plot_trades <- plot_ly(type = "scatter", mode = "markers") %>% 
    add_trace(name = "out", 
              data = strategy %>% filter(signal == 0),
              x = ~date, y = ~close, 
              marker = list(color = "gray", opacity = 0.75, size = 2)) %>%
    add_trace(name = "long",
              data = strategy %>% filter(signal == 1),
              x = ~date, y = ~close, 
              marker = list(symbol = 'circle-open', color = "green", size = 4)) %>%
    add_trace(name = "sell",
              data = strategy %>% filter(signal == -1),
              x = ~date, y = ~close, 
              marker = list(symbol = 'circle-open', color = "red", size = 4)) %>%
    add_trace(name = "bb up", mode = "lines",
              data = strategy, 
              x = ~date, y = ~bb.up, line = list(color = "blue", width = 1)) %>%
    add_trace(name = "bb down", mode = "lines",
              data = strategy, 
              x = ~date, y = ~bb.dn, line = list(color = "blue", width = 1))

    plot_macd <- plot_ly(
      data = strategy,
      type = "scatter",
      mode = "lines",
      line = list(width = 1)
    ) %>% add_trace(      
        name = "macd",
        fill = "tozeroy",
       # fillcolor = 'rgba(220,220,60,0.5)',
        x = ~date,
        y = ~macd) %>% 
      add_trace(
      name = "macd.signal",
      data = strategy, 
      x = ~date, 
      y = ~macd.signal)

    plot_ret <- plot_ly(type = "scatter", 
                        mode = 'lines', 
                        fill = 'tozeroy') %>%
      add_trace(data = strategy, 
                name = "cumul.return", 
                line = list(width = 2),
                x = ~date, 
                line = list(color = "green", opacity = 0.8, width = 1),
                fillcolor = "rgba(200,200,200,0.5);",
                y = ~t.return.cum) %>%
      add_trace(data = strategy, 
                x = ~date,
                y = ~t.drawdown,
                fill = "tozeroy",
                line = list(color = "red", opacity = 0.8, width = 1),
                fillcolor = "rgba(50,20,20,0.5);",
                name = "drawdown"
                )
    
    subplot(plot_trades, 
            plot_macd,
            plot_ret, 
            nrows = 3, 
            heights = c(0.5, 0.25, 0.25),
            shareX = TRUE)
    
}

#
# calculate sharpe ratio for returns
#
calcSharpe <- function(s.result) {
  st <- if ("data.frame" %in% class(s.result)) s.result else s.result$combined
  SharpeRatio(xts(st$t.return.pct, order.by = st$date))
}

#
# this function searches through the parameters space 
# to maximize returns while keeping the drawdown (both absolute and max percentage)
# within the limits specified in parameters
#
findBest <- function(df,
                     fast.range = c(seq(3,10),seq(12,25,by=3)),
                     slow.range = c(seq(5,10),seq(12,20,by=2), seq(25,50,by=5)),
                     sig.range = c(seq(2,10), seq(12,20,by=2)),
                     bb.range = c(seq(3,10), seq(12,20),seq(25,50,by=5)),
                     sd.range = c(2), 
                     max.drawdown.pct = 45,
                     max.drawdown.abs = 100) {
  
  result <- NULL
  end_pnl <- 0
  for (bbsd in sd.range) {
    for(bbn in bb.range) {
      for(fast in fast.range) {
        for (slow in slow.range) {
          flog.info("bbsd = %f, bbn = %d, fast = %d, slow = %d", bbsd, bbn, fast, slow)
          for (sig in sig.range) {
            if (bbn >= fast && bbn <= slow && slow > fast && sig < fast) {
              str <- makeStrategy(df, 
                                  ma.nFast = fast, 
                                  ma.nSlow = slow,
                                  ma.nSig = sig,
                                  bb.n = bbn,
                                  bb.sd = bbsd)
              endp <- tail(str$combined$t.return.cum, 1);
              enddd <- tail(str$combined$t.drawdown.pct.cummin, 1)
              absdd <- tail(str$combined$t.drawdown.cummin, 1)
              if (endp > end_pnl & 
                  (abs(enddd) < max.drawdown.pct |
                   abs(absdd) < max.drawdown.abs)) {
                end_pnl = endp
                result = str
                flog.info(sprintf("Best endp = %f, fast = %d, slow = %d, sig = %d, sd = %f",
                                  endp, fast, slow, sig, bbsd))
              }
            }
          }
        }
      }
    }
  }
  result
}

#
# plot the chart for training set using default values of parameters 
#
makeChart(makeStrategy((training)))

#
# this is the chart for the testing part of the data with default parametrs of the startegy
#
makeChart(makeStrategy(testing))

#
# searching for the best parameters on training set 
#
best <- findBest(training) # no max dd

#
# the performance of the strategy improved in both returns
# and the drawdowns; however, the bulk of this improvement came from 
# a large drop in price in april 2013
#
makeChart(best)

best$param

makeChart(makeStrategy(testing, 7, 18, 6, 7, 2))
# 7 18 6 7 2
# 6, 20, 4, 7, 2

#
# this is how the parameters which are best for the trainig set
# perform on the testing subset of the data
#
# unfortunately there is not much improvement in the performance on the testibn set
# with these parameters 
#
# I belive it means the global search for best fit on the training set 
# resulted in a fair degree of overfitting
#
makeChart(makeStrategy(testing, 7, 18, 6, 7, 2))

#
# more experiments conducted with various parts of the data shown that
# 1. it is difficult to find univerally good paramters that would deliver good performance 
#     across all years of 2000-2020
#
# 2. it seems relevant to greater increase in volatility somewhere around 2007
#     as parameters that deliver good performance prior 2007 are not working after that and vice versa
#
# 3. from observations of trade points it seems the algoritm holds the position for too short a time frame
#
# 4. there are different variations of MACD/BBands algorithms, 
#    in particular I think the one which uses more than one set of Bolinger bands is 
#     useful, since it offers criteria that should lead to more prolonged position 
#     holding compared to this strategy. 
#







