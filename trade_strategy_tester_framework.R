library(quantmod)
library(TTR)
library(R6)
library(PerformanceAnalytics)

# strategy_func is a function that receives data as xts object
# and returns xts object of signals
calculate_strategy <- function(strategy_func, ticker_data) {
  signals <- strategy_func(ticker_data)
  return(signals)
}

StrategySummarizer <- R6Class(
  "StrategySummarizer",
  c(
    tickerData = "xts",
    signals = "xts",
    returns = "xts",
    trade_returns = "xts",
    
    initialize = function(tickerData, signals) {
      self$tickerData <- tickerData
      self$signals <- signals
      
      self$returns <-
        Delt(self$tickerData)
      self$returns[1] <- 0
      self$trade_returns <-
        self$returns * lag(self$signals)
    },
    get_headers = function() {
      headers <- c(
        "Annual Return",
        "Max Drawdown",
        "Annual Std Deviation",
        "Value At Risk at 95% Confidence",
        "Annual Sharpe Ratio"
      )
      return(headers)
    },
    get_summarized_data = function() {
      #cum_returns <- Return.cumulative(self$trade_returns)
      annual_returns <- Return.annualized(self$trade_returns)
      max_drawdown <- maxDrawdown(self$trade_returns)
      #daily_std_dev <- StdDev(self$trade_returns)
      annual_std_dev <- StdDev.annualized(self$trade_returns)
      val_at_risk <- VaR(self$trade_returns)
      annual_sharpe <- SharpeRatio.annualized(self$trade_returns)
      
      data <- c(annual_returns,
                max_drawdown,
                annual_std_dev,
                val_at_risk,
                annual_sharpe)
      
      return(data)
    },
    plot = function() {
      charts.PerformanceSummary(self$trade_returns)
    }
  )
)

MultipleStrategyCSVSummarizer <- R6Class(
  "MultipleStrategyCSVSummarizer",
  c(
    strategies = "NA",
    headers = "NA",
    initialize = function(strategies, headers) {
      self$strategies <- strategies
      self$headers <- headers
    },
    summarize = function() {
      output <- toString(self$headers)
      
      for (strategy in self$strategies) {
        output <- paste(output,
                        "\n",
                        toString(strategy$get_summarized_data()),
                        sep = "")
      }
      
      return(output)
    }
  )
)

volume_momentum_strategy_1 <- function(ticker_data) {
  ticker_data$volume
  
  signal
  
  return(signal)
}

momentum_strategy_1 <- function(ticker_data) {
  macd <- MACD(
    ticker_data,
    nFast = 12,
    nSlow = 26,
    nSig = 9,
    maType = "SMA",
    percent = F
  )
  
  bb <-
    BBands(ticker_data,
           n = 20,
           maType = "SMA",
           sd = 2)
  
  signal <-
    ifelse(
      ticker_data > bb$up &
        macd$macd > macd$signal,
      1,
      ifelse(ticker_data < bb$dn &
               macd$macd < macd$signal, -1,
             0)
    )
  
  signal <- na.omit(signal)
  
  return(signal)
}

getTicker <- function(symbol, start, end) {
  data <-
    getSymbols(symbol,
               src = "yahoo",
               auto.assign = F)
  
  data <- window(data,
                 start = start,
                 end = end)
  
  # forward fill NA
  data <- na.locf(data)
  
  names(data) <-
    c("open", "high", "low", "close", "volume", "adjusted")
  
  return(data)
}

spy <- getTicker("SPY", "2010-01-01", "2019-12-31")

spy_in_sample <- window(spy,
                        start = "2010-01-01",
                        end = "2017-12-31")

spy_out_sample <- window(spy,
                         start = "2018-01-01",
                         end = "2019-12-31")

spy_in_sample$signals <- momentum_strategy_1(spy_in_sample$close)
spy_out_sample$signals <- momentum_strategy_1(spy_out_sample$close)

strat_in_sample <-
  StrategySummarizer$new(spy$close, spy_in_sample$signals)
strat_out_sample <-
  StrategySummarizer$new(spy$close, spy_out_sample$signals)


multiStratCsvSummarizer <-
  MultipleStrategyCSVSummarizer$new(c(strat_in_sample, strat_out_sample),
                                    strat_in_sample$get_headers())

multiStratCsvSummarizer$summarize()
