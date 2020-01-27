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

# based off https://www.r-bloggers.com/volume-by-price-charts-using-r/
volume_by_price <- function(ticker_data) {
  #Add Positive and Negative Volumes
  ticker_data$positive_VbP <-
    Vo(ticker_data[which(Lag(Cl(ticker_data)) <=
                           Cl(ticker_data))])
  
  ticker_data$negative_vbP <-
    Vo(ticker_data[which(Lag(Cl(ticker_data)) >
                           Cl(ticker_data))])
  
  # Since NAs got generated, replace NAs with 0
  ticker_data[is.na(ticker_data)] <- 0
  
  # Divisor for security
  div <- 1
  
  #Identify High and low of Series, as mutiple of div
  high <- as.integer(ceiling(max(Cl(ticker_data)) / div) * div)
  low <- as.integer(floor(min(Cl(ticker_data)) / div) * div)
  
  # Breaks of interval divisor
  breaks <- as.integer(seq(low, high, by = div))
  
  # Identify and assign price intervals
  ticker_data$t <-
    breaks[findInterval(ticker_data$close, breaks, all.inside = T)]
  
  # Add Positive / Negative Volumes to block
  volsP <- unlist(lapply(breaks, function(x) {
    sum(ticker_data$positive_VbP[ticker_data$t == x])
  }))
  volsN <- unlist(lapply(breaks, function(x) {
    sum(ticker_data$negative_vbP[ticker_data$t == x])
  }))
  
  #Bind the Positive and Negative Volumes
  vols <- rbind(volsP, volsN)
  colnames(vols) <- breaks
  
  return(vols)
}

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

volume_price_trend_indicator <- function(ticker_data) {
  
  ticker_data$price_increase <- rep(NA, times=nrow(ticker_data))
  
  for (i in 2:nrow(ticker_data)) {
    previous_data <- as.numeric( Cl(ticker_data)[i-1] )
    current_data <- as.numeric( Cl(ticker_data)[i] )
    
    if (previous_data <  current_data) {
      ticker_data$price_increase[i] <- 1
    } else if (previous_data > current_data) {
      ticker_data$price_increase[i] <- -1
    } else {
      ticker_data$price_increase[i] <- 0
    }
  }
  
  ticker_data$volume_price_trend <- rep(NA, times=nrow(ticker_data))
  ticker_data$volume_price_trend[1] <- 1
  
  for (i in 2:nrow(ticker_data)) {
    previous_data <- ticker_data[i-1]
    current_data <- ticker_data[i]
    
    previous_close <- as.numeric(Cl(previous_data))
    current_close <- as.numeric(Cl(current_data))
    
    if (abs(as.numeric(current_data$price_increase)) == 1) {
      percent_close_change <- (current_close - previous_close) / previous_close
      ticker_data$volume_price_trend[i] <- 
        percent_close_change * 
        previous_close +
        previous_data$volume_price_trend
    } else {
      ticker_data$volume_price_trend[i] <- previous_data$volume_price_trend
    }
  }
  
  return(ticker_data$volume_price_trend)
}

spy <- getTicker("SPY", "2010-01-01", "2019-12-31")
start_date <- index(spy)[1]+100
spy_window <- spy[start_date <= index(spy) &
                    index(spy) < (start_date + 50)]
vbp <-
  volume_by_price(spy_window)

ticker_data <- Cl(spy_window)

# start vbp plot function
plot(ticker_data,
     yaxt="n", 
     ylab="",
     xlab="Time",
     ylim=c(min(as.integer(colnames(vbp))),
            max(as.integer(colnames(vbp)))),
     main=paste("Close: Volume by Price"),
     sub="Market Analyzer http://mypapertrades.blogspot.com/")

par(new=T)

barplot(height=vbp, 
        beside=F,
        horiz=T, 
        col=c(rgb(0,1,0,alpha=.3),
              rgb(1,0,0,alpha=.3)),
        xlim=c(0,max(vbp[1,]+vbp[2,])*1.1), 
        space=10, 
        width=3,
        xaxt="n",
        yaxt="n",
        las=2)

par(new=F)
# end plot vbp function

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
