library(quantmod)
library(TTR)
library(R6)
library(PerformanceAnalytics)

# begin region: helper class and function definitions

StrategySummarizer <- R6Class(
  "StrategySummarizer",
  c(
    strategy_name = "character",
    tickerData = "xts",
    signals = "xts",
    returns = "xts",
    trade_returns = "xts",

    initialize = function(strategy_name, tickerData, signals) {
      self$strategy_name <- strategy_name
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
        "Name",
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

      data <- c(
        self$strategy_name,
        annual_returns,
        max_drawdown,
        annual_std_dev,
        val_at_risk,
        annual_sharpe
      )

      return(data)
    },
    plot = function() {
      names(self$trade_returns) <- self$strategy_name
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
        # only round the input if it's a number
        rounded_numbers <- mapply(function(i) {
          as_number <- as.numeric(i)
          if (is.na(as_number))
            return(i)
          else
            return(round(as_number, 4))
        },
                                  strategy$get_summarized_data())

        output <- paste(output,
                        "\n",
                        toString(rounded_numbers),
                        sep = "")
      }

      return(output)
    }
  )
)

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

# end region: helper class and function definitions

# begin region: strategy function definition

volume_price_trend_indicator <- function(ticker_data) {
  ticker_data$price_increase <- rep(NA, times = nrow(ticker_data))

  for (i in 2:nrow(ticker_data)) {
    previous_data <- as.numeric(Cl(ticker_data)[i - 1])
    current_data <- as.numeric(Cl(ticker_data)[i])

    if (previous_data < current_data) {
      ticker_data$price_increase[i] <- 1
    } else if (previous_data > current_data) {
      ticker_data$price_increase[i] <- -1
    } else {
      ticker_data$price_increase[i] <- 0
    }
  }

  ticker_data$volume_price_trend <-
    rep(NA, times = nrow(ticker_data))
  ticker_data$volume_price_trend[1] <- 1

  for (i in 2:nrow(ticker_data)) {
    previous_data <- ticker_data[i - 1]
    current_data <- ticker_data[i]

    previous_close <- as.numeric(Cl(previous_data))
    current_close <- as.numeric(Cl(current_data))

    if (abs(as.numeric(current_data$price_increase)) == 1) {
      percent_close_change <-
        (current_close - previous_close) / previous_close
      ticker_data$volume_price_trend[i] <-
        percent_close_change *
          previous_close +
          previous_data$volume_price_trend
    } else {
      ticker_data$volume_price_trend[i] <-
        previous_data$volume_price_trend
    }
  }

  return(ticker_data$volume_price_trend)
}

get_slope_of_points_new <- function(points) {
  y_first <- as.numeric(points[1])
  y_last <- as.numeric(points[length(points)])
  y <- c(y_first, y_last)
  x <- 1:2

  reg <- lm(y ~ x)

  slope <- reg$coefficients[2]

  return(slope)
}

get_slope_of_points <- function(points) {
  y <- as.numeric(points)
  x <- 1:length(y)

  reg <- lm(y ~ x)

  slope <- reg$coefficients[2]

  return(slope)
}

adx_above_n_and_positive_slope <- function(in_data, n) {
  adx_all <- ADX(in_data)$ADX
  adx_last <- as.numeric(adx_all[nrow(adx_all)])
  condition <- ((adx_last > n) & get_slope_of_points(in_data) > 0)
  return(condition)
}


# n1 = sma_range
# n2 = rolling_days_range
# n3 = slope_threshold
# sma_range should be < rolling_days_range
# slope_threshold should be > 0
volume_trend_vpt <-
  function(ticker_data,
           sma_range,
           rolling_days_range,
           slope_threshold) {
    # slope of sma(sma_range) of volume_price_indicator
    # of last rolling_days_range >= slope_threshold

    signals <- rep(NA, nrow(ticker_data))

    for (i in rolling_days_range:nrow(ticker_data)) {
      current_data <- ticker_data[(i - rolling_days_range + 1):i]
      vpt_indicator <- volume_price_trend_indicator(current_data)
      sma <- rollmean(vpt_indicator, sma_range)
      slope <- get_slope_of_points(sma)

      signals[i] <- slope >= slope_threshold
    }

    return(signals)
  }

get_slope_of_sma_n <- function(ticker_data, n) {
  sma <- rollmean(ticker_data, n)
  slope <- get_slope_of_points(sma)
  return(slope)
}

volume_trend_strategy_1 <-
  function(ticker_data, n1, n2, n3, n4, n5) {
    vpt_booleans <- volume_trend_vpt(ticker_data, n1, n2, n3)

    # default signal is 0, i.e. do not buy/sell
    ticker_data$signals <- rep(0, nrow(ticker_data))

    for (i in n2:nrow(ticker_data)) {
      if (vpt_booleans[i]) {
        current_data <- ticker_data[(i - n2 + 1):i]

        # only if both signals pass, perform buy / sell
        if (!adx_above_n_and_positive_slope(current_data, n4)) {
          next
        }

        slope <- get_slope_of_sma_n(current_data, n5)

        # if slope is positive, buy, else sell
        ticker_data$signals[i] <- ifelse(slope >= 0, 1, -1)
      }
    }

    return(ticker_data$signals)
  }

# end region: strategy function definition

# begin region: execution of strategy

start_date <- "2015-01-01"
end_date <- "2019-12-31"

strategy <- function(ticker_data) {
  return(volume_trend_strategy_1(ticker_data, 10, 50, .1, 35, 5))
}

spy <- getTicker("SPY", start_date, end_date)
spy_signals <- strategy(spy)
spy_strategy <- StrategySummarizer$new("volume & trend on SPY",
                                       Cl(spy),
                                       spy_signals)

aapl <- getTicker("AAPL", start_date, end_date)
aapl_signals <- strategy(aapl)
aapl_strategy <- StrategySummarizer$new("volume & trend on AAPL",
                                        Cl(aapl),
                                        aapl_signals)

# Kohl's Corporation
kss <- getTicker("KSS", start_date, end_date)
kss_signals <- strategy(kss)
kss_strategy <- StrategySummarizer$new("volume & trend on KSS",
                                       Cl(kss),
                                       kss_signals)

strategies <- c(spy_strategy,
                aapl_strategy,
                kss_strategy)

headers <- aapl_strategy$get_headers()

all_strategies <- MultipleStrategyCSVSummarizer$new(strategies, headers)

all_strategies$summarize()

# plot the strategy performance charts
for (strategy in strategies) {
  strategy$plot()
}


# end region: execution of strategy
