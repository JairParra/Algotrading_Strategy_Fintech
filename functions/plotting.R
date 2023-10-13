################################################################################
# portfolio_optimization.R
#   This script contains functions related to visualization 
#
# @author: Hair Parra
################################################################################

##########################
### 1. Data Formatting ###
##########################

f_prepare_data <- function(portfolio){
  ## Prepares data in a correct format for plotting 
  
  # pack the results into a dataframe 
  portf_df <- as.data.frame(cbind(portfolio$dates,
                                  portfolio$capital_history, 
                                  portfolio$returns
  ))
  
  portf_df <- na.trim(portf_df)
  colnames(portf_df) <- c("date", "capital", "discrete_returns")
  
  # convert to xts 
  portf_xts <- as.xts(portf_df, order.by = as.Date(portf_df$date))
  portf_xts$date <- NULL
  
  # add the log returns 
  portf_xts$discrete_returns <- as.numeric(portf_xts$discrete_returns)
  portf_xts$log_returns <- log(as.numeric(portf_xts$discrete_returns) + 1)
  
  # Download weekly prices of Apple spanning
  SP500 <- getSymbols(Symbols = "^GSPC", # symbol
                      src = "yahoo", # source: yahoo finance
                      from = "2016-01-01",
                      to = "2023-01-01",
                      periodicity = "daily", 
                      auto.assign = FALSE # prevents overwriting existing objects in env 
  )
  
  # choose only valid dates  and assign to portfolio 
  SP500 <- SP500[index(portf_xts)]
  portf_xts$sp500_discrete_rets <- Return.calculate(prices = SP500[, 6], method = "discrete")
  portf_xts$sp500_log_rets <- Return.calculate(prices = SP500[, 6], method = "log")
  portf_xts <- na.fill(portf_xts, 0)
  
  return(portf_xts)
}


f_prepare_history_data <- function(portfolio){
  # portfolio contains a list for each datapoint 
  
}

#############################
### 2. Plotting functions ###
#############################

f_plot_performance <- function(portf_xts){
  
  # Visualize portfolio performance using PerformanceSummary chart
  ret_cols <- c("discrete_returns", "log_returns", "sp500_discrete_rets", "sp500_log_rets")
  charts.PerformanceSummary(R = portf_xts[, ret_cols],
                            wealth.index = TRUE,
                            main = "MLR-RF-Sharpe Min-Var Portfolio Performance")
}


f_plot_drawdown <- function(portf_xts){
  # Visualize Portfolio Drawdown
  chart.Drawdown(
    R = portf_xts[, c(2,4)],
    geometric = TRUE,
    legend.loc = "topleft",
    colorset = (1:12),
    plot.engine = "default",
    main = "MLR-RF-Sharpe Min-Var Portfolio Drawdown"
  )
}


f_plot_capital <- function(portf_xts){
  # Visualize capital evolution through time
  plot(portf_xts$capital,
       yaxis.right = FALSE,
       legend.loc = "topleft", # legend on the top left of the chart
       main = "Portfolio Capital Evolution",
       xlab = "Date",
       ylab = "Capital",
       col = "blue",
       major.ticks = "weeks",
       grid.ticks.on = "weeks",
       grid.ticks.lty = 2)
}

f_plot_sharpe <- function(portfolio){
  # obtain the historical sharpe values 
  portfolio_dates <- portfolio$dates
  sharpe_history <- xts(portfolio$sharpe_history, order.by = as.Date(portfolio_dates))

  # Visualize capital evolution through time
  plot(sharpe_history,
       yaxis.right = FALSE,
       legend.loc = "topleft", # legend on the top left of the chart
       main = "Portfolio Sharpe Ration Evolution",
       xlab = "Date",
       ylab = "Sharpe",
       col = "blue",
       major.ticks = "weeks",
       grid.ticks.on = "weeks",
       grid.ticks.lty = 2)
}
