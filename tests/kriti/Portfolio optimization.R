library("here")
library("quantmod")
library("PerformanceAnalytics")
library("xts")
library("Matrix")
library("quadprog")

# Example of input from main code that will be used for portfolio optimization 
load("top_sector_stocks.rda")
source(here("functions", "portfolio_optimization.R"))

# Get the data frame for returns
returns_fore <- f_extract_ret_fore(top_sector_stocks, "best_shifted_arima")
sum(is.na(returns_fore)) # to check if there are any NA

# Get the data frame for volatility forecast
volatility_fore <- f_extract_vol_fore(top_sector_stocks, "vol_forecast")
sum(is.na(volatility_fore)) # to check if there are any NA

# Portfolio optimization

tau <- 10

# Covariance matrix
cov_matrix <- f_cov_matrix(volatility_fore, tau)

# Minimum returns for portfolio optimization 
mean_returns <- f_mean_ret(returns_fore, tau)

# optimizing portfolio
output <- quadprog::solve.QP(Dmat = cov_matrix, # covariance matrix to be minimized
                             dvec = mean_returns, # minimum returns
                             Amat = cbind(matrix(rep(1,length(mean_returns)), ncol = 1), diag(length(mean_returns))),
                             bvec = c(1, rep(0.1,length(mean_returns))), # sum of weights = 1 & no short selling
                             meq = 1)
# Weights allocated
weights_mv <- output$solution
names(weights_mv) <- names(mean_returns)
weights_mv
sum(weights_mv) # sum of weights = 1

