################################################################################
# strategy.R
#   This script contains broader blocks of the simulation and backtesting logic.
#
# @author: Hair Parra
################################################################################

################
### 0. Setup ###
################

# load required scripts
library("here") 
source(here("functions", "modelling.R")) # modelling procedure


################
### 1. Utils ###
################

f_read_stock_price <- function(ticker, sp500_stocks_flat, date = NULL){
  
  ## Retrieves the selected price of the ticker in question 
  price <- sp500_stocks_flat[[ticker]][index(sp500_stocks_flat[[ticker]]) == date]
  price <- as.numeric(price$adjusted_close)
  
  return(price)
}


###############################
### 2. Portfolio Management ###
###############################

f_CLOSE_positions <- function(portfolio, tau){
  ## Closing the positions in the portfolio means: 
  ## 
  ## 0. Calculate number of (fractional) shares per ticker: num_shares = weight * capital 
  ## 1. Sell each ticker and gain CF += num_shares * cur_price 
  ## 2. Clear all cur tickers and new portfolio value is cur_capital + sum(CF_tickers) 
  
  # First run only
  if(tau == 1){
    # return unchanged portfolio 
    return(portfolio)
  }
  
  # Etract tickers 
  
}

f_LONG_positions <- function(portfolio, tau){
  ## Longing all positions in the portfolio means:
  ## 
  ## 1. Retrieve updated weights from portfolio 
  ## 2. Calculate weighted (fractional) num_shares per ticker: num_shares = ticker * weight * capital  
  ## 3. Clear all tickers and new portfolio value is capital_expended 
  
}


###########################
### 2. SECTOR PROCEDURE ###
###########################

f_SECTOR_PROCEDURE <- function(G, tau, sp500_stocks, max_top_pick, model_type = "elasticnet", verbose=FALSE){
  ## This function is simply a wrapper for the MOELLING PROCEDURE of our simulation. 
  ## 
  ##  Params: 
  ##    - G (str): sector name 
  ##    - tau (int): integer > 0 which represents the current run in the backtest
  ##    - sp500_stocks (named list): List of industries and each industry contains stock data 
  ##                                 for each of those sectors. 
  ##    - max_top_pick (int): positive integer indicating the number of best stocks to pick 
  ##    - verbose (logical): if TRUE, show obtained formula and metrics at each iteration 
  
  top_sector_stocks <- f_MODELLING_PROCEDURE(G, 
                                             tau,
                                             sp500_stocks,
                                             model_type = model_type, 
                                             best_n = max_top_pick,
                                             verbose=verbose)
  
  
  return(top_sector_stocks)
}


################################
### 3. BACKTESTING PROCEDURE ###
################################

f_BACKTESTING_PROCEDURE <- function(sp500_stocks, N_window, N_runs, portfolio, verbose){ 
  
  # Initiate backtesting 
  if(verbose){
    print(paste(rep("-", 100), collapse = ""))
    print("BACKTESTING")
    print(paste(rep("-", 100), collapse = ""))
    print("")
  }
  
  # for every run (sliding window of time to consider)  
  for(tau in seq(N_runs)){
    
    # close any positions 
    if(verbose){
      print("###############")
      print(paste0("### (tau=", tau, ") ###"))
      print("###############")
      print("CLOSE all positions") 
    }

    
    
  }
  
}


