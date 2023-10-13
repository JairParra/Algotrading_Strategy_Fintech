################################################################################
# strategy.R
#   This script contains helper blocks for the simulation.
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
  
  # sometimes the stock didn't exist? 
  if(length(price) == 0){
    return(NULL)
  }
  return(price)
}
################################
### 3. BACKTESTING PROCEDURE ###
################################

f_BACKTESTING_PROCEDURE <- function(sp500_stocks, N_window, N_runs, portfolio, verbose){ 
  
  ## TODO 
  
  # Initiate backtesting 
  if(verbose){
    print(paste(rep("-", 100), collapse = ""))
    print("BACKTESTING")
    print(paste(rep("-", 100), collapse = ""))
    print("")
  }
  
}


