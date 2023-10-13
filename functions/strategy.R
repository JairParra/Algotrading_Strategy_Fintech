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


# Function to calculate geometric mean of returns
f_geometric_mean_returns <- function(returns) {
  n <- length(returns)  # Number of returns
  product <- prod(1 + returns)  # Product of (1 + each return)
  
  # Calculate geometric mean
  geom_mean <- (product)^(1 / n) - 1
  
  return(geom_mean)
}

