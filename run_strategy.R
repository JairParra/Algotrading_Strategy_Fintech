################################################################################
# run_strategy.R
# 
# @author: Hair Albeiro Parra Barrera
# @author: Xiao Xue 
# @author: Kriti Bhaya
# @author: Prateek 
# 
# This R script acts as a driver to execute backtesting of the strategy we 
# we implemented. 
################################################################################

####################
### 0. Libraries ###
####################

# load required scripts
library("here") 
source(here("functions", "fetch_sp500_sectors.R")) # functions for top stocks and economic sectors in the sp500
source(here("functions", "feature_engineering.R")) # functions for feat eng and manipulation

#######################
### 1. Data Loading ###
#######################

# Load preprocessed data from the data_clean directory 
load(here("data_clean", "sp500_stocks.rda"))

################################
### 2. Simulation Parameters ###
################################

# Set up backtesting simulation parameters
sample_xts <- sp500_stocks[[1]][[1]] # sammple stock from data 
sectors <- names(sp500_stocks) 
N_sector_best_stocks <- 3 # new strategy: 3x2 = 6 

# Formula parameters
slide <- 1 
N_months <- length(names(split.xts(sample_xts, f= "months")))
N_window <- 18 # number of months in size for each window 
N_runs <- floor((N_months - N_window)/slide)

# setup initial portfolio tracking variables 
initial_capital <- 500000
num_tickers <- length(sectors)*N_sector_best_stocks*2 # two sub-strategies for picking
initial_tickers <- rep(NA, num_tickers)
weights <- rep(1/num_tickers, num_tickers) # initialize to 1/n
returns <- rep(NA, N_runs)

# repack the portfolio for tracking 
portfolio <- list(tickers = initial_tickers, 
                  weights = weights, 
                  capital = initial_capital, 
                  returns = returns, 
                  data = NA
)


# display parameters
print(paste0("Running backtesting with the following parameters: "))
print(paste0("- N_months: ", N_months))
print(paste0("- N_runs: ", N_runs))
print(paste0("- slide: ", slide))
print(paste0("- Initial Capital: ", initial_capital))


