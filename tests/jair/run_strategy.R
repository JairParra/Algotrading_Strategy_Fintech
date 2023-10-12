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
# 
# Simplifications & Assumptions: 
# 1. No short-sales
# 2. No leverage 
# 3. Fractional sell allowed for all shares 
# 4. No transaction costs 
################################################################################

####################
### 0. Libraries ###
####################

# load required scripts
library("xts")
library("here") 
source(here("functions", "fetch_sp500_sectors.R")) # functions for top stocks and economic sectors in the sp500
source(here("functions", "feature_engineering.R")) # functions for feat eng and manipulation
source(here("functions", "modelling.R")) # modelling procedure functions

#######################
### 1. Data Loading ###
#######################

# Load preprocessed data from the data_clean directory 
load(here("data_clean", "sp500_stocks.rda"))

################################
### 2. Simulation Parameters ###
################################

# Set up backtesting simulation parameters
sample_xts <- sp500_stocks[[1]][[1]] # sample stock from data 
sectors <- names(sp500_stocks)
N_sector_best_stocks <- 3 # new strategy: max(3x2 = 6) 

# Formula parameters
slide <- 1 # moving one month at the time
N_months <- length(names(split.xts(sample_xts, f= "months"))) # total number of months 
N_window <- 24 # number of months in size for each window 
N_runs <- floor((N_months - N_window)/slide) # total number of runs/taus

# setup initial portfolio tracking variables 
initial_capital <- 500000
num_tickers <- length(sectors)*N_sector_best_stocks*2 # two sub-strategies for picking
initial_tickers <- rep(NA, num_tickers)
weights <- rep(1/num_tickers, num_tickers) # initialize to 1/n
returns <- rep(NA, N_runs) # returns for the portfolio across time 
num_shares <- rep(NA, length(weights))

# # repack the portfolio for tracking
# portfolio <- list(tickers = initial_tickers, # old version
#                   weights = weights,
#                   num_shares = num_shares,
#                   capital = initial_capital,
#                   returns = returns
#                   )


# repack the portfolio for tracking 
# Note: 
# assets = list of tickers
#       |-> ticker = list of (weight, num_shares) 
portfolio <- list(assets = NA, 
                  capital = initial_capital, 
                  returns = returns
)

# display parameters
print(paste0("Running backtesting with the following parameters: "))
print(paste0("- N_months: ", N_months))
print(paste0("- N_runs: ", N_runs))
print(paste0("- slide: ", slide))
print(paste0("- Initial Capital: ", initial_capital))


###################################
### X. MODELLING PROCEDURE TEST ###
###################################

# parameters 
G <- names(sp500_stocks)[2] # sample sector 
tau <- 10 # suppose we are in run 5 of the backtest 


system.time({
  # run simulation for one sector with verbose 
  best_sector_stocks <- f_MODELLING_PROCEDURE(G, tau, sp500_stocks, best_n = 5, verbose=TRUE)
  
  # pack the data into a format for modelling (only keep the data)
  top_sector_stocks <- lapply(best_sector_stocks, function(x)x$data) 
})



save(top_sector_stocks, file = here("tests", "jair", "top_sector_stocks.rda"))




