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

# create flattened version for ease 
sp500_stocks_flat <- sp500_stocks
names(sp500_stocks_flat) <- NULL
sp500_stocks_flat <- do.call(c, sp500_stocks_flat)

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


# system.time({
#   # run simulation for one sector with verbose 
#   best_sector_stocks <- f_MODELLING_PROCEDURE(G, tau, sp500_stocks, best_n = 6, verbose=TRUE)
#   
#   # pack the data into a format for modelling (only keep the data)
#   top_sector_stocks <- lapply(best_sector_stocks, function(x)x$data) 
# })

# 
# # save for kriti 
# save(top_sector_stocks, file = here("tests", "jair", "top_sector_stocks.rda"))


############################
### X. BACKTESTING TEST  ###
############################

# for every tau (run) in the backtesting
system.time({
  for(tau in seq(N_runs)){
    # close any positions 
    print("###############")
    print(paste0("### (tau=", tau, ") ###"))
    print("###############")
    
    # close any positions 
    print("CLOSE all positions")
    
    
    f_apply_modelling <- function(G){
      # execute sector procedure 
      print(paste0("    SECTOR_PROCEDURE(G=", G, ", tau=",tau, ")"))
      
      # run simulation for one sector with verbose 
      best_sector_stocks <- f_MODELLING_PROCEDURE(G, tau, sp500_stocks, best_n = 6, verbose=FALSE)
      
      # pack the data into a format for modelling (only keep the data)
      top_sector_stocks <- lapply(best_sector_stocks, function(x)x$data) 
      
      return(top_sector_stocks)
    }
    
    # obtain top stocks for all sectors 
    top_stocks_all_sectors <- lapply(sectors, f_apply_modelling)
    names(top_stocks_all_sectors) <- sectors
    
    # transform obtained list into a usable format 
    top_run_stocks <-  do.call(c, top_stocks_all_sectors)
    
    # calculate number of assets
    n <- length(names(top_sector_stocks))

    # extract the chosen tickers and create object for portfolio
    assets <- list(tickers = names(top_sector_stocks),
                   weight = rep(1/n, n), # equally weighted initially
                   num_shares = rep(0, n) # no shares initially
                  )

    # assign to portfolio
    portfolio$assets <- assets
    print("Cur Portfolio:")
    print(portfolio)
    
    # Optimize portfolio weights using modified min_variance 
    print("")
    print("(3) OPTIMIZE_PORTFOLIO(portfolio)")
    
    # long stocks in portfolio 
    print("")
    print("(4) LONG PORTFOLIO()")
    
    # TEST: Just for this small printing simulation !!
    if(tau > 1){
      break
    }
    print("-------------------------------------------------------------")
  }
})


# extract tickers from portfolio 
portf_tickers <- portfolio$assets$tickers

# retrieve current and previous timefor all tickers using tau 
tail_times <- tail(index(top_run_stocks[[1]]),2)

# retrieve current and previous adjusted price for a stock 
last_two_prices <- as.matrix(sapply(portf_tickers, function(ticker){
  # filter the matrix for the correct dates 
  coredata(sp500_stocks_flat[[ticker]][tail_times]$adjusted_close)
}))

# Calculate the arithmetic returns
returns <- (last_two_prices[2, ] - last_two_prices[1, ]) / last_two_prices[1, ]

# Calculate the logarithmic returns
log_returns <- log(last_two_prices[2, ] / last_two_prices[1, ])

