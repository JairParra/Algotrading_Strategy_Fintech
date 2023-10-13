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
# 
# Notes: 
#   - Every run tau symbolizes that we took the past 2 years of data, and 
#     we run the strategy to rebalance our portfolio. 
#   - Once rebalanced, we hold for one month.
#   - At the next run (determined by tau), we calculate the P&L of portfolio performance
#     and run the strategy again.
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
source(here("functions", "strategy.R")) # modelling general strategy 
source(here("functions", "portfolio_optimization.R")) # functions for top stocks and economic sectors in the sp500

#######################
### 1. Data Loading ###
#######################

# Load preprocessed data from the data_clean directory 
load(here("data_clean", "sp500_stocks.rda"))

# create flattened version for ease (delete middle nested list layer)
sp500_stocks_flat <- sp500_stocks
names(sp500_stocks_flat) <- NULL
sp500_stocks_flat <- do.call(c, sp500_stocks_flat)

################################
### 2. Simulation Parameters ###
################################

# Set up backtesting simulation parameters
sample_xts <- sp500_stocks_flat[[1]] # sample stock from data 
sectors <- names(sp500_stocks)
N_sector_max_best_stocks <- 6 # maximum number of stocks to choose, but never reached in practice

# Formula parameters
slide <- 1 # moving one month at the time
N_months <- length(names(split.xts(sample_xts, f= "months"))) # total number of months 
N_window <- 24 # number of months in size for each window (2 years)
N_runs <- floor((N_months - N_window)/slide) # total number of runs/taus

# setup initial portfolio tracking variables 
initial_capital <- 500000 
num_tickers <- length(sectors)*N_sector_max_best_stocks*2 # two sub-strategies for picking
weights <- rep(1/num_tickers, num_tickers) # initialize to 1/n
returns <- rep(NA, N_runs) # returns for the portfolio across time 
rebalance_dates <- rep("", N_runs) # rebalance dates
capital_history <- rep(NA, N_runs) # keeps track of the capital at every step 
stop_loss <- initial_capital*0.25 # half of the capital lost 

# repack the portfolio for tracking 
# Note: 
# assets = list of tickers
#       |-> ticker = list of (weight, num_shares) 
portfolio <- list(assets = NA, 
                  capital = initial_capital, 
                  capital_history = capital_history, 
                  returns = returns, # discrete
                  dates = rebalance_dates
)

# display parameters
print(paste0("Running backtesting with the following parameters: "))
print(paste0("- N_months: ", N_months))
print(paste0("- N_runs: ", N_runs)) 
print(paste0("- slide: ", slide))
print(paste0("- Initial Capital: ", initial_capital))


#######################
### 3. Backtesting  ###
#######################

# for every tau (run) in the backtesting
system.time({
  for(tau in seq(N_runs)){
    print("-------------------------------------------------------------")
    
    # close any positions 
    print("###################################################################")
    print(paste0("### (tau=", tau, ") ###"))
    print("###################################################################")
    print("Portfolio (beginning run): ") 
    print(portfolio)
    print("###################################################################")
    
    ##################################################################
    ## 0. Check stop loss 
    
    if(portfolio$capital < stop_loss){
      print("Stop loss reached. Stopping trading...")
      break 
    }
  
    ##################################################################
    ## 1. Obtain current positions
    
    # get current date with the tau 
    cur_date <- sp500_stocks_flat[[1]][sp500_stocks_flat[[1]]$month_index == tau + N_window - 1 + 1]
    cur_date <- head(index(cur_date), 1)
    
    ##################################################################
    ## 1. Close Positions 
    
    # close any positions 
    print("1. CLOSE all positions")
    
    # get current date with the tau and update in portfolio 
    cur_date <- sp500_stocks_flat[[1]][sp500_stocks_flat[[1]]$month_index == tau + N_window]
    cur_date <- as.character(head(index(cur_date), 1))
    portfolio$dates[[tau]] <- cur_date
    
    if(tau != 1){
      
      # extract tickers from portfolio 
      portf_tickers <- portfolio$assets$tickers
      
      # retrieve current prices for assets in portfolio 
      portf_prices <- sapply(portf_tickers, function(x){
                              f_read_stock_price(x, sp500_stocks_flat, cur_date)
                            })
      
      # close all positions at current price 
      portfolio$capital <- sum(portfolio$assets$num_shares * portf_prices)
      portfolio$capital_history[[tau]] <- portfolio$capital
      
      # Calculate the portfolio returns 
      cur_capital = portfolio$capital
      prev_capital = portfolio$capital_history[[tau-1]]
      portfolio$returns[[tau]] = (cur_capital - prev_capital) / prev_capital
      
    }
    else{
      # no close in the first run 
      portfolio$capital_history[1] = initial_capital
      portfolio$returns[[tau]] = 0
    }
    
    ##################################################################
    ## 2. Modelling 
    print("2. Modelling...")
    
    ## Define a wrapper that performs the SECTOR PROCEDURE to execute with lapply 
    f_apply_modelling <- function(G){
      # execute sector procedure 
      print(paste0("    SECTOR_PROCEDURE(G=", G, ", tau=",tau, ")"))
      
      # run simulation for one sector with verbose 
      best_sector_stocks <- f_MODELLING_PROCEDURE(G, tau, sp500_stocks,
                                                  best_n = N_sector_max_best_stocks, 
                                                  verbose=FALSE)
      
      # pack the data into a format for modelling (only keep the data)
      top_sector_stocks <- lapply(best_sector_stocks, function(x)x$data) 
      
      return(top_sector_stocks)
    }
    
    # obtain top stocks for all sectors 
    top_stocks_all_sectors <- lapply(sectors, f_apply_modelling)
    names(top_stocks_all_sectors) <- sectors
    
    # transform obtained list into a usable format 
    top_run_stocks <-  do.call(c, top_stocks_all_sectors)
    names(top_run_stocks) <- sub(".*\\.", "", names(top_run_stocks))
    
    # calculate number of assets
    n <- length(names(top_run_stocks))

    # extract the chosen tickers and create object for portfolio
    assets <- list(tickers = names(top_run_stocks),
                   weight = rep(1/n, n), # equally weighted initially
                   num_shares = rep(0, n) # no shares initially
                  )

    # assign to portfolio
    portfolio$assets <- assets
    
    ###################################################################
    
    ## 3. Portfolio Optimization 
    
    # Optimize portfolio weights using modified min_variance 
    print("")
    print("(3) OPTIMIZE_PORTFOLIO(portfolio)")
    
    ## Retrieve the data for each of these 
    optimal_minvar_weights <- f_optimize_portfolio(top_run_stocks, min_alloc = 0.05)
    portfolio$assets$weight <- optimal_minvar_weights
    
    ###################################################################
    
    ## 4. Portfolio Rebalance
    
    # extract tickers from portfolio (once again, different portfolio)
    portf_tickers <- portfolio$assets$tickers
    
    # calculate how much money to put per share 
    weighted_capital <- portfolio$assets$weight * portfolio$capital 
    names(weighted_capital) <- portf_tickers
    
    # retrieve current prices for assets in portfolio 
    portf_prices <- sapply(portf_tickers, function(x){
      f_read_stock_price(x, sp500_stocks_flat, cur_date)
    })
    
    # calculate number of shares of the portfolio 
    portfolio$assets$num_shares <- weighted_capital / portf_prices

    #########################################################################
    
    print("Cur Portfolio:")
    print(portfolio)
    
    # long stocks in portfolio 
    print("")
    print("(4) LONG PORTFOLIO()")
    
    #######################################################################
    
    # TEST: Just for this small printing simulation !!
    if(tau > 4){
      break
    }
    print("-------------------------------------------------------------")
  }
})

print("SUMMARY:") 
print(portfolio$dates)
print(portfolio$returns)
print(portfolio$capital_history)
