################################################################################
# run_data.R
# 
# @author: Hair Albeiro Parra Barrera
# @author: Xiao Xue 
# @author: Kriti Bhaya
# @author: Prateek 
# 
# This script contains the logic to extract and process the raw data for this 
# project. The preprocessed data is saved under data_clean/
################################################################################


#####################################
### 0. Libraries and Source files ###
#####################################

# load required scripts
library("here") 
source(here("functions", "data_load.R")) # raw data reading + minimal preprocessing
source(here("functions", "fetch_sp500_sectors.R")) # functions for top stocks and economic sectors in the sp500
source(here("functions", "feature_engineering.R")) # functions for feat eng and manipulation

######################################
### 1. Data Load and Preprocessing ###
######################################


# load sp500 data into env
sp500 <- f_load_sp500() # dSP500 components and weights from yahoo finance
sp500_sectors <- f_get_sp500_sectors() # economic sectors from wikipedia 

# Retrieve top 10 stocks by weight for each sector in the top 5 sectors from the SP500 (by weight)
sector_list <- f_retrieve_top_sp500(top_n_sectors = 6, top_n_stocks = 15, only_tickers=TRUE)

# function to fetch all the information for one ticker into a nice xts dataframe 
system.time(
  sp500_stocks <- lapply(sector_list, 
                         f_fetch_all_tickers, 
                         start_date="2017-01-01", # backtesting
                         end_date="2022-12-01")  
)

# Clean the environment 
data_stocks <- NULL
data_realized_volatility <- NULL
data_financial_ratios <- NULL
data_fama_french <- NULL 
xts_fama_french <- NULL 
xts_financial_ratios <- NULL
xts_realized_vol <- NULL
sp500 <- NULL
sp500_sectors <- NULL

# save preprocessed data in a data_clean 
save(sp500_stocks, file = here("data_clean", "sp500_stocks.rda"))






