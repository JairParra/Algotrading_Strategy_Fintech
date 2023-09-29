# to obtain relative paths
library(here)

# source local files' code
source(here("functions", "fetch_sp500_sectors.R"))
source(here("functions", "feature_engineering.R"))

### 1. Data Extraction 

# Retrieve top 10 stocks by weight for each sector in the top 5 sectors from the SP500 (by weight)
sector_list <- f_retrieve_top_sp500(top_n_sectors = 6, top_n_stocks = 15, only_tickers=TRUE)
sector_list

# function to fetch all the information for one ticker into a nice xts dataframe 
sp500_stocks <- lapply(sector_list, 
                       f_fetch_all_tickers, 
                       start_date="2018-01-01",
                       end_date="2022-12-01") 


# Show the available sectors 
names(sp500_stocks)

# Show available stocks for Industrials 
names(sp500_stocks$Industrials)

# access the xts of the stocks in industrials 
head(sp500_stocks$Industrials$ADP)


### 2. Static Features (train-val) 

# test out for a sample run  
tau = 3 # suppose run number 3

# extract static features as a list of train and validation sets
sample_xts_train_val <- f_extract_train_val_features(sample_xts, tau=tau) 

# Display head of training data for that stock
head(sample_xts_train_val$train[,c("direction_lead", "clv", "volat", "month_index")]) 
print("")

# Display head of validation datat for that stock 
head(sample_xts_train_val$val[,c("direction_lead", "clv", "volat", "month_index")])


### 3. Dynamic Features (GARCH, ARIMA) 

# add all the features to the sample data 
sample_xts_full_feats <- f_extract_dynamic_features(sample_xts) 
head(sample_xts_full_feats)