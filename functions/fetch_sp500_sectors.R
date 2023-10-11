################################################################################
# fetch_sp500_sectors.R
#   Implements logic to retrieve information from the top n economic sectors in the SP500
#   with highest weight distribution, and also the top n stocks in each sector.
#
# @author: Hair Parra
################################################################################

#########################
### 0. Setup ###
#########################

#########################
### 1. Main Functions ###
#########################

f_load_sp500 <- function(){ 
  require(tidyquant)
  
  # load the dSP500 components via the yahoo finance API
  sp500 <- tq_index("SP500")
  
  # remove the US DOLLAR (not a company), LOW generates problems
  sp500 <- sp500[(!sp500$symbol == "-") & (!sp500$symbol == "LOW"), ]  
  
  return(sp500)
}

# # load into env 
# sp500 <- f_load_sp500()

f_get_sp500_sectors <- function(){
  ## Returns the SP500 components and their respective economic sectors
  ## by fetching the information from wikipedia. 

  
  # load the rvest package
  require(rvest)
  
  # Define the URL of the Wikipedia page
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  
  # Read the HTML content of the page
  page <- read_html(url)
  
  # Extract the table of S&P 500 constituents
  table <- html_nodes(page, xpath = '//*[@id="constituents"]') %>%
    html_table()
  
  # Get the first column of the table as a vector of tickers
  tickers <- table[[1]]$Symbol
  sectors <- table[[1]]$`GICS Sector`
  
  # Combine the tickers and sectors into a dataframe
  sp500_sectors <- data.frame(tickers, sectors)
  
  # remove problematic ticks 
  sp500_sectors <- sp500_sectors[(!sp500_sectors$tickers == "-") & (!sp500_sectors$tickers == "LOW"), ]  
  
  # Print the dataframe
  return(sp500_sectors)
}
# 
# # load into env 
# sp500_sectors <- f_get_sp500_sectors()


# function to fetch the tickers, weights and no.shares held for an economic sector 
# in the SP500. 
f_fetch_sector_data <- function(sector, sp500, sp500_sectors){
  ## Params: 
  ##    - sector (str): specific SP500 economic sector 
  ##    - sp500 (tibble data.frame):  SP500 data fetched via tq_index("SP500") 
  ##    - sp500_sectors (data.frame):   dataframe contianing each ticker from the SP500 and 
  ##                                    its corresponding economic sector.
  
  # subset the dataframe based on the current sector 
  sector_df <- sp500_sectors[sp500_sectors$sectors == sector, ] 
  
  # merge the sp500 full data, which will then only match the tickers for that sector 
  sector_df <- merge(sector_df, sp500, by.x = "tickers", by.y = "symbol")
  
  # subset only columns of interest 
  sector_df <- sector_df[, c("tickers", "sectors", "weight", "shares_held")]
  colnames(sector_df) <- c("ticker", "sector", "weight", "shares_held")
  
  return(sector_df)
}

# wrap into a single argument function  
# assuming that sp500 and sp500_sectors exist in environment
f_fetch_sp500_sector_data <- function(x){
  f_fetch_sector_data(x, sp500, sp500_sectors)
}


f_retrieve_top_sp500 <- function(top_n_sectors = 11, top_n_stocks = 10, only_tickers=TRUE){
  ## function that retrieves the top 10 stocks (and data) with highest weight for every 
  ## economic sector in the SP500, based on weight allocated. 
  ## Params: 
  ##  - top_n_sectors (numeric): Retrieve top n sectors with highest weights in the sp500
  ##  - top_n_stocks (numeric): Retrieve top n stocks per sector with highest weight in that sector
  ##  - only_tickers (bool): Only return a list of tickers per sector  
  
  # Load the required packages
  require(dplyr)
  require(rvest)
  require(tidyverse)
  require(tidyquant)
  
  # load the sp500 data
  sp500 <- f_load_sp500()
  
  # Fetch tickers and sectors  from the SP500 
  sp500_sectors <- f_get_sp500_sectors()
  
  # obtain the unique economic sectors in the sp500 
  unique_sectors <- unique(sp500_sectors$sectors)
  
  # fetch data (ticker, weight, shares held) for every economic sector
  sector_list <- lapply(unique_sectors, f_fetch_sp500_sector_data)
  names(sector_list) <- unique_sectors
  
  # calculate the sum of the weights for every economic sector 
  sector_weights <- lapply(sector_list, function(df){sum(df$weight)})
  
  # Subset only the top n economic sectors
  top_sectors <- names(sort(unlist(sector_weights), decreasing = TRUE)[1:top_n_sectors])
  sector_list <- sector_list[names(sector_list) %in% top_sectors]
  
  # Obtain the top_n stocks for each sector with highest weights for that sector 
  sector_list <- lapply(sector_list, function(df){df %>% top_n(top_n_stocks, weight)})
  
  # return only tickers if required 
  if (only_tickers){
    sector_list <- lapply(sector_list, function(df){df$ticker})
  }
  
  return(sector_list)
}


################
### Examples ###
################

## Example usage: 
# # Retrieve top 10 stocks by weight for each sector in the top 5 sectors from the SP500 (by weight)
# sector_list <- retrieve_top_sp500(top_n_sectors = 5, top_n_stocks = 10, only_tickers=TRUE)
# sector_list
# 
# OUT: 
#   
#   $`Health Care`
# [1] "ABBV" "ABT"  "AMGN" "DHR"  "JNJ"  "LLY"  "MRK"  "PFE"  "TMO"  "UNH" 
# 
# $`Information Technology`
# [1] "AAPL" "ACN"  "ADBE" "AMD"  "AVGO" "CRM"  "CSCO" "MSFT" "NVDA" "ORCL"
# 
# $`Communication Services`
# [1] "ATVI"  "CMCSA" "DIS"   "GOOG"  "GOOGL" "META"  "NFLX"  "T"     "TMUS"  "VZ"   
# 
# $Financials
# [1] "BAC"   "BRK-B" "GS"    "JPM"   "MA"    "MMC"   "MS"    "SPGI"  "V"     "WFC"  
# 
# $`Consumer Discretionary`
# [1] "ABNB" "AMZN" "BKNG" "HD"   "LOW"  "MCD"  "NKE"  "SBUX" "TJX"  "TSLA"

################################################################################

