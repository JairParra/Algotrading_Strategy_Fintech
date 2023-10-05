################################################################################
# fetch_sp500_sectors.R
#   Implements logic to retrieve data from the top n economic sectors in the SP500
#   with highest weight distribution, and also the top n stocks in each sector.
#
# @author: Hair Parra
################################################################################

#########################
### 0. Setup ###
#########################

# reqruired code from external file
source(here("functions", "feature_engineering.R"))

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

# load into env 
sp500 <- f_load_sp500()

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

# load into env 
sp500_sectors <- f_get_sp500_sectors()


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

####################################
### 2. Additional Data Retrieval ###
####################################

## Note: These functions are intended only to create some sample data to experiment with
## These functions come from the utils.R script from the Machine learning for quantitative finance
## book by David Ardia @ HEC Montreal. 



f_fetch_ind_base <- function(ticker, from, to){
  ############## Indicators ########################
  
  # A few technical indicators functions from TTR, see class notes 
  # for their definition
  # Calculate Average True Range (ATR)
  f_ATR <- function(x)
    ATR(HLC(x))[, 'atr']
  
  # Calculate Average Directional Index (ADX)
  f_ADX <- function(x)
    ADX(HLC(x))[, 'ADX']
  
  # Calculate Aroon Oscillator
  f_Aroon <- function(x){
      aroon(cbind(Hi(x), Lo(x)), n = 2)$oscillator
  }
  
  # Calculate Bollinger Bands percentage B
  f_BB <- function(x)
    BBands(HLC(x))[, "pctB"]
  
  # Calculate Chaikin Volatility
  f_ChaikinVol <- function(x)
    Delt(chaikinVolatility(cbind(Hi(x), Lo(x))))[, 1]
  
  # Calculate Close Location Value (CLV) using Exponential Moving Average (EMA)
  f_CLV <- function(x)
    EMA(CLV(HLC(x)))[, 1]
  
  # Calculate Ease of Movement (EMV)
  f_EMV <- function(x)
    EMV(cbind(Hi(x), Lo(x)), Vo(x))[, 2]
  
  # Calculate Moving Average Convergence Divergence (MACD)
  f_MACD <- function(x)
    MACD(Cl(x))[, 2]
  
  # Calculate Money Flow Index (MFI)
  f_MFI <- function(x)
    MFI(HLC(x), Vo(x))
  
  # Calculate Parabolic Stop and Reverse (SAR)
  f_SAR <- function(x)
    SAR(cbind(Hi(x), Cl(x))) [, 1]
  
  # Calculate Stochastic Momentum Index (SMI)
  f_SMI <- function(x)
    SMI(HLC(x))[, "SMI"]
  
  # Calculate Volatility using Garman-Klass method
  f_Volat <- function(x)
    volatility(OHLC(x), calc = "garman")[, 1]
  
  
  ############## Data Preparation ########################
  
  # Download the stock data for ticker from Yahoo (default) 
  # between the dates from and to.
  stock <- getSymbols(ticker,
                      auto.assign = FALSE,
                      from = from,
                      to = to)
  
  # Choose Wednesday adjusted close price and compute their returns 
  # using PerformanceAnalytics library
  # indexwday() == 3 corresponds to wednesday
  stock_wed <- stock[.indexwday(stock) == 3]
  stock_wed_rets <-
    PerformanceAnalytics::Return.calculate(prices = stock_wed,
                                           method = "log")[, 6]
  
  # Create lagged returns and convert to xts object
  
  # Create a base name from ticker by removing any character that is not
  # a number or letter and then converting the results to lower case letters
  base_name <- tolower(gsub("[[:punct:]]", "", ticker))
  
  # Create 1 week forward (realized) returns by shifting the returns by a period
  realized_returns <- as.xts(data.table::shift(stock_wed_rets, type = "lead")) # stock_adjclose_lead
  
  # Name the column using the base_name variable
  colnames(realized_returns)[1] <- paste0(base_name,"_adjclose_lead")
  
  # Create a 1 week forward (realized) stock returns direction
  direction <- rep(NA, nrow(stock_wed_rets))
  direction[realized_returns > 0.] = 1  # UP 
  direction[realized_returns <= 0.] = -1  # DOWN
  
  # Create data.frame with output variable and predictors
  df_ticker <- data.frame(
    date = index(stock_wed_rets),
    direction = factor(direction, levels = c(1, -1)),
    stock_close_lead = realized_returns,
    coredata(stats::lag(stock_wed_rets, k = 0:3)),
    atr = coredata(f_ATR(stock_wed)),
    adx = coredata(f_ADX(stock_wed)),
    aroon = coredata(f_Aroon(stock_wed)),
    bb = coredata(f_BB(stock_wed)),
    chaikin_vol = coredata(f_ChaikinVol(stock_wed)),
    clv = coredata(f_CLV(stock_wed)),
    emv = coredata(f_EMV(stock_wed)),
    macd = coredata(f_MACD(stock_wed)),
    mfi = coredata(f_MFI(stock_wed)),
    sar = coredata(f_SAR(stock_wed)),
    smi = coredata(f_SMI(stock_wed)),
    volat = coredata(f_Volat(stock_wed))
  )
  
  # Rename the various columns using the base_name and the technical indicators names
  base_name <- tolower(gsub("[[:punct:]]", "", ticker))
  col_names <- c("date", "direction_lead", 
                 "realized_returns",# stock_adjclose_lead = TARGET
                 "actual_returns", # "adjclose_lag0" = 
                 "adjclose_lag1", 
                 "adjclose_lag2",
                 "adjclose_lag3",
                 "atr", "adx", "aaron", "bb", "chaikin_vol", "clv", 
                 "emv", "macd", "mfi", "sar", "smi", "volat")
  names(df_ticker) <- col_names
  
  # convert to xts format and drop the date column 
  xts_ticker <- xts(df_ticker, order.by = as.Date(df_ticker$date))
  
  # drop the useless date column 
  xts_ticker <- xts_ticker[, colnames(xts_ticker) != "date"]
  
  # assign date numeric index 
  suppressWarnings(
    xts_ticker <- f_assign_int_month_index(xts_ticker)
  )

  return(xts_ticker)
}


# fetches the xts data for all the stocks above at once 
# and packs it into a list (or a list of lists if nested_list=TRUE,)
f_fetch_all_tickers <- function(tickers, 
                       start_date = "2000-01-01", 
                       end_date = "2019-12-31"){ 
  
  # Use lapply to download all the tickers data at once for dates
  # between start_date and end_date
  list_stock_data <- lapply(tickers,
                            f_fetch_ind_base,
                            from = as.Date(start_date), 
                            to = as.Date(end_date))
  
  # Create a list containing the tickers (stock names) and the stock data
  list_stock_data <- list(tickers = tickers,
                          stock_data = list_stock_data)
  
  # transform list format 
  list_stock_data <- setNames(list_stock_data$stock_data, list_stock_data$tickers)
  
  return(list_stock_data)
  
}

