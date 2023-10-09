# Load necessary packages
library("zoo")
library("xts")
library("PerformanceAnalytics")
library("TTR")
library("data.table")
library("quantmod")
library("here")

source(file = here("functions.R"))

# Read data_stocks.csv
df_stocks <- read.csv("data/data_stocks.csv")
df_stocks$Date <- as.Date(df_stocks$Date, format = "%d-%m-%Y")

# Read data_financial_ratios.csv
df_financial_ratios <- read.csv("data/data_financial_ratios.csv")
df_financial_ratios$Date <- as.Date(df_financial_ratios$Date, format = "%d-%m-%Y")
df_financial_ratios$adate <- NULL
df_financial_ratios$qdate <- NULL


# Read data_realized_volatility.csv
df_realized_vol <- read.csv("data/data_realized_volatility.csv")
# Use 10-day volatlity
df_realized_vol <- df_realized_vol[df_realized_vol$days == 10, ]
df_realized_vol$days <- NULL
colnames(df_realized_vol)[colnames(df_realized_vol) == "Volatility"] = "Realized Volatility"
df_realized_vol$Date <- as.Date(df_realized_vol$Date, format = "%d-%m-%Y")

# Read data_fama-french.csv
df_fama_french <- read.csv("data/data_fama_french.csv")
df_fama_french$Date <- as.Date(df_fama_french$Date, format = "%d-%m-%Y")


f_fetch_feature <- function(ticker){
  
  # Start building xts  
  df_current <- df_stocks[df_stocks$tic == ticker,]
  df_current <- as.xts(df_current, order.by = df_current$Date)
  df_current <- df_current[.indexwday(df_current) == 3]
  df_current$Date <- NULL 
  df_current$tic <- NULL
  storage.mode(df_current) <- "numeric"
  df_current$Returns_lag0 <- Return.calculate(prices = df_current$Close,
                                              method = "log")
  
  # Create lagged variables
  df_current$Returns_lag1 <- lag(df_current$Returns_lag0, k = 1)
  df_current$Returns_lag2 <- lag(df_current$Returns_lag0, k = 2)
  df_current$Returns_lag3 <- lag(df_current$Returns_lag0, k = 3)
  df_current$Price_lag1 <- lag(df_current$Close, k = 1)
  df_current$Price_lag2 <- lag(df_current$Close, k = 2)
  df_current$Price_lag3 <- lag(df_current$Close, k = 3)
  
  # Adding technical indicators
  df_current$ADX <- f_ADX(df_current)
  df_current$Aroon <- f_Aroon(df_current)
  df_current$ATR <- f_ATR(df_current)
  df_current$BB <- f_BB(df_current)
  df_current$ChaikinVol <- f_ChaikinVol(df_current)
  df_current$CLV <- f_CLV(df_current)
  df_current$EMV <- f_EMV(df_current)
  df_current$MACD <- f_MACD(df_current)
  df_current$MFI <- f_MFI(df_current)
  df_current$SAR <- f_SAR(df_current)
  df_current$SMI <- f_SMI(df_current)
  df_current$Vol <- f_Volat(df_current)
  
  #Add financial ratios to the feature xts
  xts_financial_ratios <- as.xts(df_financial_ratios, order.by = df_financial_ratios$Date)
  if (ticker %in% xts_financial_ratios$Ticker) {
    xts_financial_ratios$Date <- NULL
    xts_financial_ratios <- xts_financial_ratios[xts_financial_ratios$Ticker == ticker]
    xts_financial_ratios$Ticker <- NULL
    xts_financial_ratios$divyield <- gsub("%", "", xts_financial_ratios$divyield)
    storage.mode(xts_financial_ratios) <- "numeric"
    df_current <- merge.xts(df_current, xts_financial_ratios, join = "left")
    
    columns_to_fill <- colnames(xts_financial_ratios)  # Add the names of the columns you want to fill
    
    # Loop through the specified columns and forward-fill their missing values
    for (col_name in columns_to_fill) {
      df_current[, col_name] <- na.locf(df_current[, col_name], fromLast = TRUE)
      df_current[, col_name] <- na.locf(df_current[, col_name])
    }
  }
  
  df_current$P.E_lag1 <- lag(df_current$P.E, k = 1)
  df_current$Return.on.Equity_lag1 <- lag(df_current$Return.on.Equity, k = 1)
  df_current$Debt.Equity_lag1 <- lag(df_current$Debt.Equity, k = 1)
  df_current$quick.ratio_lag1 <- lag(df_current$quick.ratio, k = 1)
  df_current$curr.ratio_lag1 <- lag(df_current$curr.ratio, k = 1)
  df_current$Price.Book_lag1 <- lag(df_current$Book, k = 1)
  df_current$divyield_lag1 <- lag(df_current$divyield, k = 1)
  
  # Adding Fama-French facto
  xts_fama_french <- df_fama_french
  xts_fama_french <- as.xts(xts_fama_french, order.by = xts_fama_french$Date)
  xts_fama_french$Date <- NULL
  df_current <- merge.xts(df_current, xts_fama_french, join = "left")
  
  # Adding option implied volatility
  xts_realized_vol <- as.xts(df_realized_vol, order.by = df_realized_vol$Date)
  if (ticker %in% xts_realized_vol$Ticker) {
    xts_realized_vol <- xts_realized_vol[xts_realized_vol$Ticker == ticker]
    xts_realized_vol$Date <- NULL
    xts_realized_vol$Ticker <- NULL
    df_current <- merge.xts(df_current, xts_realized_vol, join = "left")
  }
  return(df_current)
}

tick <- "AAPL"

temp1 <- f_fetch_feature(tick)
