################################################################################
# feature_engineering.R
#   This script contains functions related to data manipulation and 
#   feature engineering. 
#
# @author: Hair Parra
################################################################################


################
### 0. Setup ###
################

# packages
library("here") 

# custom functions 
source(here("functions", "data_load.R")) # raw data reading + minimal preprocessing
source(here("functions", "technical_indicators.R")) # technical indicators functions


################################################################################

#########################################################
### 1. Data Retrieval and General Feature Engineering ###
#########################################################

# Function to count months and assign index
f_assign_int_month_index <- function(xts_data) {
  require("xts")
  
  # Extract the date index
  date_index <- index(xts_data)
  
  # Extract the unique months and years
  unique_months <- unique(format(date_index, "%Y-%m"))
  
  # Count the number of unique months
  num_months <- length(unique_months)
  
  # Create a month index for each row
  month_index <- match(format(date_index, "%Y-%m"), unique_months)
  
  # Add the month index as a new column to the xts dataframe
  xts_data$month_index <- month_index
  
  # Return the modified xts dataframe and the number of unique months
  modified_xts_data = xts_data
  
  return(modified_xts_data)
}


f_fetch_ind_base <- function(ticker, from, to, extra_feats_xts){
  ### Fetches data and performs feature engineering for specified ticker
  
  
  ###########################
  ### Additional Raw data ###
  ###########################
  
  xts_fama_french <- extra_feats_xts$xts_fama_french
  xts_financial_ratios <- extra_feats_xts$xts_financial_ratios
  xts_realized_vol <- extra_feats_xts$xts_realized_vol
  
  ########################
  ### Base Engineering ###
  ########################
  
  # Download the stock data for ticker from Yahoo (default) 
  # between the dates from and to.
  stock <- tryCatch({
    quantmod::getSymbols(ticker,
                         auto.assign = FALSE,
                         from = from,
                         to = to)
  }, error = function(e){
    warning("ticker not found. NULL returned.")
    return(NA)
  }
  )
  
  # Choose Wednesday adjusted close price and compute their returns 
  # using PerformanceAnalytics library
  # indexwday() == 3 corresponds to wednesday
  stock_wed <- stock[.indexwday(stock) == 3]
  stock_wed_rets <-
    PerformanceAnalytics::Return.calculate(prices = stock_wed,
                                           method = "log")[, 6]
  discrete_returns <-
    PerformanceAnalytics::Return.calculate(prices = stock_wed,
                                           method = "discrete")[, 6]
  
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
    adjusted_close = stock_wed[, 6], # stock price
    direction = factor(direction, levels = c(1, -1)),
    discrete_retruns = discrete_returns, # discrete returns, unlagged
    realized_returns = realized_returns, # log returns, future lag 
    coredata(stats::lag(stock_wed_rets, k = 0:3)), # lagged returns, log 
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
    volume = stock_wed[, 5], 
    volat = coredata(f_Volat(stock_wed))
  )
  
  # Rename the various columns using the base_name and the technical indicators names
  base_name <- tolower(gsub("[[:punct:]]", "", ticker))
  col_names <- c("date", 
                 "adjusted_close", # raw adjusted close price
                 "direction_lead", 
                 "discrete_returns",
                 "realized_returns",# stock_adjclose_lead = TARGET
                 "log_returns_lag0", # "adjclose_lag0" = 
                 "log_returns_lag1", 
                 "log_returns_lag2",
                 "log_returns_lag3",
                 "atr", "adx", "aaron", "bb", "chaikin_vol", "clv", 
                 "emv", "macd", "mfi", "sar", "smi", 
                 "volume", 
                 "volat")
  names(df_ticker) <- col_names
  
  
  # convert to xts format and drop the date column 
  xts_ticker <- xts(df_ticker, order.by = as.Date(df_ticker$date))
  
  # drop the useless date column 
  xts_ticker <- xts_ticker[, colnames(xts_ticker) != "date"]
  
  # assign date numeric index 
  suppressWarnings(
    xts_ticker <- f_assign_int_month_index(xts_ticker)
  )
  
  #############################
  ### External Data Sources ###
  #############################
  
  ### 1. Financial Ratios 
  
  # check ticker exists in the data to add the ratios
  if (ticker %in% xts_financial_ratios$Ticker) {
    
    # subset ratios for that ticker 
    xts_ticker_financial_ratios <- xts_financial_ratios[xts_financial_ratios$Ticker == ticker] 
    
    # remove ticker and merge with data 
    xts_ticker_financial_ratios$Ticker <- NULL 
    xts_ticker <- merge.xts(xts_ticker, xts_ticker_financial_ratios, join = "left")
    
    # fill the NAs using the last row 
    xts_ticker <- na.locf(xts_ticker, fromLast = TRUE)
    xts_ticker <- na.locf(xts_ticker)
    
    # remove temp object 
    xts_ticker_financial_ratios <- NULL
    
    # Calculate financial ratio lagged features 
    xts_ticker$P.E_lag1 <- stats::lag(xts_ticker$P.E, k=1)
    xts_ticker$Return.on.Equity_lag1 <- stats::lag(xts_ticker$Return.on.Equity, k = 1)
    xts_ticker$Debt.Equity_lag1 <- stats::lag(xts_ticker$Debt.Equity, k = 1)
    xts_ticker$quick.ratio_lag1 <- stats::lag(xts_ticker$quick.ratio, k = 1)
    xts_ticker$curr.ratio_lag1 <- stats::lag(xts_ticker$curr.ratio, k = 1)
    xts_ticker$Price.Book_lag1 <- stats::lag(xts_ticker$Book, k = 1)
    xts_ticker$divyield_lag1 <- stats::lag(xts_ticker$divyield, k = 1)
    
  }
  else{
    warning(paste0("No financial ratio data for ticker ", ticker, ", skipping..."))
  }
  
  
  ### 3. FAMA-FRENCH Factors
  
  # merge with data 
  xts_ticker <- merge.xts(xts_ticker, xts_fama_french, join = "left")
  
  
  ### 4. Implied Option Volatility 
  
  # check if ticker exists in xts 
  if (ticker %in% xts_realized_vol$Ticker) {
    
    # subset ratios for that ticker 
    xts_ticker_realized_vol <- xts_realized_vol[xts_realized_vol$Ticker == ticker] 
    
    # remove ticker and merge with data 
    xts_ticker_realized_vol$Ticker <- NULL 
    xts_ticker <- merge.xts(xts_ticker, xts_ticker_realized_vol, join = "left")
    
    
    # fill the NAs using the last row 
    xts_ticker <- na.locf(xts_ticker, fromLast = TRUE)
    xts_ticker <- na.locf(xts_ticker)
    
    # remove temp object 
    xts_ticker_realized_vol <- NULL
  }
  else{
    warning(paste0("No IV data for ticker ", ticker, ", skipping..."))
  }

  return(xts_ticker)
}


# fetches the xts data for all the stocks above at once 
# and packs it into a list (or a list of lists if nested_list=TRUE,)
f_fetch_all_tickers <- function(tickers, 
                                start_date = "2017-01-01", 
                                end_date = "2022-12-31", 
                                verbose = FALSE
                                ){ 
  
  if(verbose){
    print("running f_fetch_all_tickers...")
    print(tickers)
  }
  
  # load required csv files data into memory
  extra_feats_xts <- f_preload_raw_data(from=start_date, to=end_date)
  
  # Use lapply to download all the tickers data at once for dates
  # between start_date and end_date
  list_stock_data <- lapply(tickers,
                            function(x, from, to, extra_feats_xts) tryCatch({
                              f_fetch_ind_base(x, from = from, to=to, 
                                               extra_feats_xts = extra_feats_xts)
                            }, error = function(e){ 
                              print(paste0("(f_fetch_all_tickers) --> error with ticker: ", x, ", returning null..."))
                              return(NULL)
                              }
                            )
                              ,
                            from = as.Date(start_date), 
                            to = as.Date(end_date), 
                            extra_feats_xts = extra_feats_xts
                            )
  
  # Create a list containing the tickers (stock names) and the stock data
  list_stock_data <- list(tickers = tickers,
                          stock_data = list_stock_data)
  
  # transform list format 
  list_stock_data <- setNames(list_stock_data$stock_data, list_stock_data$tickers)
  
  # remove all ticker with null data 
  list_stock_data <- compact(list_stock_data)
  
  # Clean the environment
  data_fama_french <- NULL
  data_financial_ratios <- NULL
  data_realized_volatility <- NULL
  data_stocks <- NULL
  extra_feats_xts <- NULL
  
  return(list_stock_data)
  
}

################################################################################

##########################
### 1. Static Features ###
##########################

# function that extracts the static (no-changing) features from a matrix of features 
f_extract_train_val_features <- function (stock_data, tau = NULL, n_months = 12, val_lag = 2){
  ## 
  ## Params: 
  ##  - stock_data (xts): An xts object contianing the features and target for the stock 
  ##  - tau (int): the current run number used to split the data 
  ##  - n_months (int): the number of months to consider for the window 
  ##  - val_lag (int): Number of months to consider for the validation set 
  
  # Calculate the beginning and end of the current window 
  t_start <- tau
  t_end <- tau + n_months - 1 
  t_val <- t_end - val_lag
  
  # Subset the appropriate train and test sets for that stock 
  train_sub = stock_data[(stock_data$month_index >= t_start) & (stock_data$month_index <= t_val)] 
  val_sub = stock_data[(stock_data$month_index > t_val) & (stock_data$month_index <= t_end)] 
  
  
  stock_train_val <- list(train = train_sub,
                          val = val_sub
  )
  
  return(stock_train_val)
}  


# function that extracts data for the window only, but does not split the data into train-val 
f_extract_window <- function (stock_data, tau = NULL, n_months = 12){
  ## 
  ## Params: 
  ##  - stock_data (xts): An xts object contianing the features and target for the stock 
  ##  - tau (int): the current run number used to split the data 
  ##  - n_months (int): the number of months to consider for the window 
  ##  - val_lag (int): Number of months to consider for the validation set 
  
  # Calculate the beginning and end of the current window 
  t_start <- tau
  t_end <- tau + n_months - 1 

  # Subset the appropriate train and test sets for that stock 
  data_sub = stock_data[(stock_data$month_index >= t_start) & (stock_data$month_index <= t_end)] 
  
  return(data_sub)
}  

# function that extracts the train and val only  
f_extract_train_val_no_window <- function (stock_data, val_lag = 2){
  ## 
  ## Params: 
  ##  - stock_data (xts): An xts object contianing the features and target for the stock 
  ##  - val_lag (int): Number of months to consider for the validation set 
  
  # Calculate the beginning and end of the current window 
  t_start <- as.vector(stock_data[, c("month_index")])[1]
  t_end <- as.vector(stock_data[, c("month_index")])[nrow(stock_data)]
  t_val <- t_end - val_lag
  
  # Subset the appropriate train and test sets for that stock 
  train_sub = stock_data[(stock_data$month_index >= t_start) & (stock_data$month_index <= t_val)] 
  val_sub = stock_data[(stock_data$month_index > t_val) & (stock_data$month_index <= t_end)] 
  
  # pacl into a list to return 
  stock_train_val <- list(train = train_sub,
                          val = val_sub)
  
  return(stock_train_val)
}  

###########################
### 2. Dynamic Features ###
###########################


f_add_garch_forecast <- function(stock_xts, volat_col="volat") {
  ## Computes the GARCH(1,1) on the data, procudes in-sample and out-of-sample forecasts, 
  ## and assigns a shifted forecasted volatility vector as a feature to the data. 
  
  # specify packages 
  require("xts") 
  require("rugarch")
  
  # Check if stock_xts is an xts object with a column named 'volat_col'
  if (!is.xts(stock_xts) || !(volat_col %in% colnames(stock_xts))) {
    stop("Invalid input: x must be an xts object with a column named 'volat'")
  }
  
  # Remove any rows that contain NA values in 'volat_col'
  # stock_xts_no_na <- na.omit(stock_xts)
  stock_xts_no_na <- stock_xts[complete.cases(stock_xts[, volat_col]), ]
  
  # Fit a GARCH(1,1) model to the 'volat_col' column
  spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                           garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), 
                                       include.mean = FALSE), 
                     )
  fit <- ugarchfit(spec, data = stock_xts_no_na[, volat_col], solver = "hybrid")
  
  # Predict the volatility for the in-sample data and the next 4 steps
  pred <- ugarchforecast(fit, n.ahead = 4)
  
  # Extract the predicted volatility as a vector
  sigma <- as.vector(pred@forecast$sigmaFor)
  
  # Construct shifted forecasted volatility vector 
  shifted_sigma <- c(as.vector(stock_xts[, volat_col])[5:dim(stock_xts)[1]], sigma)
  
  # Assign the forecasted volatility to the original xts object as a new column, shifted by 4 steps
  stock_xts$vol_forecast <- shifted_sigma
  
  # Return the modified xts object
  return(stock_xts)
}



# Define the function
f_add_arima_forecast <- function(stock_data, arima_col) {
  ## Computes in-sample and OOS ARIMA(p,d,q) forecasts on the input return_col from the data. 
  ## All the combinations for p,d,q = {0,1} are computed and added.
  
  require("xts")
  require("forecast")
  
  # Validate input
  if (!is.xts(stock_data) || !(arima_col %in% colnames(stock_data))) {
    print("cols(stock_data):")
    print(cols(stock_data))
    stop("Invalid input: stock_data must be an xts object and return_col must be a column in stock_data")
  }
  
  # Remove NA values from the specified column
  stock_data_no_na <- na.omit(stock_data[, arima_col, drop = FALSE])
  
  # Calculate the number of steps to forecast based on the number of weeks 
  # of last month + 1 
  num_pred_weeks <- nrow(stock_data[stock_data$month_index == tail(as.vector(stock_data[, "month_index"]),1), ])

  # Convert the xts column to a basic time series object
  ts_data <- as.ts(coredata(stock_data_no_na[, 1]))
  
  # Generate a list of all combinations of p, d, q (0 or 1)
  pdq_combinations <- expand.grid(p = 0:1, d = 0:2, q = 0, P=0, D=0:1, Q=1)
  
  # save best arima model
  lowest_sigma2 <-  Inf
  best_shifted_arima <- NULL
  
  for (i in 1:nrow(pdq_combinations)) {
    # extract parameters
    p <- pdq_combinations[i, "p"]
    d <- pdq_combinations[i, "d"]
    q <- pdq_combinations[i, "q"]
    P <- pdq_combinations[i, "P"]
    D <- pdq_combinations[i, "D"]
    Q <- pdq_combinations[i, "Q"]
    
    # skip if 000 - XXX 
    if(p==0 & d ==0 & q==0){ 
      next
    }
    
    # autofit the best arima model 
    fit <- tryCatch({
      Arima(ts_data, order = c(p,d,q), seasonal = c(P,D,Q))
    }, 
    error = function(e){
      warning(paste0("error for SARIMA(", p,",", d,",", q,",", P,",", D,",", Q, ") -> skipping..."))
      return(NULL)
    })  
    
    # skip this arima fit if not stationary 
    if(is.null(fit)){
      next
    }
    
    # Forecast the next 4 steps (+1 of missing realized return for today)
    pred <- forecast(fit, h = num_pred_weeks)
    
    # Obtain in-sample fitted values 
    pred_is <- fit$fitted 
    
    # Extract the forecasted values as a vector
    arima_forecast <- as.vector(pred$mean)

    # Stack in-sample forecasts with OOS
    shifted_arima <- c(pred_is, arima_forecast)

    # Subset to shift the future values as features 
    shifted_arima <- tail(shifted_arima, nrow(stock_data))
    
    # create an xts with the original index 
    shifted_arima_xts <- xts(shifted_arima, order.by = index(stock_data))
    names(shifted_arima_xts) <- c(paste0("sarima_", p, d, q, "_", P, D, Q, collapse=""))
    
    # concatenate result to original dataframe 
    stock_data <- cbind.xts(stock_data, shifted_arima_xts)
    
    # Svae model forecasts with lowest mse of the residuals
    if(fit$sigma2 < lowest_sigma2){ 
      best_shifted_arima <- shifted_arima_xts
      names(best_shifted_arima) <- "best_shifted_arima"
    }
  }
  
  # Append best shifted arima to stock 
  stock_data <- cbind.xts(stock_data, best_shifted_arima)
  
  # Return the modified xts object
  return(stock_data)
}


f_add_arima_forecast_lapply <- function(stock_data, arima_col) {
  
  # Import libraries
  require("xts")
  require("forecast")
  
  # Validate input
  if (!is.xts(stock_data) || !(arima_col %in% colnames(stock_data))) {
    stop("Invalid input: stock_data must be an xts object and return_col must be a column in stock_data")
  }
  
  # Remove NA values
  stock_data_no_na <- na.omit(stock_data[, arima_col, drop = FALSE])
  
  # Calculate the number of steps to forecast
  num_pred_weeks <- nrow(stock_data[stock_data$month_index == tail(as.vector(stock_data[, "month_index"]),1), ])
  
  # Convert to basic time series
  ts_data <- as.ts(coredata(stock_data_no_na[, 1]))
  
  # Generate combinations of p, d, q
  pdq_combinations <- expand.grid(p = 0:1, d = 0:2, q = 0, P = 0, D = 0:1, Q = 1)
  
  # Initialize the best model tracker
  lowest_sigma2 <- Inf
  best_shifted_arima <- NULL
  
  # Function to encapsulate the loop logic
  arima_logic <- function(pdq_row) {
    p <- pdq_row["p"]
    d <- pdq_row["d"]
    q <- pdq_row["q"]
    P <- pdq_row["P"]
    D <- pdq_row["D"]
    Q <- pdq_row["Q"]
    
    # Skip if 000
    if(p == 0 & d == 0 & q == 0) {
      return(NULL)
    }
    
    # Fit ARIMA model
    fit <- tryCatch({
      Arima(ts_data, order = c(p,d,q), seasonal = c(P,D,Q))
    }, error = function(e) {
      warning(paste0("error for SARIMA(", p,",", d,",", q,",", P,",", D,",", Q, ") -> skipping..."))
      return(NULL)
    })
    
    # Skip non-stationary fits
    if(is.null(fit)) {
      return(NULL)
    }
    
    # Forecast
    pred <- forecast(fit, h = num_pred_weeks)
    pred_is <- fit$fitted
    arima_forecast <- as.vector(pred$mean)
    shifted_arima <- c(pred_is, arima_forecast)
    shifted_arima <- tail(shifted_arima, nrow(stock_data))
    shifted_arima_xts <- xts(shifted_arima, order.by = index(stock_data))
    names(shifted_arima_xts) <- c(paste0("sarima_", p, d, q, "_", P, D, Q, collapse=""))
    
    return(list(shifted_arima_xts, fit$sigma2))
  }
  
  # Apply function over combinations
  arima_results <- lapply(1:nrow(pdq_combinations), function(i) {
    arima_logic(pdq_combinations[i, ])
  })
  
  # Filter out NULLs
  arima_results <- Filter(Negate(is.null), arima_results)
  
  # Get best model
  best_result <- Reduce(function(a, b) {
    if (is.null(a) || b[[2]] < a[[2]]) return(b) else return(a)
  }, arima_results)
  
  best_shifted_arima <- best_result[[1]]
  names(best_shifted_arima) <- "best_shifted_arima"
  
  # Combine all columns with stock_data
  all_arima_cols <- do.call(cbind, lapply(arima_results, `[[`, 1))
  stock_data <- cbind.xts(stock_data, all_arima_cols, best_shifted_arima)
  
  return(stock_data)
}



f_extract_dynamic_features <- function(stock_data, 
                                       arima_col="realized_returns", 
                                       volat_col="volat"){
  ## Computes and incorporates GARCH(1,1) 4-days-shifted forecasted volatility features 
  ## as well as 4-days-shifted forecasted returns ARIMA(p,d,q) features for all combinations 
  ## of p,d,q in {0,1}. 
  ## 
  ## Params: 
  ##    - stock_data (xts): A date-indexed xts object containing features for a stock. 
  ##    - retun_col (str): Specifies the name of the column containing the realized returns
  ##    - volat_col (str): Specifies the name of the column containing the volatility
  
  require("xts")
  require("forecast")
  require("rugarch")
  
  # Compute the ARIMA features  
  stock_data <- f_add_arima_forecast(stock_data, arima_col=arima_col)
  
  # Compute the GARCH features
  stock_data <- f_add_garch_forecast(stock_data, volat_col=volat_col) 
  
  return(stock_data)
}


############################
### 3. Feature Selection ###
############################

f_select_features <- function(fmla, 
                              data, 
                              target_var, 
                              volat_col, 
                              garch_col = NULL, 
                              method="exhaustive", 
                              nvmax=15
){ 
  ## Wrapper for feature selection given some formula and train data. 
  ## 
  ## Params: 
  ##    - fmla (formula): lm-like R formula for the linear fitting. 
  ##    - data (xts): should contain the train set from a stock in list_train_val_sector
  ##                  , which corresponds to one economic sector.
  ##    - target_var (str): columnname which contains the target variable in the data
  ##    - volat_col (str): columnname which contains the volatility column in the data.
  ##    - garch_col (str): colunname which contains the GARCH(1,1) feature column of the data.
  ##    - method (str): actual method in regsubsets()
  ##    - nvmax (int):max size of subsets to examine
  ##
  ## NOTE: the columns volat_col, garch_col and arima_col are always kept in the final data. 
  
  # require the package 
  require("leaps")
  
  # Perform backward stepwise selection using 'regsubsets()' on the training data
  regfit_bwd <- suppressMessages(regsubsets(x = fmla, 
                           data = data, 
                           method = method, 
                           nvmax = nvmax # max size of subsets to examine
  ))
  
  # Generate a summary of the fitted model
  reg_summary <- summary(regfit_bwd) 
  
  # Identify the index of the model with the minimum BIC value
  id_best <- which.max(reg_summary$adjr2) 
  
  # Get the column names from the 'which' matrix in the summary object
  featnames <- colnames(reg_summary$which) 
  
  # Filter the selected columns and remove intercept 
  best_featnames <- featnames[reg_summary$which[id_best,]] 
  best_featnames <- best_featnames[-1]
  
  # Add the volat_col if not a feature 
  if (!(volat_col %in% best_featnames)) {
    # add "test" to the end of the list
    best_featnames <- c(best_featnames, volat_col)
  }
  
  # Add the garch_col if not a feature 
  if (!(garch_col %in% best_featnames) & !is.null(garch_col)) {
    # add "test" to the end of the list
    best_featnames <- c(best_featnames, garch_col)
  }
  
  # Construct the best formula for the linear model using the selected variables
  fmla_best <- paste0(target_var, " ~", paste(best_featnames, collapse = " + "))
  
  # Pack into a list to return
  return_feats <- list(featnames = best_featnames, 
                       fmla = as.formula(fmla_best))
  
  return(return_feats)
}


######################
### 4. Other Utils ###
######################

f_unlist_portf_data <- function(portf_stocks_data){
  ## this function changes the format which is a list of outputs 
  ## produced by the SECTOR_PROCEDURE() and into a single portfolio 
  ## list in which names are tickers and values are the xts data for each. 
  
  # Assuming 'nested_list' is your original nested list
  portf_data <- do.call(c, lapply(portf_stocks_data, function(industry) {
    setNames(industry, names(industry))
  }))
  
  # Remove the industry name and dot from each element name
  names(portf_data) <- gsub(".*\\.", "", names(portf_data))
  
  return(portf_data)
}
