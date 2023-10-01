################################################################################
# feature_engineering.R
#   This script contains functions related to data manipulation and 
#   feature engineering. 
#
# @author: Hair Parra
################################################################################

###########################
### 0. Initial Features ###
###########################


##########################
### 1. Static Features ###
##########################

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


# function that extracts the static (no-changing) features from a matrix of features 
f_extract_train_val_features <- function (stock_data, tau = NULL, n_months = 12, val_lag = 2){
  ## 
  ## Params: 
  ##  - stock_data (xts): An xts object contianing the features and target for the stock 
  ##  - tau (int): the current run number used to split the data 
  ##  - n_months (int): the number of months to consider for the window 
  ##  - val_lag (int): Number of months to consider for the validation set 
  
  # Calculate the beginning and end of the current window 
  t_start <- tau;
  t_end <- tau + n_months - 1 
  t_val <- t_end - val_lag
  
  # Subset the appropriate train and test sets for that stock 
  train_sub = stock_data[(stock_data$month_index >= t_start) & (stock_data$month_index < t_val)] 
  val_sub = stock_data[(stock_data$month_index >= t_val) & (stock_data$month_index <= t_end)] 
  
  
  stock_train_val <- list(train = train_sub,
                          val = val_sub
  )
  
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
  stock_xts_no_na <- na.omit(stock_xts)
  
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
# Define the function
f_add_arima_forecast <- function(stock_data, return_col) {
  ## Computes in-sample and OOS ARIMA(p,d,q) forecasts on the input return_col from the data. 
  ## All the combinations for p,d,q = {0,1} are computed and added.
  
  require("xts")
  require("forecast")
  
  # Validate input
  if (!is.xts(stock_data) || !(return_col %in% colnames(stock_data))) {
    stop("Invalid input: stock_data must be an xts object and return_col must be a column in stock_data")
  }
  
  # Remove NA values from the specified column
  stock_data_no_na <- na.omit(stock_data[, return_col, drop = FALSE])
  
  # Calculate the number of steps to forecast based on the number of weeks 
  # of last month + 1 
  num_pred_weeks <- nrow(stock_data[stock_data$month_index == tail(as.vector(stock_data[, "month_index"]),1), ])

  # Convert the xts column to a basic time series object
  ts_data <- as.ts(coredata(stock_data_no_na[, 1]))
  
  # Generate a list of all combinations of p, d, q (0 or 1)
  pdq_combinations <- expand.grid(p = 0:1, d = 0:2, q = 0, P=0, D=0:1, Q=1)
  
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
    fit <- Arima(ts_data, order = c(p,d,q), seasonal = c(P,D,Q))
    
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
    names(shifted_arima_xts) <- c(paste0("arima_", p, d, q, "_", P, D, Q, collapse=""))
    
    # concatenate result to original dataframe 
    stock_data <- cbind.xts(stock_data, shifted_arima_xts)
    
  }
  
  # Return the modified xts object
  return(stock_data)
}

f_extract_dynamic_features <- function(stock_data, 
                                       return_col="realized_returns", 
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
  stock_data <- f_add_arima_forecast(stock_data, return_col=return_col)
  
  # Compute the GARCH features
  stock_data <- f_add_garch_forecast(stock_data, volat_col=volat_col) 
  
  return(stock_data)
}

######################
### 3. Other Utils ###
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
