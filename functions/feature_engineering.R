################################################################################
# feature_engineering.R
#   This script contains functions related to data manipulation and 
#   feature engineering. 
#
# @author: Hair Parra
################################################################################


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
f_extract_train_val_features <- function (stock_data, tau = NULL){
  
  # Calculate the beginning and end of the current window 
  t1 = tau;
  t11 = tau + 10
  t12 = tau + 11 
  
  # Subset the appropriate train and test sets for that stock 
  train_sub = stock_data[(stock_data$month_index >= t1) & (stock_data$month_index <= t11)] 
  val_sub = stock_data[stock_data$month_index >= t12]
  
  
  stock_train_val <- list(train = train_sub,
                          val = val_sub
  )
  
  return(stock_train_val)
}  

###########################
### 2. Dynamic Features ###
###########################


f_add_garch_forecast <- function(x, volat_col="volat") {
  ## Computes the GARCH(1,1) on the data, procudes in-sample and out-of-sample forecasts, 
  ## and assigns a shifted forecasted volatility vector as a feature to the data. 
  
  # specify packages 
  require("xts") 
  require("rugarch")
  
  # Check if x is an xts object with a column named 'volat'
  if (!is.xts(x) || !(volat_col %in% colnames(x))) {
    stop("Invalid input: x must be an xts object with a column named 'volat'")
  }
  
  # Remove any rows that contain NA values in 'volat'
  x_no_na <- na.omit(x)
  
  # Fit a GARCH(1,1) model to the 'volat' column
  spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                           garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), 
                                       include.mean = FALSE))
  fit <- ugarchfit(spec, data = x_no_na[, volat_col], solver = "hybrid")
  
  # Predict the volatility for the in-sample data and the next 4 steps
  pred <- ugarchforecast(fit, n.ahead = 4)
  
  # Extract the predicted volatility as a vector
  sigma <- as.vector(pred@forecast$sigmaFor)
  
  # Construct shifted forecasted volatility vector 
  shifted_sigma <- c(as.vector(x[, volat_col])[5:dim(x)[1]], sigma)
  
  # Assign the forecasted volatility to the original xts object as a new column, shifted by 4 steps
  x$vol_forecast <- shifted_sigma
  
  # Return the modified xts object
  return(x)
}



# Define the function
f_add_arima_features <- function(stock_data, return_col) {
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
  
  # Convert the xts column to a basic time series object
  ts_data <- as.ts(stock_data_no_na[, 1])
  
  # Generate a list of all combinations of p, d, q (0 or 1)
  pdq_combinations <- expand.grid(p = 0:1, d = 0:1, q = 0:1)
  
  # Loop through the list of pdq combinations
  
  for (i in 1:nrow(pdq_combinations)) {
    # extract parameters
    p <- pdq_combinations[i, "p"]
    d <- pdq_combinations[i, "d"]
    q <- pdq_combinations[i, "q"]
    
    # fit an arima model 
    fit <- Arima(ts_data, order=c(p,d,q))
    
    # Forecast the next 4 steps
    pred <- forecast(fit, h = 4)
    
    # Extract the forecasted values as a vector
    arima_forecast <- as.vector(pred$mean)
    
    # create the name for the new feature
    col_name <- paste("ret_arima_", p, d, q, sep = "")
    
    # Construct shifted forecasted return vector 
    shifted_arima <- c(as.vector(stock_data[, return_col])[5:dim(stock_data)[1]], arima_forecast)
    
    # create an xts with the original index 
    shifted_arima_xts <- xts(shifted_arima, order.by = index(stock_data), )
    names(shifted_arima_xts) <- c(col_name)
    
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
  stock_data <- f_add_arima_features(stock_data, return_col=return_col)
  
  # Compute the GARCH features
  stock_data <- f_add_garch_forecast(stock_data, volat_col=volat_col) 
  
  return(stock_data)
}