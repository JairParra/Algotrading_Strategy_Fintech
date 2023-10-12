################################################################################
# portfolio_optimization.R
#   This script contains the logic to perform custom portfolio optimization 
#
# @author: Hair Parra
# @author: Kriti
################################################################################

################
### 0. Setup ###
################


#############################################
### 1. Data Manipulation for Optimization ###
#############################################


f_extract_ret_fore <- function(data, column_name) {
  ## Function to extract the xts object for forecasted returns from a list of xts objects
  
  require("xts")
  
  # Define the column name
  column_name <- column_name
  
  # Extract and merge columns that match the given name across all xts objects in the list
  returns_fore <- do.call(merge, lapply(data, function(x) x[, grep(column_name, colnames(x)), drop = FALSE]))
  
  # Set the column names of the merged xts object
  colnames(returns_fore) <- names(data)
  
  # Append the month_index column from the first xts object in the list
  returns_fore <- cbind(returns_fore, data[[1]]$month_index)
  
  # Return the merged xts object
  return(returns_fore)
}


f_extract_vol_fore <- function(data, column_name) {
  ## Function to extract the xts object for forecasted volatility from a list of xts objects
  
  require("xts")
  
  # Define the column name
  column_name <- column_name
  
  # Extract and merge columns that match the given name across all xts objects in the list
  vol_fore <- do.call(merge, lapply(data, function(x) x[, grep(column_name, colnames(x)), drop = FALSE]))
  
  # Set the column names of the merged xts object
  colnames(vol_fore) <- names(data)
  
  # Append the month_index column from the first xts object in the list
  vol_fore <- cbind(vol_fore, data[[1]]$month_index)
  
  # Return the merged xts object
  return(vol_fore)
}


###############################
### 2. Mean and Covariance  ###
############################### 

f_cov_matrix <- function(volat_matrix, tau) {
  ## Function to compute the covariance matrix based on filtered volatility
  
  require("Matrix")

  # Filter the volatility matrix to include only the rows where month_index equals tau
  filtered_volat <- volat_matrix[volat_matrix$month_index == tau, 1:(ncol(volat_matrix) - 1)]
  
  # Compute the covariance matrix
  cov_matrix <- crossprod(filtered_volat[1,], filtered_volat[1,]) # this will be tau + 1 if the features are against the correct dates
  
  # Ensure the covariance matrix is positive definite
  cov_matrix <- nearPD(cov_matrix)
  
  # Convert the positive definite covariance matrix to a standard matrix form
  cov_matrix <- as.matrix(cov_matrix$mat)
  
  return(cov_matrix)
}

# Function to compute the mean return for portfolio optimization
f_mean_ret <- function(return_matrix, tau) {
  
  require("Matrix")
  
  # Filter the return matrix to include only rows where month_index equals tau and calculate the mean for each column
  mean_filt_returns <- apply(return_matrix[return_matrix$month_index == tau, 1:(ncol(return_matrix) - 1)], 2, mean)

  return(mean_filt_returns)
}

##################################
### 3. Portfolio Optimization  ###
##################################

f_optimize_portfolio <- function(top_sector_stocks){
  
  # Load required packages
  require("xts")
  require("Matrix")
  require("quadprog")
  require("quantmod")
  require("PerformanceAnalytics")
  
  # Get the data frame for returns
  returns_fore <- f_extract_ret_fore(top_sector_stocks, "best_shifted_arima")
  
  # to check if there are any NA
  if(sum(is.na(returns_fore))){
    returns_fore <- na.omit(returns_fore)
  } 
  
  # Get the data frame for volatility forecast
  volatility_fore <- f_extract_vol_fore(top_sector_stocks, "vol_forecast")
  sum(is.na(volatility_fore)) # to check if there are any NA
  
  # Calculate covariance matrix 
  cov_matrix <- f_cov_matrix(volatility_fore, tau)
  
  # Minimum returns for portfolio optimization 
  mean_returns <- f_mean_ret(returns_fore, tau)
  
  # Perform min-variance portfolio optimization 
  output <- quadprog::solve.QP(Dmat = cov_matrix, # covariance matrix to be minimized
                               dvec = mean_returns, # minimum returns
                               Amat = cbind(matrix(rep(1,length(mean_returns)), ncol = 1), diag(length(mean_returns))),
                               bvec = c(1, rep(0.05,length(mean_returns))), # sum of weights = 1 & no short selling
                               meq = 1)
  # Weights allocated
  weights_mv <- output$solution
  names(weights_mv) <- names(mean_returns)
  
  # return optimized weights 
  return(weights_mv)
}
