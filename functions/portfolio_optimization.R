################################################################################
# portfolio_optimization.R
#   This script contains the logic to perform custom portfolio optimization 
#
# @author: Hair Parra
# @author: Kriti
################################################################################

#############################################
### 1. Data Manipulation for Optimization ###
#############################################

f_extract_ret_fore <- function(data, column_name = "best_shifted_arima") {
  ## Function to extract the xts object for forecasted returns from a list of xts objects
  
  require("xts")
  
  # Extract and merge columns that match the given name across all xts objects in the list
  returns_fore <- do.call(merge, lapply(data, function(x) x[, grep(column_name, colnames(x)), drop = FALSE]))
  
  # Set the column names of the merged xts object
  colnames(returns_fore) <- names(data)
  
  # Append the month_index column from the first xts object in the list
  returns_fore <- cbind(returns_fore, data[[1]]$month_index)
  
  # Return the merged xts object
  return(returns_fore)
}


f_extract_vol_fore <- function(data, returns_fore_index,  column_name ="vol_forecast") {
  ## Function to extract the xts object for forecasted volatility from a list of xts objects
  
  require("xts")
  
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

f_cov_matrix <- function(volat_matrix) {
  ## Function to compute the covariance matrix based on filtered volatility
  
  require("Matrix")

  # remove the month_index column
  volat_matrix$month_index <- NULL
  
  # Compute the covariance matrix
  cov_matrix <- crossprod(volat_matrix[1,], volat_matrix[1,]) # this will be tau + 1 if the features are against the correct dates
  
  # Ensure the covariance matrix is positive definite
  cov_matrix <- nearPD(cov_matrix)
  
  # Convert the positive definite covariance matrix to a standard matrix form
  cov_matrix <- as.matrix(cov_matrix$mat)
  
  return(cov_matrix)
}

# Function to compute the mean return for portfolio optimization
f_mean_ret <- function(return_matrix) {
  
  require("Matrix")
  
  # remove the extra column 
  return_matrix$month_index <- NULL
  
  # calculate the mean by column 
  mean_returns <- apply(return_matrix, 2, mean)

  return(mean_returns)
}

##################################
### 3. Portfolio Optimization  ###
##################################

f_optimize_portfolio <- function(top_sector_stocks, min_alloc = 0.05){
  ### Performs portfolio optimization based on the list of xts stocks top_sector_stocks
  ## The min_alloc argument specified minimum portfolio allocation in the min-var portfolio 
  
  # Load required packages
  require("xts")
  require("Matrix")
  require("quadprog")
  require("quantmod")
  require("PerformanceAnalytics")
  
  # Get the data frame for returns
  returns_fore <- f_extract_ret_fore(top_sector_stocks,
                                     column_name = "best_shifted_arima") # forecasted returns with SARIMA
  
  # to check if there are any NA
  if(sum(is.na(returns_fore))){
    returns_fore <- na.omit(returns_fore)
  } 
  
  # obtain indexes of original object 
  returns_fore_index <- index(returns_fore)
  
  # Get the data frame for volatility forecast
  volatility_fore <- f_extract_vol_fore(top_sector_stocks, 
                                        column_name =  "vol_forecast") # forecasted volatility with GARCH
  
  # keep only rows that didn't have NAs
  volatility_fore <- volatility_fore[returns_fore_index] 
  
  # verify that there are no NAs left 
  if(sum(is.na(returns_fore)) > 0 || sum(is.na(volatility_fore)) >0){
    stop("(f_optimize_portfolio) --> NAs found that couldn't be removed. ")
  }
  
  # Minimum returns for portfolio optimization 
  mean_returns <- f_mean_ret(returns_fore)
  
  # Calculate covariance matrix 
  cov_matrix <- f_cov_matrix(volatility_fore)
  
  # ensure dimensions are compatible 
  if(length(mean_returns) != dim(cov_matrix)[1]){
    stop("(f_optimize_portfolio) --> Incompatible dimensions of mean_returns and cov_matrix")
  }
  
  # Perform min-variance portfolio optimization 
  output <- tryCatch({
    quadprog::solve.QP(Dmat = cov_matrix, # covariance matrix to be minimized
                       dvec = mean_returns, # minimum returns
                       Amat = cbind(matrix(rep(1,length(mean_returns)), ncol = 1), diag(length(mean_returns))),
                       bvec = c(1, rep(min_alloc,length(mean_returns))), # sum of weights = 1 & no short selling
                       meq = 1)
  }, error = function(e){
    print("(f_optimize_portfolio) -->error in portfolio optimization, default equally weighted portfolio returned.")
    return(NULL)
  })
  
  # if error in  optimization, return equally weighted porf 
  if (is.null(output)){
    return(rep(1/length(mean_returns), length(mean_returns)))
  }
  
  # Weights allocated
  weights_mv <- output$solution
  names(weights_mv) <- names(mean_returns)
  
  # return optimized weights 
  return(weights_mv)
}
