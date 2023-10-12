

### Functions to extract data frames ### 

# Extract the xts for the returns
f_extract_ret_fore <- function(data, column_name) {
  column_name <- column_name
  returns_fore <- do.call(merge, lapply(data, function(x) x[, grep(column_name, colnames(x)), drop = FALSE]))
  colnames(returns_fore) <- names(data)
  returns_fore <- cbind(returns_fore, data[[1]]$month_index)
  return(returns_fore)
}

# Extract the xts for the volatility forecaset
f_extract_vol_fore <- function(data, column_name) {
  column_name <- column_name
  vol_fore <- do.call(merge, lapply(data, function(x) x[, grep(column_name, colnames(x)), drop = FALSE]))
  colnames(vol_fore) <- names(data)
  vol_fore <- cbind(vol_fore, data[[1]]$month_index)
  return(vol_fore)
}


### Functions for Portfolio optimization ###

f_cov_matrix <- function(volat_matrix, tau) {
  # Filter just the volatility for the period
  filtered_volat <- volat_matrix[volat_matrix$month_index == tau, 1:(ncol(volat_matrix) - 1)]
  # Get the covariance matrix
  cov_matrix <- crossprod(filtered_volat[1,], filtered_volat[1,]) # this will be tau + 1 if the features are against the correct dates
  cov_matrix <- nearPD(cov_matrix) # To get a positive definite covariance matrix
  cov_matrix <- as.matrix(cov_matrix$mat) # Convert the solution to a matrix form 
}

f_mean_ret <- function(return_matrix, tau) {
  # Get the mean returns to be used for the portfolio optimization
  mean_filt_returns <- apply(return_matrix[return_matrix$month_index == tau, 1:(ncol(return_matrix) - 1)], 2, mean)
}
