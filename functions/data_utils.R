################################################################################
# data_utils.R
#   Implements utils to transform data, 
#
# @author: Hair Parra
################################################################################

# Function to count months and assign index
assign_int_month_index <- function(xts_data) {
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