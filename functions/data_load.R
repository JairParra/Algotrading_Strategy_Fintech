################################################################################
# data_load.R
#   This script contains functions to read the raw data from csv files into the 
#   environment. 
#
# @author: Hair Parra
# @author: Patreek
################################################################################

#########################
### 0. Read CSV Files ###
#########################

f_load_csv_from_zip <- function(zip_file_path) {
  ## Function to unzip a file containing .csv datasets and load each into the environment
  ## Objects loaded should be: 
  ##    - data_fama_french 
  ##    - data_financial_ratios
  ##    - data_realized_volatility
  ##    - data_stocks
  
  # Create a temporary directory to unzip the files
  temp_dir <- tempdir()
  
  # Unzip the file
  unzip(zip_file_path, exdir = temp_dir)
  
  # List all the .csv files in the temporary directory
  csv_files <- list.files(temp_dir, pattern = "*.csv")
  
  # Loop through each .csv file and load it into the environment
  for (csv_file in csv_files) {
    # Create a variable name for the dataset
    var_name <- tools::file_path_sans_ext(csv_file)
    
    # Read the .csv file into a data frame
    data_frame <- read.csv(file.path(temp_dir, csv_file))
    
    # Assign the data frame to a variable in the global environment
    assign(var_name, data_frame, envir = .GlobalEnv)
  }
  
  # Return a message indicating successful operation
  return(paste(length(csv_files), "CSV files have been loaded into the environment."))
}

# Needed in the scope of all other functions 
library("here")
f_load_csv_from_zip(here("data", "raw_data.zip"))


f_preload_raw_data <- function(from="2016-01-01", to="2022-12-31"){
  ## Functin to preprocess the raw data from the csv files under raw_data.zip
  ## assumes these files are loaded beforehand 
  
  # load libraries
  require("here")
  
  # Process data stocks 
  data_stocks$Date <- as.Date(data_stocks$Date, format = "%d-%m-%Y")
  
  # Format data_financial_ratios
  data_financial_ratios$Date <- as.Date(data_financial_ratios$Date, format = "%d-%m-%Y")
  data_financial_ratios$adate <- NULL
  data_financial_ratios$qdate <- NULL
  
  # Format data_realized_vol, use 10 days volatility
  data_realized_volatility <- data_realized_volatility[data_realized_volatility$days == 10, ]
  data_realized_volatility$days <- NULL
  colnames(data_realized_volatility)[colnames(data_realized_volatility) == "Volatility"] = "realized_vol"
  data_realized_volatility$Date <- as.Date(data_realized_volatility$Date, format = "%d-%m-%Y")
  
  # Read data_fama-french.csv
  data_fama_french$Date <- as.Date(data_fama_french$Date, format = "%d-%m-%Y")
  
  # Convert raw data to xts
  xts_fama_french <- as.xts(data_fama_french, order.by = data_fama_french$Date)
  xts_financial_ratios <- as.xts(data_financial_ratios, order.by = data_financial_ratios$Date)
  xts_realized_vol <- as.xts(data_realized_volatility, order.by = data_realized_volatility$Date)
  
  # Remove additional columns 
  xts_fama_french$Date <- NULL
  xts_financial_ratios$Date <- NULL
  xts_realized_vol$Date <- NULL
  
  # Additional preprocessing 
  xts_financial_ratios$divyield <- gsub("%", "", xts_financial_ratios$divyield)
  
  # Subset period of interest for data
  xts_fama_french <- xts_fama_french[(index(xts_fama_french) >= from) & (index(xts_fama_french) <= to)]
  xts_financial_ratios <- xts_financial_ratios[(index(xts_financial_ratios) >= from) & (index(xts_financial_ratios) <= to)]
  xts_realized_vol <- xts_realized_vol[(index(xts_realized_vol) >= from) & (index(xts_realized_vol) <= to)]
  
  # Pack into a list to return 
  xts_extra_feats <- list(
    xts_fama_french = xts_fama_french, 
    xts_financial_ratios = xts_financial_ratios, 
    xts_realized_vol = xts_realized_vol
  )
  
  return(xts_extra_feats)
}

# data cleanup 
data_stocks <- NULL
data_realized_volatility <- NULL
data_financial_ratios <- NULL
data_fama_french <- NULL 



