################################################################################
# data_load.R
#   This script contains functions to read the raw data from csv files into the 
#   environment. 
#
# @author: Hair Parra
################################################################################

###########################
### 0. Initial Features ###
###########################

# Function to unzip a file containing .csv datasets and load each into the environment
load_csv_from_zip <- function(zip_file_path) {
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

# Example usage
library("here")
load_csv_from_zip(here("data", "raw_data.zip"))


