################################################################################
# run_install_packages.R
# 
# @author: Hair Albeiro Parra Barrera
# @author: Xiao Xue 
# @author: Kriti Bhaya
# @author: Prateek 
# 
# This R script is designed to load specified packages and install them if they 
# are not already installed. 
################################################################################

################
### 0. Setup ###
################

# load required scripts
library("here") 
source(here("functions", "modelling.R")) 

################
### 0. Setup ###
################

# List of packages to automatically install 
proj_packages <- c(
  "xts", 
  "zoo", 
  "TTR", 
  "here",
  "stats",
  "leaps",
  "rvest", 
  "dplyr", 
  "caret", 
  "glmnet", 
  "Metrics", 
  "quadprog",
  "quantmod", 
  "forecast", 
  "data.table", 
  "tidyverse", 
  "tidyquant", 
  "PerformanceAnalytics"
)

####################
### 1. Functions ###
####################

# Load required packages
f_load_packages <- function(package_list) {
  for (package in package_list) {
    # Check if the package is already installed
    if (!requireNamespace(package, quietly = TRUE)) {
      # If not installed, install the package with dependencies
      install.packages(package, dependencies = TRUE)
    }
    # Load the package
    library(package, character.only = TRUE)
  }
}

###############
### 2. Main ###
###############

# Call the function to load packages
f_load_packages(proj_packages)
