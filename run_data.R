################################################################################
# run_data.R
# 
# @author: Hair Albeiro Parra Barrera
# @author: Xiao Xue 
# @author: Kriti Bhaya
# @author: Prateek 
# 
# This script contains the logic to extract and process the raw data for this 
# project. 
################################################################################


#####################################
### 0. Libraries and Source files ###
#####################################

# packages
library("here") 

source(here("functions", "data_load.R")) # raw data reading + minimal preprocessing
source(here("functions", "technical_indicators.R")) # technical indicators functions

#####################################
### 1. Data Load  ###
#####################################

# Loads the raw data 
f_preload_raw_data()
