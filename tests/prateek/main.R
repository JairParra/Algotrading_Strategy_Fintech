# script to insatll packages
source("run_install_packages.R")

# Load necessary packages
library("zoo")
library("xts")
library("PerformanceAnalytics")

# Read the data from the CSV file named "aapl_and_msft.csv"
df_data <- read.csv("data/aapl_and_msft.csv")

# Extract data for AAPL (Apple Inc.)
df_aapl <- df_data[df_data$tic == "AAPL",]

# Convert the 'datadate' column to a Date object
idx <- as.Date(df_aapl$datadate, format = "%d-%m-%Y")

# Create an xts object (time series) with AAPL data, ordered by date
df_weekly_aapl <- xts(df_aapl, order.by = idx)

# Select only the data corresponding to Wednesdays (weekday == 3)
aapl_wed <- df_weekly_aapl[.indexwday(df_weekly_aapl) == 3]

# Convert the 'prccd' column to numeric
aapl_wed$prccd <- as.numeric(aapl_wed$prccd)

# Calculate logarithmic returns using the 'Return.calculate' function
aapl_wed_rets <- Return.calculate(prices = aapl_wed$prccd,
                                  method = "log")

# Calculate the differences in logarithmic returns (log-returns)
aapl_wed_rets <- diff(log(aapl_wed$prccd))

# Extract the numerical values from the first element of aapl_wed$prccd
aapl_wed$prccd[1] <- as.numeric(aapl_wed$prccd[1])

# Check if the extracted values are numeric
is_numeric <- is.numeric(aapl_wed$prccd[1])

# Print the result
print(is_numeric)
