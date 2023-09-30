# script to install packages
#source("run_install_packages.R")

# load packages
library("TTR")
library("quantmod")
library("here")

# A few technical indicators functions from TTR
# Calculate Average True Range (ATR)
f_ATR <- function(x)
  ATR(HLC(x))[, 'atr']
  
# Calculate Average Directional Index (ADX)
f_ADX <- function(x)
  ADX(HLC(x))[, 'ADX']
  
# Calculate Aroon Oscillator
f_Aroon <- function(x)
  aroon(cbind(Hi(x), Lo(x)), n = 2)$oscillator
  
# Calculate Bollinger Bands percentage B
f_BB <- function(x)
  BBands(HLC(x))[, "pctB"]
  
# Calculate Chaikin Volatility
f_ChaikinVol <- function(x)
  Delt(chaikinVolatility(cbind(Hi(x), Lo(x))))[, 1]
  
# Calculate Close Location Value (CLV) using Exponential Moving Average (EMA)
f_CLV <- function(x)
  EMA(CLV(HLC(x)))[, 1]
  
# Calculate Ease of Movement (EMV)
f_EMV <- function(x)
  EMV(cbind(Hi(x), Lo(x)), Vo(x))[, 2]
  
# Calculate Moving Average Convergence Divergence (MACD)
f_MACD <- function(x)
  MACD(Cl(x))[, 2]
  
# Calculate Money Flow Index (MFI)
f_MFI <- function(x)
  MFI(HLC(x), Vo(x))
  
# Calculate Parabolic Stop and Reverse (SAR)
f_SAR <- function(x)
  SAR(cbind(Hi(x), Cl(x))) [, 1]
  
# Calculate Stochastic Momentum Index (SMI)
f_SMI <- function(x)
  SMI(HLC(x))[, "SMI"]
  
# Calculate Volatility using Garman-Klass method
f_Volat <- function(x)
  volatility(OHLC(x), calc = "garman")[, 1]
