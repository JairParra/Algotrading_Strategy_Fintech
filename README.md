# Algotrading_Strategy_Fintech
The objective is to create a realistic investment strategy based on several machine learning techniques seen during the masters course *Machine Learning Applied to Financial Data*, at HEC Montréal.

## Dataset 

Describe the: 
- **Raw data:** desc
    - **Preprocessing steps:** 
        1. Step 1 
        2. Step 2 

- **Clean data** desc

## Simulation Assumptions 

TODO 

# Workflow

## Entrypoint(s) 

Should be the SECTOR_PROCEDURE 
-> calls  

## Stategy description 

General description. 

### Workflow: 
-> 


### MODELLING_PROCEDURE

$$
N^{runs} = \left\lfloor 
    \dfrac{N_{months} - N_W}{s} 
\right\rfloor
+ 1
$$

$$
\mathcal{L}(\beta) 
= 
\dfrac{1}{2}
\sum_{i=1}^{n}(y_i - x_{i}^T\beta)^{2}
+ 
\lambda\left[
  \alpha ||\beta||_1 
  + (1-\alpha)||\beta||_{2}^{2}
\right]
$$

1. Sector $G$ contains tickers $\{S_1,S_1,\dots,  S_{|G|}\}$, where $|G|$= number of stocks per sector (before selection). 
2. For each ticker, want to calculate **current window:**

$$
\left[
  t_1 = \text{week } W_{s\times\tau}
  \;,\;
  t_{12} = \text{week } W_{s\times\tau + 11}
\right]
$$ 

e.g. with $s=1$ (slide one month at the time)

$$
\begin{cases}
\tau = 1 \implies [t_1 = W_{1} \;,\; t_{12} = W_{12}] \\ 
\tau = 2 \implies [t_1 = W_{2} \;,\; t_{12} = W_{13}] \\ 
\vdots \\ 
\tau = i \implies [t_1 = W_{i} \;,\; t_{12} = W_{i+11}] \\ 
\vdots \\ 
\tau = T \implies [t_1 = W_{T-12} \;,\; t_{12} = W_{T}]
\end{cases}
$$


# OLD (TO DELETE)

## Dataset 

Description of our dataset 

### Features 

**Basic Features**
- Open, Close, High, Low 
- Bid & Ask 
- Volume 
- P/E Ratio
- Beta 
- NAV 
- Adjusted Close 

**Technical Indicators:** 
- Exponential Moving Averages (EMA): With 7, 14, 30 days
- Relative Strength Index (RSI)
- Chaikin Oscillator
- Average True Range (ATR): Indicator of the volatility of the series
- Stochastic Momentum Index (SMI): Momentum indicator
- Welles Wilder’s Directional Movement Index (ADX)
- Aroon indicator: Tries to identify starting trends
- Bollinger Bands: Compare the volatility over a period of time
- Close Location Value (CLV): Relates the session close to its trading range
- Arms’s Ease of Movement Value (EMV)
- MACD oscillator
- Money Flow Index (MFI)
- Parabolic Stop-and-Reverse
- Volatility indicator


**Lagging Indicators** 
- Lag 0, 1, 2, 3 returns 
- Lag 0, 1, 2, 3, price

**ARIMA(p,d, q) features**
- Add all combinations of p=0,1 (e.g. ARIMA(0,1,1), ARIMA(1,1,1) etc.)
- Predict the next tick return 

**GARCH(1,1)** 
- Fit a GARCH(1,1) model and predict the volatility for the next tick 


## Strategy 

1. Choose **5 economics sectors**, and 10 stocks per each
2. Obtain the OCLH data, adjusted close, volume, etc for these indicators
3. Obtain **features** for all stocks
4. For each of the sectors:
    a. Train a **regularized MLR** for all the ETFs, stocks in that sector:
        - Multiple Linear Regression 
        - K-fold Cross-Validation (On Modified Sharpe Ratio!)
        - Elastic Net Regularization
    b. Predict the return for each stock 
    c. Pick the top 3 stocks with best predicted return 
    d. Add these to the stocks to the list of stocks to use to create the portfolio 
5. Create the portfolio out of the stocks selected in **Step 4** 
    - List all the stocks and the predicted returns 
    - Assign a proportional weight to each stock based on the forecasted expected return 
6. Every month: 
    - Close all your positions, if any, by simply selling the stocks. 
    - Run the algorithm at the beginning, using the data from last month. 
    - Forecast for the week ahead and pick the relevant stocks 
    - Rebalance the portfolio using **step 5**
