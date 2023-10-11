# data 

For this project we used Wharton Research Data Services (WRDS) to extract our data.

### Stock Price Data

The stock price data is contained in the "data_stocks.CSV" file sourced from `'Compustat Capital IQ'`. The features directly obtained were `['Open', 'Close', 'High', 'Low', 'EPS', 'Volume]`, although there was additional preprocessing involved. 

- Script and logic used 
- Additional explanation 1
- Additional explanation 2 

### Financial Ratios Data

The financial ratios data is contained in the "data_financial_ratios.CSV" file sourced from `'Compustat Capital IQ'`. The features directly obtained were `['Book to Market Ratio', 'Price to Sales Ratio', 'Return on Equity', 'Debt to Equity', 'Quick Ratio', 'Current Ratio', 'Price to Book', 'Dividend Yield']`

### Implied volatility

The implied volatility data is contained in the "data_relaized_volatility.CSV" file, sourced from `'OptionMetrics'`. The features directly obtained were `['10-day Implied Volatility']`.

### Fama-French factors

The fama french factors data is contained in the "data_fama_french.CSV" file, sourced from `'Kenneth French Database'`. The features directly obtained were `['Excess Market Return', 'Small minus Big', 'High minus Low', 'Robust minus Weak', 'Conservative minus Aggressive', 'Risk free rate', 'Momentum']`
