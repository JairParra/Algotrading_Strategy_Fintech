# Data Collection and Sources

For this project, our primary data repository was the Wharton Research Data Services (WRDS).

## **Stock Price Information**

**Source:** Compustat Capital IQ, Yahoo! Finance

**File:** `data_stocks.CSV`

### **Features Extracted:**

- **Open Price:** The initial trading price of a stock when the market opens.
- **Close Price:** The final trading price of a stock when the market closes.
- **Adjusted Close Price:** Close price adjusted for dividends and stock splits.
- **High Price of the Day:** Highest traded price during a single trading day.
- **Low Price of the Day:** Lowest traded price during a single trading day.
- **Earnings Per Share (EPS):** Profit allocated per outstanding share of a company.
- **Trade Volume:** Number of shares traded during a specific time period.

**Notes:**

- The data underwent additional preprocessing before analysis.
- Script and logic were employed to clean and format the data.

## **Financial Ratios Overview**

**Source:** Compustat Capital IQ

**File:** `data_financial_ratios.CSV`

### **Features Extracted:**

- **Price to Earnings Ratio (P/E):** Measures company's stock price relative to its earnings.
- **Book to Market Ratio:** Gauge of company valuation relative to market price.
- **Price to Sales Ratio:** Stock value in relation to sales.
- **Return on Equity:** Measure of profitability with respect to shareholders' equity.
- **Debt to Equity Ratio:** Measure of a company's financial leverage.
- **Quick Ratio:** Indicator of a company's short-term liquidity.
- **Current Ratio:** Broader measure of a company's liquidity.
- **Price to Book Value:** Market valuation relative to the company's book value.
- **Dividend Yield:** Return on investment via dividends for income-focused investors.

## **Historical Volatility Details**

**Source:** OptionMetrics

**File:** `data_realized_volatility.CSV`

### **Features Extracted:**

- **10-day Historical Volatility:** Volatility measured over a 10-day window, calculated using daily returns.

## **Fama-French Factors**

**Source:** Kenneth French Database

**File:** `data_fama_french.CSV`

### **Features Extracted:**

- **Excess Market Return:** Market return over and above the risk-free rate.
- **Size Factor (Small minus Big):** Captures the returns due to firm size.
- **Value Factor (High minus Low):** Captures the returns due to company valuation.
- **Operating Profitability (Robust minus Weak):** Measures the profitability of a company.
- **Investment Patterns (Conservative minus Aggressive):** Captures the company's investment behavior.
- **Risk-Free Rate:** The rate of return of a risk-free asset, usually a government bond.
- **Momentum Factor:** Captures the tendency of stocks to exhibit persistence in their performance.
