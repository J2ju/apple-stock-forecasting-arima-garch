# Forecasting Apple Inc. Stock Prices, Volatility and Market Capitalisation

BSc Statistics final-year group thesis · Department of Statistics and Actuarial Science, KNUST (2023)
Authors: Adjei Jude, Asare Boatemaa Rejoice, Ofori Harrison, Owusu Acheampong Bekoe Benjamin · Supervisor: Ms Wilhemina Adoma Pels

## Highlights
- 523 weekly Apple opening prices, 18 March 2013 – 16 March 2023 (Yahoo Finance).
- Box–Cox transformation (λ = −0.267), ADF / KPSS / Phillips–Perron tests, ACF/PACF and information-criteria model search.
- **ARIMA(2,1,2) with drift** selected: lowest AIC and best 20-week hold-out accuracy (RMSE 22.98, MAPE 15.17% vs 24.87 and 16.48% for ARIMA(1,1,1)).
- **sGARCH(1,1)** with Student-t errors chosen for volatility after comparing eGARCH, GJR-GARCH and apARCH; 20-week volatility forecast from 3.1% to 3.6% per week.

## Workflow (`apple_forecasting.R`)
1. Import weekly prices and convert to a weekly time series
2. Rolling variance, 503/20 train–test split, Box–Cox transform
3. Unit-root and seasonality tests, differencing
4. `auto.arima` search and candidate ARIMA models
5. Hold-out forecasts, accuracy and residual diagnostics
6. ARCH-LM test, log returns, mean model
7. sGARCH, eGARCH, GJR-GARCH and apARCH fits with `rugarch`
8. 20-week price and volatility forecasts

## Run it
Put the weekly price file at `data/AAPL.csv` (Date, Open, High, Low, Close, Adj Close, Volume), then run the script in R with these packages:
`tseries, FinTS, lubridate, forecast, readxl, PerformanceAnalytics, rugarch, fpp2, seastests, car`
