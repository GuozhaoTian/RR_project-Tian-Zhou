# RR_project-Tian-Zhou
This project aims to reproduce the paper **"ARIMA Model for Accurate Time Series Stocks Forecasting"** by Shakir Khan and Hela Alghulaiakh. 
The orininal paper uses R, so we also using R to reproduce the result.

## Notes
Netflix 5 years stock price data(
[NFLX.csv](https://github.com/GuozhaoTian/RR_project-Tian-Zhou/blob/main/RR%20project/NFLX.csv)
)
R code(
[RRproject.R](https://github.com/GuozhaoTian/RR_project-Tian-Zhou/blob/main/RR%20project/RR%20project.R)
)
Quotra code Documentation(
[RR - Nextflix stock prediction.qmd](https://github.com/GuozhaoTian/RR_project-Tian-Zhou/blob/main/RR%20project/RR%20-%20Nextflix%20stock%20prediction.qmd)
)
Quotra Web Documentation(
[RR - Nextflix stock prediction.html](https://github.com/GuozhaoTian/RR_project-Tian-Zhou/blob/main/RR%20project/RR%20-%20Nextflix%20stock%20prediction.html)
)

## Article Overview
This article introduces how to use the ARIMA model to predict the future stock price trend of Netflix in order to obtain an accurate stock prediction model.
The article does not include the code used by the author. We restore the code required by the article step by step based on the author's description in the article.

## Data
Netflix’s stock price data for a total of five years from April 7, 2015 to April 6, 2020 was downloaded from Yahoo Finance.

## Work Process
1. Create an R project and fix the versions of R studio and R packages.
2. Load the data, preprocess data and keep the required dataset.
3. Draw a 5-year stock price chart and 7, 30, 365-day moving average.
4. Stationary data, remove seasonality
5. Use ACF PACF to test the stationarity of data before and after difference
6. Generate ARIMA models, including 1 automatic ARIMA and 2 customize ARIMA models.
7. Three models are used to make predictions and the results are compared.
8. Summarize article and reproducible research.

## Team Members
Guozhao Tian 444173
Yue Zhou 437844
