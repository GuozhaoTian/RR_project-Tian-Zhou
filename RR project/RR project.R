library(xts)
library(lmtest)
library(tidyverse)
library(urca)
library(forecast)

# Read and prepare data
data <- read.csv("NFLX.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
str(data)

data <- xts(data[, -1],order.by = data$Date,frequency = 12)

# Extract and work with Adjusted Close prices
Adj.Close <- data$Adj.Close

# Plot original Adjusted Close prices
plot(Adj.Close, main = "NFLX Adjusted Close Prices", ylab = "Price", xlab = "Date", col = "sky blue")

# Calculate weekly moving average (K=7)
weekly_ma <- rollmean(Adj.Close, k = 7, fill = NA)

# Calculate monthly moving average (K=30, approximating 1 month)
monthly_ma <- rollmean(Adj.Close, k = 30, fill = NA)

# Calculate yearly moving average (K=365, approximating 1 year)
yearly_ma <- rollmean(Adj.Close, k = 365, fill = NA)

# Plot all moving averages
plot(Adj.Close, main = "NFLX Adjusted Close Prices with Moving Averages", ylab = "Price", xlab = "Date", col = "red")
lines(weekly_ma, col = "green", lwd = 2)
lines(monthly_ma, col = "sky blue", lwd = 2)
lines(yearly_ma, col = "purple", lwd = 2)
legend("topright", legend = c("Weekly MA", "Monthly MA", "Yearly MA"), col = c("green", "sky blue", "purple"), lwd = 2)

# Decomposition of the time series
Adj.Close_ts <- as.ts(Adj.Close)
Adj.Close_ts <- ts(coredata(Adj.Close), frequency = 12)
decomp <- decompose(Adj.Close_ts)

# Plotting decompose results
par(mfrow = c(4, 1), mar = c(4, 4, 1, 1))  # Setting up the plotting area
plot(decomp$x, main = "Original Data", ylab = "Data")
plot(decomp$seasonal, main = "Seasonal Component", ylab = "Seasonal")
plot(decomp$trend, main = "Trend Component", ylab = "Trend")
plot(decomp$random, main = "Random Component", ylab = "Remainder")
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1)) 


# Set up the plotting area to have 2 rows and 1 column
par(mfrow = c(2, 1)) 
# Set smaller margins
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)
# Plot your ACF and PACF with adjusted margins
acf(Adj.Close, lag.max = 60, lwd = 5, col = "grey", na.action = na.pass)
pacf(Adj.Close,lag.max = 36, lwd = 5, col = "black",na.action = na.pass)
# Reset the plotting area to the default
par(mfrow = c(1, 1)) 

#The Augmented Dickey-Fuller (ADF) test for unit roots in a time series.
adf_test <- ur.df(Adj.Close, type = "drift", selectlags = "AIC")
summary(adf_test)


# Difference the data and remove NAs
dadj <- na.exclude(diff.xts(Adj.Close))

# Set up the plotting area to have 2 rows and 1 column
par(mfrow = c(2, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)
# Plot the ACF
acf(dadj,lag.max = 60, lwd = 5,col = "dark green",na.action = na.pass) 
# Plot the PACF
pacf(dadj,lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
# Reset the plotting area to the default
par(mfrow = c(1, 1)) 

# Perform the ADF test on the differenced data
adf_test_dadj <- ur.df(dadj, type = "drift", selectlags = "AIC")
summary(adf_test_dadj)


# Automatically fit ARIMA model
auto.arima(Adj.Close, seasonal = TRUE)
# Subset data up to a specific date
cdata.shortx1 <- Adj.Close["/20171007", ]
# Fit ARIMA(2, 1, 2) model
arima212 <- Arima(cdata.shortx1,order = c(2, 1, 2))
# Coefficients and summary
summary(arima212)

# Set up the plotting area to have 3 rows and 1 column
par(mfrow = c(3, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)

# Plot residuals
plot(resid(arima212))
# ACF and PACF of residuals
acf(resid(arima212),lag.max = 60, ylim = c(-0.1, 0.1), main = "ACF", lwd = 2, col = "dark green",na.action = na.pass)
pacf(resid(arima212),lag.max = 36, main = "PACF", lwd = 2, col = "dark green",na.action = na.pass)
# Reset the plotting area to the default
par(mfrow = c(1, 1))
# Ljung-Box test
# The Box.test checks for the presence of autocorrelation in residuals. 
Box.test(resid(arima212), type = "Ljung-Box", lag = 3)


# Fit ARIMA(1, 1, 33) model
arima1133 <- Arima(cdata.shortx1,order = c(1, 1,33))
# Coefficient test and summary
summary(arima1133)

# Set up the plotting area to have 3 rows and 1 column
par(mfrow = c(3, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)

# Plot residuals
plot(resid(arima1133))
# ACF and PACF of residuals
acf(resid(arima1133),lag.max = 60, main = "ACF", ylim = c(-0.1, 0.1),lwd = 2, col = "dark green",na.action = na.pass)
pacf(resid(arima1133),lag.max = 36, main = "PACF", lwd = 2, col = "dark green",na.action = na.pass)
# Reset the plotting area to the default
par(mfrow = c(1, 1))
# Ljung-Box test
# The Box.test checks for the presence of autocorrelation in residuals. 
Box.test(resid(arima1133), type = "Ljung-Box", lag = 3)


# Fit ARIMA(1, 2, 33) model
arima1233 <- Arima(cdata.shortx1$Adj.Close,order = c(1, 2,33))
# Coefficient test and summary
summary(arima1233)

# Set up the plotting area to have 3 rows and 1 column
par(mfrow = c(3, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)
# Plot residuals
plot(resid(arima1233))
# ACF and PACF of residuals
acf(resid(arima1233),lag.max = 60, main = "ACF", ylim = c(-0.1, 0.1), lwd = 2, col = "dark green",na.action = na.pass)
pacf(resid(arima1233),lag.max = 36, main = "PACF", lwd = 2, col = "dark green",na.action = na.pass)
# Reset the plotting area to the default
par(mfrow = c(1, 1))
# Ljung-Box test
# The Box.test checks for the presence of autocorrelation in residuals. 
Box.test(resid(arima1233), type = "Ljung-Box", lag = 3)


# Generate forecasts,627 future values are forecasted.
forecasts <- forecast(arima212, h = 627) 

# Extract forecasted mean
forecasts
forecasts$mean
# Check class and convert to numeric
class(forecasts$mean)
as.numeric(forecasts$mean)
# Extract forecast confidence intervals
forecasts$lower
forecasts$upper
# Create data frame for forecast data
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))
# Display the forecast data
forecasts_data 


# Subset data up to a specific date
cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
cdata.shortx1
# Check the tail of the subset data
tail(cdata.shortx1)
# Subset data from a specific date
cdata.shortx2 <-Adj.Close$Adj.Close["20171007/", ]
# Convert forecast data to xts object
forecasts_xts <- xts(forecasts_data,order.by =index(cdata.shortx2))
# Merge original data with forecast data
cdata3 <- merge(Adj.Close,forecasts_xts)
# Display the merged data
head(cdata3)
tail(cdata3, n =627)
# Plot the merged data
plot(cdata3, main = "Original Data and Forecasts", col = c("black", "blue", "red", "green"))


# Generate forecasts,627 future values are forecasted.
forecasts <- forecast(arima1133, h = 627) 
forecasts
# Extract forecasted mean
forecasts$mean
# Check class and convert to numeric
class(forecasts$mean)
as.numeric(forecasts$mean)
# Extract forecast confidence intervals
forecasts$lower
forecasts$upper
# Create data frame for forecast data
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))

# Display the forecast data
forecasts_data 


# Subset data up to a specific date
cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
cdata.shortx1
# Check the tail of the subset data
tail(cdata.shortx1)
# Subset data from a specific date
cdata.shortx2 <-Adj.Close$Adj.Close["20171007/", ]
# Convert forecast data to xts object
forecasts_xts <- xts(forecasts_data,order.by =index(cdata.shortx2))
# Merge original data with forecast data
cdata3 <- merge(Adj.Close,forecasts_xts)
# Display the merged data
head(cdata3)
tail(cdata3, n =627)
# Plot the merged data
plot(cdata3, main = "Original Data and Forecasts", col = c("black", "blue", "red", "green"))

forecasts <- forecast(arima1233, h = 627) 
forecasts
# Extract forecasted mean
forecasts$mean
# Check class and convert to numeric
class(forecasts$mean)
as.numeric(forecasts$mean)
# Extract forecast confidence intervals
forecasts$lower
forecasts$upper
# Create data frame for forecast data
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))
# Display the forecast data
forecasts_data 


# Subset data up to a specific date
cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
cdata.shortx1
# Check the tail of the subset data
tail(cdata.shortx1)
# Subset data from a specific date
cdata.shortx2 <-Adj.Close$Adj.Close["20171007/", ]
# Convert forecast data to xts object
forecasts_xts <- xts(forecasts_data,order.by =index(cdata.shortx2))
# Merge original data with forecast data
cdata3 <- merge(Adj.Close,forecasts_xts)
# Display the merged data
head(cdata3)
tail(cdata3, n =627)
# Plot the merged data
plot(cdata3, main = "Original Data and Forecasts", col = c("black", "blue", "red", "green"))
