library(xts)
library(lmtest)
library(tidyverse)
library(urca)

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



adf_test <- ur.df(Adj.Close$Adj.Close, type = "none", lags = 0)
summary(adf_test)

par(mfrow = c(2, 1)) 
# Set smaller margins
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)

# Plot your ACF with adjusted margins
acf(Adj.Close$Adj.Close, lag.max = 60, ylim = c(0.0, 1), lwd = 5, col = "grey", na.action = na.pass)

pacf(Adj.Close$Adj.Close,lag.max = 36,lwd = 5, col = "black",na.action = na.pass)
par(mfrow = c(1, 1)) 


dadj <- na.exclude(diff.xts(Adj.Close$Adj.Close))
adf_test_dadj <- ur.df(dadj, type = "none", lags = 0)
summary(adf_test_dadj)

par(mfrow = c(2, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)
acf(dadj,lag.max = 60, ylim = c(0.0, 0.1),lwd = 5,col = "dark green",na.action = na.pass)  
pacf(dadj,lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1)) 


library(forecast)
auto.arima(Adj.Close)
cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
arima212 <- Arima(cdata.shortx1$Adj.Close,order = c(2, 1, 2))
arima212
coeftest(arima212)
summary(arima212)
plot(resid(arima212))
tibble(date = index(cdata.shortx1$Adj.Close),resid = arima212 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +theme_bw()


par(mfrow = c(2, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)
acf(resid(arima212),lag.max = 60,ylim = c(-0.1, 0.1),lwd = 5, col = "dark green",na.action = na.pass)
pacf(resid(arima212),lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima212), type = "Ljung-Box", lag = 4)

arima1133 <- Arima(cdata.shortx1$Adj.Close,order = c(1, 1,33))
arima1133
coeftest(arima1133)
summary(arima1133)
plot(resid(arima1133))
tibble(date = index(cdata.shortx1$Adj.Close),resid = arima1133 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +theme_bw()


par(mfrow = c(2, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)
acf(resid(arima1133),lag.max = 60,ylim = c(-0.1, 0.1),lwd = 5, col = "dark green",na.action = na.pass)
pacf(resid(arima1133),lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima1133), type = "Ljung-Box", lag = 4)



arima1233 <- Arima(cdata.shortx1$Adj.Close,order = c(1, 2,33))
arima1233
coeftest(arima1233)
summary(arima1233)
plot(resid(arima1233))
tibble(date = index(cdata.shortx1$Adj.Close),resid = arima1233 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +theme_bw()


par(mfrow = c(2, 1)) 
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)
acf(resid(arima1233),lag.max = 60,ylim = c(-0.1, 0.1),lwd = 5, col = "dark green",na.action = na.pass)
pacf(resid(arima1233),lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima1233), type = "Ljung-Box", lag = 4)