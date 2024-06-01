library(xts)
library(lmtest)
library(tidyverse)
library(urca)


data <- read.csv("NFLX.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data %>% glimpse()
data <- xts(data[, -1],order.by = data$Date,frequency = 12)
Adj.Close <- data$Adj.Close
# Plot original Adjusted Close prices
plot(Adj.Close, main = "NFLX Adjusted Close Prices", ylab = "Price", xlab = "Date")

# Calculate weekly moving average (K=7)
weekly_ma <- rollmean(Adj.Close, k = 7, fill = NA)

# Calculate monthly moving average (K=30, approximating 1 month)
monthly_ma <- rollmean(Adj.Close, k = 30, fill = NA)

# Calculate yearly moving average (K=365, approximating 1 year)
yearly_ma <- rollmean(Adj.Close, k = 365, fill = NA)

# Plot all moving averages
plot(Adj.Close, main = "NFLX Adjusted Close Prices with Moving Averages", ylab = "Price", xlab = "Date")
lines(weekly_ma, col = "blue", lwd = 2)
lines(monthly_ma, col = "red", lwd = 2)
lines(yearly_ma, col = "green", lwd = 2)
legend("topright", legend = c("Weekly MA", "Monthly MA", "Yearly MA"), col = c("blue", "red", "green"), lwd = 2)

