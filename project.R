getwd()
library(xts)
library(lmtest)
library(tidyverse)
library(urca)


data <- read.csv("NFLX.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data %>% glimpse()
data <- xts(data[, -1],order.by = data$Date,frequency = 12)
Adj.Close <- data$Adj.Close
plot(Adj.Close)
