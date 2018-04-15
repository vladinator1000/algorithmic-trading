library(timeSeries)
library(forecast)
library(xts)

set.seed(42)

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

# Get a random fund name
name <- sample(fileList, 1)
data <- read.csv(paste(dataFolder, name, sep = ""))
price <- xts(data$Close, as.Date(data$Date))

stock <- diff(log(price), lag = 1)
stock <- stock[!is.na(stock)]

plot(stock, type = 'l', main = 'Log Returns')

# Check if time causes a change in the distribution (stationarity)
# if p-value is less than 0.05, then data is stationary
print(adf.test(stock))

