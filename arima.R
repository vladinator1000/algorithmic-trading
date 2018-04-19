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
priceDiff1 <- diff(price, differences = 1)


logReturns <- diff(log(price), lag = 1)
logReturns <- logReturns[!is.na(logReturns)]

plot(logReturns, type = 'l', main = 'Log Returns')

# Check if time causes a change in the distribution (stationarity)
# if p-value is less than 0.05, then data is stationary
print(adf.test(logReturns))

# Autocorrelation (correlation between series and its lags)
# dotted blue lines show 95% significance boundaries
Acf(price, main = 'Autocorrelation of Price')

# Partial Autocorrelation
# shows correlation between a variable and its lags that is not explained by previous lags
Pacf(price, main = 'Partial Autocorrelation of Price')

# Examine differenced price
plot(priceDiff1, main = 'Differenced Rrice')
Acf(priceDiff1, main = 'Autocorrelation of Differenced Price')
Pacf(priceDiff1, main = 'Autocorrelation of Differences Price')

