library(tseries)
library(forecast)
library(xts)

names <- c("veu.us.txt", "vig.us.txt", "xhs.us.txt")

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

# Get a random fund name
for (name in names) {
  formattedName <- gsub(".us.txt", "", name)
  data <- read.csv(paste(dataFolder, name, sep = ""))
  
  # Train-test split
  splitIndex <- floor(nrow(data) * 0.8)
  train <- data[c(1:splitIndex),]
  train <- ts(train$Close)
  
  test <- data[c(splitIndex:nrow(data)),]
  test <- test$Close
  test <- head(test, n = 100)
  
  priceDiff1 <- diff(train, differences = 1)
  
  logReturns <- diff(log(train), lag = 1)
  logReturns <- logReturns[!is.na(logReturns)]
  
  plot(logReturns, type = 'l', main = 'Log Returns')
  
  # Check if time causes a change in the distribution (stationarity)
  # if p-value is less than 0.05, then data is stationary
  print(adf.test(priceDiff1))
  
  # Autocorrelation (correlation between series and its lags)
  # dotted blue lines show 95% significance boundaries
  Acf(train, main = 'Autocorrelation of Price')
  
  # Partial Autocorrelation
  # shows correlation between a variable and its lags that is not explained by previous lags
  Pacf(train, main = 'Partial Autocorrelation of Price')
  
  # Examine differenced price
  plot(priceDiff1, main = 'Differenced Price')
  Acf(priceDiff1, main = 'Autocorrelation of Differenced Price')
  Pacf(priceDiff1, main = 'Partial Autocorrelation of Differenced Price')
  
  arimaModel <- arima(train, order = c(1, 2, 1))
  
  prediction <- predict(arimaModel, n.ahead = length(test))
  predictions <- as.vector(prediction$pred)
  
  rmse <- function(fitted, observed) {
    sqrt(mean((fitted - observed) ^ 2))
  }
  
  error <- rmse(test, predictions)
  
  # Find sell / buy points
  maxIndex <- which.max(predictions)
  predictionsBeforeMax <- predictions[c(1:maxIndex)]
  
  minIndex <- which.min(predictionsBeforeMax)
  predictedProfit <- predictions[maxIndex] - predictions[minIndex]
  actualProfit <- test[maxIndex] - test[minIndex]
  
  
  plot(
    test,
    type ="l",
    col = "black",
    main = "ARIMA ETF Prediction",
    sub = sprintf("fund name: %s, RMSE: %#.1f, predicted profit: %#.1f, actual profit: %#.1f", formattedName, error, predictedProfit, actualProfit),
    xlab = "Date Index",
    ylab = "Price"
  )
  lines(predictions, col = "red")
  
  # Plot Buy Point
  points(minIndex, predictions[minIndex], type="p", pch=3, col="blue")
  
  # Plot Sell point
  points(maxIndex, predictions[maxIndex], type="p", pch=2, col="green")
  
  legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
  legend("topright", c("Buy", "Sell"), pch=c(3, 2),lty=c(0,0), col = c("Blue", "Green"))
  
  
  print(sprintf("RMSE: %#.1f, Predicted Profit: %#.1f, Actual Profit: %#.1f", error, predictedProfit, actualProfit))
  
  print(sprintf("NAME %s", name))
}
