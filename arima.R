library(tseries)
library(forecast)
library(xts)

names <- c("ayt.us.txt", "bil.us.txt", "chie.us.txt",  "cefl.us.txt", "epu.us.txt", "fan.us.txt", "gal.us.txt", "veu.us.txt", "vig.us.txt", "xhs.us.txt")

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

colClasses = c("character", "numeric", "numeric", "numeric")
col.names = c("name", "rmse", "predictedProfit", "actualProfit")

results <- read.table(
  text = "",
  colClasses = colClasses,
  col.names = col.names,
  stringsAsFactors = FALSE
)

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
  png(filename = sprintf("plots/arima/acf_%s.png", formattedName))
  Acf(train, main = 'Autocorrelation of Price')
  dev.off()
  
  # Partial Autocorrelation
  # shows correlation between a variable and its lags that is not explained by previous lags
  png(filename = sprintf("plots/arima/pacf_%s.png", formattedName))
  Pacf(train, main = 'Partial Autocorrelation of Price')
  dev.off()
  
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
  
  png(filename = sprintf("plots/arima/arimaPrediction_%s.png", formattedName))
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
  dev.off()
  
  
  print(sprintf("Name: %s, RMSE: %#.1f, Predicted Profit: %#.1f, Actual Profit: %#.1f", formattedName, error, predictedProfit, actualProfit))
  
  results <- rbind(results, data.frame(name = formattedName, error, predictedProfit, actualProfit, absDistanceProfit = abs(predictedProfit - actualProfit)))
}

print("Results:")
print(results)

resultsNumeric <- results[, !(names(results) %in% c("name"))]
means <- colMeans(resultsNumeric)
finalResults <- data.frame(as.list(means))
rownames(finalResults) <- "arima"

print("Mean Results:")
print(finalResults)

write.table(finalResults, "results.csv", sep = ",", col.names = FALSE, append = TRUE)