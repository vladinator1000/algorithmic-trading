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
  dataFolder <- file.path(getwd(), "data", "/")
  fileList <- list.files(path = dataFolder)

  formattedName <- gsub(".us.txt", "", name)
  path <- paste(dataFolder, name, sep = "")
  data <- read.csv(path)
  
  # Train-test split
  splitIndex <- floor(nrow(data) * 0.8)
  train <- data[c(1:splitIndex),]
  train <- train$Close
  
  test <- data[c(splitIndex:nrow(data)),]
  test <- test$Close
  test <- head(test, n = 100)
  
  fit <- HoltWinters(train, gamma = FALSE)
  forecasted <- predict(fit, n.ahead = length(test), level = 0.95)
  predictions <- as.vector(forecasted)
  
  # Find sell / buy points
  maxIndex <- which.max(predictions)
  predictionsBeforeMax <- predictions[c(1:maxIndex)]
  
  minIndex <- which.min(predictionsBeforeMax)
  predictedProfit <- predictions[maxIndex] - predictions[minIndex]
  actualProfit <- test[maxIndex] - test[minIndex]
  
  
  rmse <- function(fitted, observed) {
    sqrt(mean((fitted - observed) ^ 2))
  }
  
  error <- rmse(test, predictions)
  print("RMSE:")
  print(error)
  
  png(filename = sprintf("plots/holtWinters/holtPrediction_%s.png", formattedName))
  plot(
    as.vector(predictions),
    type ="l",
    col = "red",
    main = "Holt-Winters ETF Prediction",
    sub = sprintf("fund name: %s, RMSE: %#.1f, predicted profit: %#.1f, actual profit: %#.1f", formattedName, error, predictedProfit, actualProfit),
    xlab = "Date Index",
    ylab = "Price"
  )
  lines(test)
  
  # Plot Buy Point
  points(minIndex, predictions[minIndex], type="p", pch=3, col="blue")
  
  # Plot Sell point
  points(maxIndex, predictions[maxIndex], type="p", pch=2, col="green")
  
  legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
  legend("topright", c("Buy", "Sell"), pch = c(3, 2), col = c("Blue", "Green"))
  dev.off()
  
  print(sprintf("RMSE: %#.1f, Predicted Profit: %#.1f, Actual Profit: %#.1f", error, predictedProfit, actualProfit))
  
  results <- rbind(results, data.frame(name = formattedName, error, predictedProfit, actualProfit, absDistanceProfit = abs(predictedProfit - actualProfit)))
}


print("Results:")
print(results)

resultsNumeric <- results[, !(names(results) %in% c("name"))]
means <- colMeans(resultsNumeric)
finalResults <- data.frame(as.list(means))
rownames(finalResults) <- "holtWinters"

print("Mean Results:")
print(finalResults)

write.table(finalResults, "results.csv", sep = ",", col.names = FALSE, append = TRUE)