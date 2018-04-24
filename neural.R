library(nnfor)

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
  data <- read.csv(paste(dataFolder, name, sep = ""))
  
  # Train-test split
  splitIndex <- floor(nrow(data) * 0.8)
  train <- data[c(1:splitIndex),]
  train <- ts(train$Close)
  
  test <- data[c(splitIndex:nrow(data)),]
  test <- ts(test$Close)
  
  # Train neural network
  mlp.fit <- mlp(train)
  
  png(filename = sprintf("plots/neural/neuralSimple_model_%s.png", formattedName))
  plot(mlp.fit, main = sprintf("Multi-layer Perceptron Model for %s", formattedName))
  dev.off()
  print(mlp.fit)
  

  # Forecast future prices
  mlp.frc <- forecast(mlp.fit, length(test))
  
  png(filename = sprintf("plots/neural/neuralSimple_forecast_%s.png", formattedName))
  plot(mlp.frc, main = sprintf("Simple NN Model Forecasts for \"%s\"", formattedName))
  dev.off()
  
  # Plot predicted vs actual
  predictions <- tail(mlp.frc$fitted, n = length(test))
  
  # Find sell / buy points
  maxIndex <- which.max(predictions)
  predictionsBeforeMax <- predictions[c(1:maxIndex)]
  
  minIndex <- which.min(predictionsBeforeMax)
  predictedProfit <- predictions[maxIndex] - predictions[minIndex]
  actualProfit <- test[maxIndex] - test[minIndex]
  
  rmse <- function(fitted, observed) {
    sqrt(mean((fitted - observed) ^ 2))
  }
  
  error <- rmse(as.vector(test), as.vector(predictions))
  
  png(filename = sprintf("plots/neural/neuralSimple_%s.png", formattedName))
  plot(
    as.vector(predictions),
    type ="l",
    col = "red",
    main = "Neural Net ETF Prediction",
    sub = sprintf("fund name: %s, RMSE: %#.1f, predicted profit: %#.1f, actual profit: %#.1f", formattedName, error, predictedProfit, actualProfit),
    xlab = "Date Index",
    ylab = "Price"
  )
  lines(as.vector(test))
  
  # Plot Buy Point
  points(minIndex, predictions[minIndex], type="p", pch = 3, col = "blue")
  
  # Plot Sell point
  points(maxIndex, predictions[maxIndex], type="p", pch = 2, col = "green")
  
  legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
  legend("topright", c("Buy", "Sell"), pch=c(3, 2), col = c("Blue", "Green"))
  
  dev.off()
  
  print(sprintf("RMSE: %#.1f, Predicted Profit: %#.1f, Actual Profit: %#.1f", error, predictedProfit, actualProfit))
  
  results <- rbind(results, data.frame(name = formattedName, error, predictedProfit, actualProfit, absDistanceProfit = abs(predictedProfit - actualProfit)))
}

print("Results:")
print(results)

resultsNumeric <- results[, !(names(results) %in% c("name"))]
means <- colMeans(resultsNumeric)
finalResults <- data.frame(as.list(means))
rownames(finalResults) <- "neuralSimple"

print("Mean Results:")
print(finalResults)

write.table(finalResults, "results.csv", sep = ",", col.names = FALSE, append = TRUE)