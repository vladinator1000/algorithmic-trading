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

for (name in names) {
  formattedName <- gsub(".us.txt", "", name)
  path <- paste(dataFolder, name, sep = "")
  data <- read.csv(path)
  data$Date <- as.Date(data$Date)
  
  # Train-test split
  splitIndex <- floor(nrow(data) * 0.8)
  train <- data[c(1:splitIndex),]
  test <- data[c(splitIndex:nrow(data)),]
  
  model <- lm(Close ~ Date + Volume, data = train)
  predictions <- predict(model, test, n.ahead = 300)
  
  # Find sell / buy points
  maxIndex <- which.max(predictions)
  predictionsBeforeMax <- predictions[c(1:maxIndex)]
  
  minIndex <- which.min(predictionsBeforeMax)
  predictedProfit <- predictions[maxIndex] - predictions[minIndex]
  actualProfit <- test$Close[maxIndex] - test$Close[minIndex]
  
  
  rmse <- function(fitted, observed) {
    sqrt(mean((fitted - observed) ^ 2))
  }
  
  error <- rmse(test$Close, predictions)
  
  png(filename = sprintf("plots/linear/linearModel_%s.png", formattedName))
  plot(
    as.vector(predictions),
    type ="l",
    col = "red",
    main = "Linear Model ETF Prediction",
    sub = sprintf("fund name: %s, RMSE: %#.1f, predicted profit: %#.1f, actual profit: %#.1f", formattedName, error, predictedProfit, actualProfit),
    xlab = "Date Index",
    ylab = "Price"
  )
  lines(test$Close)
  
  # Plot Buy Point
  points(minIndex, predictions[minIndex], type = "p", pch = 3, col = "blue")
  
  # Plot Sell point
  points(maxIndex, predictions[maxIndex], type = "p", pch = 2, col = "green")
  
  legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
  legend("bottomleft", c("Buy", "Sell"), pch = c(3, 2), col = c("Blue", "Green"))
  
  dev.off()
  
  results <- rbind(results, data.frame(name = formattedName, error, predictedProfit, actualProfit, absDistanceProfit = abs(predictedProfit - actualProfit)))
}

print("Results:")
print(results)

resultsNumeric <- results[, !(names(results) %in% c("name"))]
means <- colMeans(resultsNumeric)
finalResults <- data.frame(as.list(means))
rownames(finalResults) <- "linear"

print("Mean Results:")
print(finalResults)

write.table(finalResults, "results.csv", sep = ",", col.names = FALSE, append = TRUE)
