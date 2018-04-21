names <- c("veu.us.txt", "vig.us.txt", "xhs.us.txt")

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

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
  predictions <- predict(model, test)
  
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
  points(minIndex, predictions[minIndex], type="p", pch=3, col="blue")
  
  # Plot Sell point
  points(maxIndex, predictions[maxIndex], type="p", pch=2, col="green")
  
  legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
  legend("bottomleft", c("Buy", "Sell"), pch=c(3, 2), col = c("Blue", "Green"))
  
  print(sprintf("RMSE: %#.1f, Predicted Profit: %#.1f, Actual Profit: %#.1f", error, predictedProfit, actualProfit))
}