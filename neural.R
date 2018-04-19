library(nnfor)
library(xts)

set.seed(42)

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

# Get a random fund name
name <- sample(fileList, 1)
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
plot(mlp.fit)
print(mlp.fit)

# Forecast future prices
mlp.frc <- forecast(mlp.fit, length(test))
plot(mlp.frc)

# Plot predicted vs actual
predictions <- tail(mlp.frc$fitted, n = length(test))

# Find sell / buy points
maxIndex <- which.max(predictions)
predictionsBeforeMax <- predictions[c(1:maxIndex)]

minIndex <- which.min(predictionsBeforeMax)
predictedProfit <- predictions[maxIndex] - predictions[minIndex]
actualProfit <- test[maxIndex] - test[minIndex]

print(sprintf("Predicted profit: %#.1f", predictedProfit))
print(sprintf("Actual profit: %#.1f", actualProfit))

rmse <- function(fitted, observed){
  sqrt(mean((fitted - observed) ^ 2))
}

error <- rmse(as.vector(test), as.vector(predictions))

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
points(minIndex, predictions[minIndex], type="p", pch=3, col="blue")

# Plot Sell point
points(maxIndex, predictions[maxIndex], type="p", pch=2, col="green")

legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
legend("topright", c("Buy", "Sell"), pch=c(3, 2), col = c("Blue", "Green"))


