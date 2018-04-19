set.seed(42)

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

# Get a random fund name
name <- sample(fileList, 1)
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
print("RMSE:")
print(error)

plot(predictions, type = "l", col = "red", main = "Linear Model ETF price prediction", sub = sprintf("fund name: %s, RMSE: %#.1f", formattedName, error), xlab = "Daily Date Index", ylab = "Price")
lines(test$Close)
# Plot Sell point
points(maxIndex, predictions[maxIndex], type="p", pch=1, col="green")

# Plot Buy Point
points(minIndex, predictions[minIndex], type="p", pch=1, col="green")

legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
legend("topright", c("Buy", "Sell"), pch=c(3, 2), col = c("Blue", "Green"))