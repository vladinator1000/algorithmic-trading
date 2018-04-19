library(nnfor)
library(xts)

set.seed(42)

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

# Get a random fund name
name <- sample(fileList, 1)
data <- read.csv(paste(dataFolder, name, sep = ""))

# Train-test split
splitIndex <- floor(nrow(data) * 0.8)
train <- data[c(1:splitIndex),]
train <- ts(train$Close)

test <- data[c(splitIndex:nrow(data)),]
test <- test$Close

mlp.fit <- mlp(ts(train))
plot(mlp.fit)
print(mlp.fit)

mlp.frc <- forecast(mlp.fit, length(test))
plot(mlp.frc)

# Plot predicted vs actual
predictions <- tail(mlp.frc$fitted, n = length(test))

rmse <- function(fitted, observed) {
  sqrt(mean((fitted - observed) ^ 2))
}

error <- rmse(test, predictions)

plot(as.vector(predictions), type = "l", col = "red", sprintf("Predicted ETF prices, RMSE: %#.1f", error), xlab = "Date Index", ylab = "Price")
lines(as.vector(test))
legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
