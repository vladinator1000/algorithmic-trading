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

rmse <- function(fitted, observed) {
  sqrt(mean((fitted - observed) ^ 2))
}

error <- rmse(test$Close, predictions)
print("RMSE:")
print(error)

plot(as.vector(predictions), type = "l", col = "red", main = sprintf("ETF price prediction, name: %s, RMSE: %#.1f", formattedName, error), xlab = "Daily Date Index", ylab = "Price")
lines(as.vector(test))
legend("bottomright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
