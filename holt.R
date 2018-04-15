library(forecast)
library(xts)

set.seed(42)

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

# Get a random fund name
name <- sample(fileList, 1)
path <- paste(dataFolder, name, sep = "")
data <- read.csv(path)

price <- xts(data$Close, as.Date(data$Date))

# Train-test split
splitIndex <- floor(nrow(price) * 0.8)
train <- price[c(1:splitIndex)]
test <- price[c(splitIndex:length(price))]

               
fit <- HoltWinters(train, gamma = FALSE)
prediction <- predict(fit)

plot.xts(test, main = paste(name, "closing price"))
(fit)

forecast <- predict(fit, n.ahead = 90, prediction.interval = T, level = 0.95)
plot(fit, forecast)
