library(forecast)
library(tseries)

set.seed(42)

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

# Get a random fund name
name <- sample(fileList, 1)
path <- paste(dataFolder, name, sep = "")
data <- read.csv(path)

price <- ts(data$Close)
price <- tsclean(price)

# When trying to decompose: "Time series is not periodic error"
# makes sense, because stock prices are chaotic
# decompose = stl(price, s.window="periodic")