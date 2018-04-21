library(forecast)
library(tseries)

names <- c("veu.us.txt", "vig.us.txt", "xhs.us.txt")

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

for (name in names) {
  formattedName <- gsub(".us.txt", "", name)
  path <- paste(dataFolder, name, sep = "")
  data <- read.csv(path)
  
  price <- data$Close
  
  # When trying to decompose: "Time series is not periodic error"
  # makes sense, because stock prices are chaotic
  # decompose = stl(price, s.window="periodic")
  
  summaryPlot <- function(data_to_plot) {
    data <- na.omit(as.numeric(as.character(data_to_plot)))
    dataFull <- as.numeric(as.character(data_to_plot))
  
    #histogram on the top left
    h <- hist(data, breaks = "Sturges", plot = FALSE)
    xfit <- seq(min(data), max(data), length = 100)
    yfit <- yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))
    yfit <- yfit * diff(h$mids[1:2]) * length(data)
  
    plot (h, axes = TRUE, main = sprintf("Summary plot of \"%s\" ETF Price", formattedName), cex.main = 2, xlab = NA)
    lines(xfit, yfit, col = "blue", lwd=2)
  
    leg1 <- paste("mean = ", round(mean(data), digits = 4))
    leg2 <- paste("sd = ", round(sd(data),digits = 4))
    count <- paste("count = ", sum(!is.na(dataFull)))
    missing <- paste("missing = ", sum(is.na(dataFull)))
    legend(x = "topright", c(leg1, leg2, count, missing), bty = "n")
  }
  
  summaryPlot(price)
}