library(forecast)
library(anytime)
library(corrplot)

names <- c("ayt.us.txt", "bil.us.txt", "chie.us.txt",  "cefl.us.txt", "epu.us.txt", "fan.us.txt", "gal.us.txt", "veu.us.txt", "vig.us.txt", "xhs.us.txt")

dataFolder <- file.path(getwd(), "data", "/")
fileList <- list.files(path = dataFolder)

for (name in names) {
  formattedName <- gsub(".us.txt", "", name)
  path <- paste(dataFolder, name, sep = "")
  data <- read.csv(path)
  
  png(filename = sprintf("plots/exploration/price_%s.png", formattedName))
  plot(anydate(data$Date), data$Close, type = "l", main = sprintf("\"%s\" ETF Price", formattedName), xlab = "Date", ylab = "Price")
  dev.off()
  
  price <- data$Close

  summaryPlot <- function(data_to_plot) {
    data <- na.omit(as.numeric(as.character(data_to_plot)))
    dataFull <- as.numeric(as.character(data_to_plot))
  
    #histogram on the top left
    h <- hist(data, breaks = "Sturges", plot = FALSE)
    xfit <- seq(min(data), max(data), length = 100)
    yfit <- yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))
    yfit <- yfit * diff(h$mids[1:2]) * length(data)
    
    png(filename = sprintf("plots/exploration/distribution_%s.png", formattedName))
    plot (h, axes = TRUE, main = sprintf("Summary plot of \"%s\" ETF Price", formattedName), cex.main = 2, xlab = NA)
    lines(xfit, yfit, col = "blue", lwd=2)
  
    leg1 <- paste("mean = ", round(mean(data), digits = 4))
    leg2 <- paste("sd = ", round(sd(data),digits = 4))
    count <- paste("count = ", sum(!is.na(dataFull)))
    missing <- paste("missing = ", sum(is.na(dataFull)))
    legend(x = "topright", c(leg1, leg2, count, missing), bty = "n")
    
    dev.off()
  }
  
  summaryPlot(price)
}



renameColumns <- function(df, oldNames, newNames) {
  dfNames <- colnames(df)
  dfNames[which(dfNames %in% oldNames)] <- newNames
  
  colnames(df) <- dfNames
  
  return(df)
}

# Load each .csv with unique column names except "Date"
load <- function(item) {
  data <- read.csv(paste(dataFolder, item, sep=''))
  data <- data.frame(data)
  data <- data[c("Date", "Close")]
  
  # Format Date
  data$Date <- anydate(data$Date)
  
  prefix <- unlist(strsplit(item, split = ".us.txt"))[1]
  
  newColumnNames <- paste(prefix, names(data), sep = "")
  newColumnNames[1] <- "Date"
  
  data = renameColumns(data, names(data), newColumnNames)
  
  return(data)
}

dataframes <- lapply(names, load)
combined <- Reduce(function(x, y) merge(x, y, by = "Date"), dataframes)
combined <- combined[order(combined$Date),]

onlyNumerical <- combined[sapply(combined, function(x) is.numeric(x))]
Filter(function(x) !all(0), onlyNumerical)

correlationMatrix <- cor(onlyNumerical)

png(filename = "plots/exploration/correlation.png")
corrplot(correlationMatrix, method = "color", title = "Correlation of ETF Closing Prices", mar = c(0,0,1,0))
dev.off()