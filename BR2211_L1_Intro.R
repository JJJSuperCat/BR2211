#BR2211 L1

# Remove the full list of R objects in session
rm(list = ls())


# Set data directory
# Current working directory is where the R script is
data_folder <- "/Users/joeyzhou/Library/CloudStorage/OneDrive-Personal/0.OneDrive_ZhouFiles/A.Work/A.NTU/A.Uni.Courses/BR2211 Financial and Risk Analytics I/2025 Material (Joey)/Lecture Slides/Lecture Data"


# R-example0 --------------------------------------------------------------


#Create sample data within RStudio
x <- 3 * 4
y <- c(2, 4, 6)
y[2]

#Import data from a csv file
library(readr)

filename <- "indexData.csv"
StockEXData <- read_csv(paste(data_folder, filename, sep = "/"))
View(StockEXData)
summary(StockEXData)

# Change Index into factor type
StockEXData$Index <- as.factor(StockEXData$Index)
summary(StockEXData)


# date range for each index
library(tidyverse)
StockEXData %>% group_by(Index) %>% 
  summarize(
    Date_Begin = min(Date),
    Date_End = max(Date)
  )


# R-example1 --------------------------------------------------------------

# Remove the full list of R objects in session
rm(list = ls())

#Stylized facts on the returns for Siemens.
library(fBasics) # Provides specialized plotting/analysis functions (e.g., boxPlot(), qqnormPlot())
library(timeSeries) # Provides timeSeries objects and related methods
library(evir) #to access the siemens dataset
data(siemens)
?siemens
head(siemens)
SieDates <- as.character(format(as.POSIXct(attr(siemens, "times")),"%Y-%m-%d"))
head(SieDates)
SieRet <- timeSeries(siemens * 100, charvec = SieDates)
head(SieRet)
colnames(SieRet) <- "SieRet"	
head(SieRet)

## Stylised Facts I	
par(mfrow=c(2, 2))
seriesPlot(SieRet, title=FALSE, main="Daily Returns of Siemens",col="blue")
boxPlot(SieRet, title=FALSE, main="Box plot of Returns",col="blue", cex=0.5, pch=19)
acf(SieRet, main="ACF of Returns", lag.max=20, ylab=" ", xlab=" ", col="blue", ci.col="red")
pacf(SieRet, main = "PACF of Returns", lag.max = 20, ylab = " ",xlab = " ", col = "blue", ci.col = "red")

## Stylised Facts II
SieRetAbs <-abs(SieRet) 
SieRet100 <- tail(sort(abs(series(SieRet))), 100)[1]
idx <- which(series(SieRetAbs) > SieRet100, arr.ind = TRUE)
SieRetAbs100 <- timeSeries(rep(0, length(SieRet)),charvec = time(SieRet))
SieRetAbs100[idx, 1] <- SieRetAbs[idx]
acf(SieRetAbs, main = "ACF of Absolute Returns", lag.max = 20,ylab = " ", xlab = " ", col = "blue", ci.col = "red")
pacf(SieRetAbs, main = "PACF of Absolute Returns", lag.max = 20,ylab = " ", xlab = " ", col = "blue", ci.col = "red")
qqnormPlot(SieRet, main = "QQ-Plot of Returns", title = FALSE,col = "blue", cex = 0.5, pch = 19)
plot(SieRetAbs100, type = "h", main = "Volatility Clustering", ylab = " ", xlab = " ", col = "blue")


# R-example2 --------------------------------------------------------------

# Remove the full list of R objects in session
rm(list = ls())

#Stylized facts on the European equity market.
library(zoo) # infrastructure for working with time series data. The name zoo comes from Z’s ordered observations. 
data(EuStockMarkets) 
?EuStockMarkets
head(EuStockMarkets)

## Time series plot of levels 
EuStockLevel <- as.zoo(EuStockMarkets)[, c("DAX", "CAC", "FTSE")] 
plot(EuStockLevel, xlab = " ", main = " ")

## Percentage returns 
EuStockRet <- diff(log(EuStockLevel))*100
plot(EuStockRet, xlab = " ", main = " ") 

## Cross correlations
layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))
ccf(EuStockRet[, 1], EuStockRet[, 2], ylab = " ", xlab = " ",lag.max = 20, main = "Returns DAX vs CAC")
ccf(abs(EuStockRet)[, 1], abs(EuStockRet)[, 2], ylab = " ",xlab = " ", lag.max = 20, main = "Absolute returns DAX vs CAC") 
ccf(EuStockRet[, 1], EuStockRet[, 3], ylab = " ", xlab = " ", lag.max = 20, main = "Returns DAX vs FTSE")
ccf(abs(EuStockRet)[, 1], abs(EuStockRet)[, 3], ylab = " ", xlab = " ", lag.max = 20, main = "Absolute returns DAX vs FTSE") 
ccf(EuStockRet[, 2], EuStockRet[, 3], ylab = " ", xlab = " ",lag.max = 20, main = "Returns CAC vs FTSE") 
ccf(abs(EuStockRet)[, 2], abs(EuStockRet)[, 3], ylab = " ", xlab = " ", lag.max = 20, main = "Absolute returns CAC vs FTSE") 

## Rolling correlations 
rollc <- function(x){
  dim <- ncol(x) 
  rcor <- cor(x)[lower.tri(diag(dim), diag = FALSE)]
  return(rcor)
}
rcor <- rollapply(EuStockRet, width = 250, rollc, align = "right", by.column = FALSE)
colnames(rcor) <- c("DAX & CAC", "DAX & FTSE", "CAC & FTSE") 
plot(rcor, main = " ", xlab = " ") 

