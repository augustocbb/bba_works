###########################################################
# File: analysis_script.R                                   #
# Author: Augusto                                           #
# Date: 05/11/2020                                          #
# Description: This script performs data analysis on        #
#              stock price data and Google Trends data,     #
#              including stationarity tests, cointegration  #
#              tests, and VAR (Vector Autoregression)       #
#              analysis. The results are saved in           #
#              Excel files.                                 #
###########################################################

# Load required libraries
library(installr)
library(vars)
library(openxlsx)
library(readxl)
library(WDI)
library(plm)
library(sqldf)
library(magrittr)
library(rprojroot)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(skimr)
library(scales)
library(gridExtra)
library(forecast)
library(fpp2)
library(stringr)
library(FitAR)
library(gtrendsR)
library(quantmod)
library(BatchGetSymbols)
library(tidyverse)
library(Amelia)
library(reshape2)
library(ggthemes)
library(plyr)
library(tseries)
library(urca)

### Declare functions ###

# Function to import Google Trends data
importar_gtrends <- function(pchave, grupo, local, intervalo) {
  for (i in 1:length(pchave)) {
    x <- gtrends(
      keyword = pchave[i],
      geo = local,
      gprop = grupo,
      time = intervalo
    )
    
    # Combine data from multiple queries into a single data frame
    if (i > 1) {
      trends.ticker <- rbind(trends.ticker, x$interest_over_time)
    } else {
      trends.ticker <- x$interest_over_time
    }
    print(i)
  }
  return(trends.ticker)
}

## Declare variables ##

# Define the time interval
bg <- '2014-01-01'  # Start date
lst <- Sys.Date()   # End date (current date)

## Get stock ticker symbols for Ibovespa index stocks ##

# Get stock tickers for Ibovespa index stocks
acoes <- GetIbovStocks()
tickers <- paste(acoes$tickers, ".SA", sep = "")

# Define benchmark index ticker
bench <- '^BVSP'

# Get stock price data using BatchGetSymbols package
cotacoes_bruto <- BatchGetSymbols(
  tickers = tickers,
  bench.ticker = bench,
  first.date = bg,
  last.date = lst
)
cotacoes <- cotacoes_bruto$df.tickers
cotacoes <- cotacoes[complete.cases(cotacoes),]

## Get Google Trends data ##

# Import Google Trends data for individual stock tickers
a <- importar_gtrends(
  pchave = acoes$tickers[1],
  grupo = "web",
  local = "",
  intervalo = "2014-01-01 2020-09-29"
)

names(a)
ticker <- importar_gtrends(
  pchave = acoes$tickers,
  grupo = "web",
  local = "",
  intervalo = "2014-01-01 2020-09-29"
)

noticias <- importar_gtrends(
  pchave = acoes$tickers,
  grupo = "news",
  local = "",
  intervalo = "2014-01-01 2020-09-29"
)

noticias_br <- importar_gtrends(
  pchave = acoes$tickers,
  grupo = "news",
  local = "BR",
  intervalo = "2014-01-01 2020-09-29"
)

ticker_br <- importar_gtrends(
  pchave = acoes$tickers,
  grupo = "web",
  local = "BR",
  intervalo = "2014-01-01 2020-09-29"
)

# Merge and transform the data
trends.ticker <- rbind(a, ticker, noticias, noticias_br, ticker_br)
trends.ticker$date <- as.Date(trends.ticker$date)

# Perform stationarity tests (ADF test) on the time series data
trends.stationary <- data.frame()

for (i in 2:ncol(trends.ticker)) {
  adf.test <- ur.df(trends.ticker[, i], type = "none", lags = 5)
  adf.pvalue <- summary(adf.test)$teststatistic[[1]]
  adf.critical <- summary(adf.test)$cval[[1, "1%"]]
  
  # Store the test results in a data frame
  test_result <- data.frame(
    Ticker = colnames(trends.ticker)[i],
    P_Value = adf.pvalue,
    Critical_Value = adf.critical
  )
  
  trends.stationary <- rbind(trends.stationary, test_result)
}

# Save the test results
write.csv(trends.stationary, "stationarity_results.csv", row.names = FALSE)

# Perform Johansen test for cointegration
johansen_test <- vars::Johansen(trends.ticker[, -1], r = 2, p = 0)
summary(johansen_test)

# Perform VAR (Vector Autoregression) analysis
var_model <- VAR(trends.ticker[, -1], p = 1, type = "const")
var_summary <- summary(var_model)
var_summary

## Save the obtained results into Excel files ##

# Create a new Excel workbook
wb <- createWorkbook()

# Add a sheet for stationarity test results
addWorksheet(wb, "Stationarity Test")
writeData(wb, sheet = "Stationarity Test", trends.stationary)

# Add a sheet for Johansen test results
addWorksheet(wb, "Johansen Test")
writeData(wb, sheet = "Johansen Test", summary(johansen_test))

# Add a sheet for VAR analysis results
addWorksheet(wb, "VAR Analysis")
writeData(wb, sheet = "VAR Analysis", var_summary$varresult$residuals)

# Save the workbook
saveWorkbook(wb, "analysis_results.xlsx", overwrite = TRUE)
