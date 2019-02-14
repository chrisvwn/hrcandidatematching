# freelancer_test

if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}


calcStocks <- function(stockCode, stockYear)
{  
  # Find access to real time data, I would like to put in the stock tickers name and have the program download and interpret 
  # 1yr worth of data. The data that can be downloaded online or is provided by certain packages includes : the price of the stock, 
  # the high of day, the low , the closing price, the opening price, and the volume of the day. 
  
  
  start <- as.Date(paste0(stockYear, "-01-01"))
  end <- as.Date(paste0(stockYear+1, "-01-01"))
  
  getSymbols(stockCode, src = "yahoo", from = start, to = end)
  
  head(stockCode)
  
  
  ## Show me how you will calculate it.
  
  
  # VolumeAverage%  - 10day. 20day, 60day
  # This is the % of the average volume it has done on a particular day
  
  aver10 =mean(head(AAPL$AAPL.Volume, n=10))
  aver20=mean(head(AAPL$AAPL.Volume, n=20))
  aver60 =mean(head(AAPL$AAPL.Volume, n=60))
  aver = mean(AAPL$AAPL.Volume)
  print(paste0("This is the average of the volume of the first 10 days: ", aver10))
  print(paste0("This is the average of the volume of the first 20 days: ", aver20))
  print(paste0("This is the average of the volume of the first 60 days: ", aver60))
  
  
  # Also calculate
  
  
  # Range 
  # Todays high - low 
  #such as this is range
  AAPL$range <- AAPL$AAPL.High - AAPL$AAPL.Low 
  
  # how will you calculate this
  # Average Range - 10day, 20day, 60day
  # The average of the ranges over the last 10,20,60 days. 
  
  averrange10 =mean(head(AAPL$range, n=10))
  averrange20=mean(head(AAPL$range, n=20))
  averrange60 =mean(head(AAPL$range, n=60))
  averrange = mean(AAPL$range)
  print(paste0("This is the average of the range of the first 10 days: ", averrange10))
  print(paste0("This is the average of the range of the first 20 days: ", averrange20))
  print(paste0("This is the average of the range of the first 60 days: ", averrange60))
}

calcStocks("AAPL")


