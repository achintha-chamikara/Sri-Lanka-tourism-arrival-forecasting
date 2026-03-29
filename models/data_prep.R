# data_prep.R

library(readxl)
library(dplyr)
library(lubridate)
library(tseries)

load_and_prepare_data <- function(file_path = "data/Tourist Arrivals (2014 Jan - 2025 Dec).xlsx") {
  # Read the Excel file
  arrival_data <- read_excel(file_path)
  names(arrival_data) <- c("Year", "Month", "Tourist_Arrivals")
  
  # Create a Date column (first day of month)
  arrival_data$Date <- as.Date(paste(arrival_data$Year, 
                                     match(arrival_data$Month, month.name), 
                                     "01", sep = "-"))
  
  # Create time series object (for ARIMA)
  ts_data <- ts(arrival_data$Tourist_Arrivals, 
                frequency = 12, 
                start = c(2014, 1))
  
  # Create Prophet dataframe
  prophet_df <- data.frame(
    ds = arrival_data$Date,
    y = arrival_data$Tourist_Arrivals
  )
  
  # Mask COVID period (set to NA for Prophet)
  prophet_df <- prophet_df %>%
    mutate(
      y = ifelse(
        ds >= as.Date("2020-03-01") & ds <= as.Date("2021-12-31"),
        NA,
        y
      )
    )
  
  # Split into train/test for ARIMA (using time index)
  n <- length(ts_data)
  train_size <- floor(0.8 * n)
  train_ts <- window(ts_data, end = c(2014 + (train_size-1) %/% 12, (train_size-1) %% 12 + 1))
  test_ts  <- window(ts_data, start = c(2014 + train_size %/% 12, train_size %% 12 + 1))
  
  # Split into train/test for Prophet (using row index, after masking)
  n_prophet <- nrow(prophet_df)
  train_size_prophet <- floor(0.8 * n_prophet)
  train_data <- prophet_df[1:train_size_prophet, ]
  test_data  <- prophet_df[(train_size_prophet + 1):n_prophet, ]
  
  # Return all objects in a list
  list(
    arrival_data = arrival_data,
    ts_data = ts_data,
    prophet_df = prophet_df,
    train_ts = train_ts,
    test_ts = test_ts,
    train_data = train_data,
    test_data = test_data
  )
}