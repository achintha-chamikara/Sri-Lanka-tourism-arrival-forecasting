# arima_model.R

library(forecast)
library(Metrics)

run_arima_model <- function(train_ts, test_ts) {
  # Auto ARIMA on training data
  auto_arima_model <- auto.arima(train_ts, 
                                 seasonal = TRUE, 
                                 stepwise = FALSE, 
                                 approximation = FALSE)
  
  # Forecast over test period
  forecast_length <- length(test_ts)
  sarima_forecast <- forecast(auto_arima_model, h = forecast_length)
  
  # Extract predictions and actuals
  actual <- as.numeric(test_ts)
  predicted <- as.numeric(sarima_forecast$mean)
  
  # Calculate metrics
  ME   <- mean(predicted - actual, na.rm = TRUE)
  RMSE <- sqrt(mean((predicted - actual)^2, na.rm = TRUE))
  MAE  <- mean(abs(predicted - actual), na.rm = TRUE)
  MAPE <- mean(abs((predicted - actual) / actual) * 100, na.rm = TRUE)
  
  metrics <- data.frame(
    Model = "ARIMA",
    ME = ME,
    RMSE = RMSE,
    MAE = MAE,
    MAPE = MAPE
  )
  
  # Return results
  list(
    model = auto_arima_model,
    forecast = sarima_forecast,
    metrics = metrics,
    actual = actual,
    predicted = predicted
  )
}