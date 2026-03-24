# prophet_model.R

library(prophet)
library(Metrics)

run_prophet_model <- function(train_data, test_data) {
  # Train Prophet model
  prophet_model <- prophet(
    train_data,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",
    changepoint.prior.scale = 0.02,
    holidays.prior.scale = 5
  )
  
  # Forecast on test period
  future <- data.frame(ds = test_data$ds)
  forecast <- predict(prophet_model, future)
  predicted <- forecast$yhat
  
  # Actual values (note: test_data may contain NA, handle later)
  actual <- test_data$y
  
  # Remove NA for metrics
  valid <- complete.cases(actual, predicted)
  actual_valid <- actual[valid]
  predicted_valid <- predicted[valid]
  
  if (length(actual_valid) == 0) {
    metrics <- data.frame(Model = "Prophet", 
                          RMSE = NA, MAE = NA, MAPE = NA)
  } else {
    RMSE <- rmse(actual_valid, predicted_valid)
    MAE  <- mae(actual_valid, predicted_valid)
    
    # MAPE only for non-zero actuals
    non_zero <- actual_valid != 0
    if (sum(non_zero) > 0) {
      MAPE <- mape(actual_valid[non_zero], predicted_valid[non_zero]) * 100
    } else {
      MAPE <- NA
    }
    
    metrics <- data.frame(
      Model = "Prophet",
      RMSE = round(RMSE, 2),
      MAE = round(MAE, 2),
      MAPE = ifelse(is.na(MAPE), "N/A", round(MAPE, 2))
    )
  }
  
  list(
    model = prophet_model,
    forecast = forecast,
    metrics = metrics,
    actual = actual,
    predicted = predicted
  )
}
