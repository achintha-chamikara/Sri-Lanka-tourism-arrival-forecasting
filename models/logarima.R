# models/arima_model.R
library(forecast)

run_arima_model <- function(train_ts, test_ts, use_log1p = TRUE) {
  stopifnot(length(train_ts) > 0, length(test_ts) > 0)

  # Transform if requested
  if (use_log1p) {
    train_fit <- log1p(as.numeric(train_ts))
    test_actual <- as.numeric(test_ts)
  } else {
    train_fit <- as.numeric(train_ts)
    test_actual <- as.numeric(test_ts)
  }

  # Convert transformed training data back to ts
  train_fit_ts <- ts(
    train_fit,
    start = start(train_ts),
    frequency = frequency(train_ts)
  )

  fit <- auto.arima(
    train_fit_ts,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE
  )

  h <- length(test_actual)
  fc <- forecast(fit, h = h)

  # Back-transform forecasts if needed
  predicted <- as.numeric(fc$mean)
  if (use_log1p) {
    predicted <- expm1(predicted)
  }

  actual <- test_actual

  stopifnot(length(actual) == length(predicted))

  err <- predicted - actual
  ME <- mean(err, na.rm = TRUE)
  RMSE <- sqrt(mean(err^2, na.rm = TRUE))
  MAE <- mean(abs(err), na.rm = TRUE)

  non_zero <- !is.na(actual) & !is.na(predicted) & actual != 0
  MAPE <- if (sum(non_zero) > 0) mean(abs(err[non_zero] / actual[non_zero]) * 100) else NA_real_

  metrics <- data.frame(
    Model = if (use_log1p) "ARIMA (log1p)" else "ARIMA",
    ME = ME,
    RMSE = RMSE,
    MAE = MAE,
    MAPE = MAPE
  )

  list(
    model = fit,
    forecast = fc,
    metrics = metrics,
    actual = actual,
    predicted = predicted
  )
}