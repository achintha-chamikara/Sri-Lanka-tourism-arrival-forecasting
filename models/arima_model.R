

library(forecast)

run_arima_model <- function(train_ts, test_ts) {
  stopifnot(length(test_ts) > 0)

  fit <- auto.arima(
    train_ts,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE
  )

  h <- length(test_ts)
  fc <- forecast(fit, h = h)

  actual <- as.numeric(test_ts)
  predicted <- as.numeric(fc$mean)

  # Guardrails: prevent recycling / misalignment
  stopifnot(length(actual) == length(predicted))

  err <- predicted - actual
  ME <- mean(err, na.rm = TRUE)
  RMSE <- sqrt(mean(err^2, na.rm = TRUE))
  MAE <- mean(abs(err), na.rm = TRUE)

  # MAPE: exclude zeros
  non_zero <- !is.na(actual) & !is.na(predicted) & actual != 0
  MAPE <- if (sum(non_zero) > 0) mean(abs(err[non_zero] / actual[non_zero]) * 100) else NA_real_

  metrics <- data.frame(
    Model = "ARIMA",
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