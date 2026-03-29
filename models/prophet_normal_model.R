# prophet_normal_model.R (improved)
library(prophet)
library(Metrics)

run_prophet_model <- function(train_data, test_data, use_log1p = FALSE) {
  # Expect columns: ds, y
  train_df <- train_data
  test_df  <- test_data

  # Optional log1p transform for stability
  if (use_log1p) {
    train_df$y <- log1p(train_df$y)
  }

  # Build Prophet model
  m <- prophet(
    train_df,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "additive",
    changepoint.prior.scale = 0.15,
    seasonality.prior.scale = 10,
    holidays.prior.scale = 10,
    fit = FALSE
  )

  # Fit the model
  m <- fit.prophet(m, train_df)

  # Forecast
  future <- data.frame(ds = test_df$ds)
  fc <- predict(m, future)

  predicted <- as.numeric(fc$yhat)

  # Back-transform if needed
  if (use_log1p) {
    predicted <- expm1(predicted)
  }

  actual <- as.numeric(test_df$y)

  # Metrics
  valid <- complete.cases(actual, predicted)
  a <- actual[valid]
  p <- predicted[valid]

  if (length(a) == 0) {
    metrics <- data.frame(Model = "Prophet", RMSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
  } else {
    RMSE <- rmse(a, p)
    MAE  <- mae(a, p)
    nz <- a != 0
    MAPE <- if (sum(nz) > 0) mape(a[nz], p[nz]) * 100 else NA_real_

    metrics <- data.frame(
      Model = "Prophet",
      RMSE = round(RMSE, 2),
      MAE = round(MAE, 2),
      MAPE = round(MAPE, 2)
    )
  }

  list(
    model = m,
    forecast = fc,
    metrics = metrics,
    actual = actual,
    predicted = predicted
  )
}