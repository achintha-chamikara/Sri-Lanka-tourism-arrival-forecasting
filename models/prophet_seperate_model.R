# prophet_seperate_model.R (Regime Prophet) - corrected + improved
library(prophet)
library(dplyr)
library(lubridate)

run_regime_prophet <- function(prophet_df,
                               eval_mode = c("both", "full", "meaningful"),
                               meaningful_exclude_covid_collapse = TRUE,
                               add_monthly_seasonality = FALSE,
                               future_periods = 12) {
  eval_mode <- match.arg(eval_mode)
  
  stopifnot(all(c("ds", "y") %in% names(prophet_df)))
  
  df <- prophet_df %>%
    mutate(ds = as.Date(ds), y = as.numeric(y)) %>%
    filter(!is.na(ds))
  
  stopifnot(nrow(df) > 0)
  
  # --- Regime splits ---
  pre_easter   <- df %>% filter(year(ds) < 2019)
  easter_covid <- df %>% filter(year(ds) >= 2019 & year(ds) <= 2021)
  post_covid   <- df %>% filter(year(ds) >= 2022)
  
  min_points <- 10
  if (nrow(pre_easter) < min_points) stop("Not enough points in pre_easter regime.")
  if (nrow(easter_covid) < min_points) stop("Not enough points in easter_covid regime.")
  if (nrow(post_covid) < min_points) stop("Not enough points in post_covid regime.")
  
  # --- Holidays for regime 2 ---
  easter_attack <- data.frame(
    holiday = "easter_attack",
    ds = as.Date("2019-04-21"),
    lower_window = -14,
    upper_window = 45
  )
  
  covid_lockdown <- data.frame(
    holiday = "covid",
    ds = seq(as.Date("2020-03-01"), as.Date("2021-12-01"), by = "month"),
    lower_window = 0,
    upper_window = 30
  )
  
  holidays_m2 <- bind_rows(easter_attack, covid_lockdown)
  
  # --- Fit m1 ---
  m1 <- prophet(
    pre_easter[, c("ds", "y")],
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",
    changepoint.prior.scale = 0.05,
    seasonality.prior.scale = 10,
    n.changepoints = min(10, nrow(pre_easter) - 1),
    fit = FALSE
  )
  if (add_monthly_seasonality) m1 <- add_seasonality(m1, name = "monthly", period = 30.5, fourier.order = 5)
  m1 <- fit.prophet(m1, pre_easter[, c("ds", "y")])
  
  # --- Fit m2 ---
  
  m2 <- prophet(
    easter_covid[, c("ds", "y")],
    yearly.seasonality = TRUE,       # CHANGED from FALSE (more realistic seasonality)
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "additive",
    changepoint.prior.scale = 0.01,
    seasonality.prior.scale = 0.5,   # CHANGED from 0.1 (less underfit)
    holidays = holidays_m2,
    holidays.prior.scale = 10,
    n.changepoints = min(10, nrow(easter_covid) - 1),
    fit = FALSE
  )
  if (add_monthly_seasonality) m2 <- add_seasonality(m2, name = "monthly", period = 30.5, fourier.order = 5)
  m2 <- fit.prophet(m2, easter_covid[, c("ds", "y")])
  
  # --- Fit m3 ---
  m3 <- prophet(
    post_covid[, c("ds", "y")],
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",
    changepoint.prior.scale = 0.1,
    seasonality.prior.scale = 15,
    n.changepoints = min(10, nrow(post_covid) - 1),
    fit = FALSE
  )
  if (add_monthly_seasonality) m3 <- add_seasonality(m3, name = "monthly", period = 30.5, fourier.order = 5)
  m3 <- fit.prophet(m3, post_covid[, c("ds", "y")])
  
  # --- Assign regimes for prediction ---
  df_pred <- df %>%
    mutate(regime = case_when(
      ds < as.Date("2019-04-01") ~ "pre_easter",
      ds >= as.Date("2019-04-01") & ds <= as.Date("2021-12-31") ~ "easter_covid",
      TRUE ~ "post_covid"
    ))
  
  # --- Predict per regime ---
  df_pred$predicted <- NA_real_
  
  idx1 <- which(df_pred$regime == "pre_easter")
  if (length(idx1) > 0) df_pred$predicted[idx1] <- as.numeric(predict(m1, data.frame(ds = df_pred$ds[idx1]))$yhat)
  
  idx2 <- which(df_pred$regime == "easter_covid")
  if (length(idx2) > 0) df_pred$predicted[idx2] <- as.numeric(predict(m2, data.frame(ds = df_pred$ds[idx2]))$yhat)
  
  idx3 <- which(df_pred$regime == "post_covid")
  if (length(idx3) > 0) df_pred$predicted[idx3] <- as.numeric(predict(m3, data.frame(ds = df_pred$ds[idx3]))$yhat)
  
  stopifnot(length(df_pred$y) == length(df_pred$predicted))
  
  # --- Metrics ---
  calc_metrics_safe <- function(actual, predicted, model_name) {
    valid <- complete.cases(actual, predicted)
    a <- actual[valid]
    p <- predicted[valid]
    
    if (length(a) == 0) {
      return(data.frame(Model = model_name, RMSE = NA_real_, MAE = NA_real_, MAPE = NA_real_, sMAPE = NA_real_))
    }
    
    RMSE <- sqrt(mean((a - p)^2))
    MAE  <- mean(abs(a - p))
    
    nz <- a != 0
    MAPE <- if (sum(nz) > 0) mean(abs((a[nz] - p[nz]) / a[nz])) * 100 else NA_real_
    
    denom <- abs(a) + abs(p)
    ok <- denom != 0
    sMAPE <- if (sum(ok) > 0) mean(200 * abs(a[ok] - p[ok]) / denom[ok]) else NA_real_
    
    data.frame(Model = model_name, RMSE = round(RMSE, 2), MAE = round(MAE, 2),
               MAPE = round(MAPE, 2), sMAPE = round(sMAPE, 2))
  }
  
  metrics_full <- calc_metrics_safe(df_pred$y, df_pred$predicted, "Regime Prophet (full)")
  
  meaningful_df <- df_pred %>% filter(!is.na(y), y > 0)
  if (meaningful_exclude_covid_collapse) {
    meaningful_df <- meaningful_df %>%
      filter(!(year(ds) >= 2020 & year(ds) <= 2021 & y < 10000))
  }
  metrics_meaningful <- calc_metrics_safe(meaningful_df$y, meaningful_df$predicted, "Regime Prophet (meaningful)")
  
  metrics <- switch(
    eval_mode,
    full = metrics_full,
    meaningful = metrics_meaningful,
    both = bind_rows(metrics_full, metrics_meaningful)
  )
  
  # --- Forecast output  ---
  forecast_df <- df_pred %>%
    transmute(
      ds = as.Date(ds),
      actual = as.numeric(y),
      predicted = as.numeric(predicted),
      regime = as.character(regime)
    )
  

  last_ds <- max(df$ds)
  future_ds <- seq(from = last_ds %m+% months(1), by = "month", length.out = future_periods)
  future_fc <- predict(m3, data.frame(ds = future_ds))
  
  future_forecast_df <- data.frame(
    ds = as.Date(future_fc$ds),
    predicted = as.numeric(future_fc$yhat),
    yhat_lower = as.numeric(future_fc$yhat_lower),
    yhat_upper = as.numeric(future_fc$yhat_upper),
    model = "post_covid_m3"
  )
  
  # --- Single return ---
  list(
    models = list(pre_easter = m1, easter_covid = m2, post_covid = m3),
    combined_fit = df_pred,
    forecast_df = forecast_df,
    future_forecast_df = future_forecast_df,
    metrics = metrics
  )
}