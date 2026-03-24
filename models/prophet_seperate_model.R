# regime_prophet.R

library(prophet)
library(dplyr)
library(lubridate)
library(Metrics)

run_regime_prophet <- function(prophet_df) {
  # Split into three regimes
  pre_easter <- prophet_df %>% filter(year(ds) < 2019) %>% mutate(regime = "pre_easter")
  easter_covid <- prophet_df %>% filter(year(ds) >= 2019 & year(ds) <= 2021) %>% mutate(regime = "easter_covid")
  post_covid <- prophet_df %>% filter(year(ds) >= 2022) %>% mutate(regime = "post_covid")
  
  # Model 1: Pre‑Easter (2014–2018)
  m1 <- prophet(
    pre_easter[, c("ds", "y")],
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",
    changepoint.prior.scale = 0.05,
    seasonality.prior.scale = 10
  )
  
  # Model 2: Easter + COVID (2019–2021)
  # Define holidays (easter attack, COVID)
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
  holidays_m2 <- rbind(easter_attack, covid_lockdown)
  
  m2 <- prophet(
    easter_covid[, c("ds", "y")],
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "additive",
    changepoint.prior.scale = 0.001,
    seasonality.prior.scale = 0.1,
    holidays = holidays_m2,
    holidays.prior.scale = 10
  )
  
  # Model 3: Post‑COVID (2022 onward)
  m3 <- prophet(
    post_covid[, c("ds", "y")],
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",
    changepoint.prior.scale = 0.1,
    seasonality.prior.scale = 15
  )
  
  # Function to get predictions for any date using the appropriate model
  get_prediction <- function(date) {
    if (date < as.Date("2019-04-01")) {
      pred <- predict(m1, data.frame(ds = date))$yhat[1]
    } else if (date >= as.Date("2019-04-01") & date <= as.Date("2021-12-31")) {
      pred <- predict(m2, data.frame(ds = date))$yhat[1]
    } else {
      pred <- predict(m3, data.frame(ds = date))$yhat[1]
    }
    return(pred)
  }
  
  # Apply to all data points to get combined fit
  combined_fit <- prophet_df %>%
    mutate(predicted = sapply(ds, get_prediction))
  
  # Evaluate on meaningful data (non‑zero, excluding COVID collapse)
  meaningful <- combined_fit %>%
    filter(y > 0, !(year(ds) >= 2020 & year(ds) <= 2021 & y < 10000))
  
  actual_valid <- meaningful$y
  pred_valid <- meaningful$predicted
  
  if (nrow(meaningful) > 0) {
    RMSE <- sqrt(mean((actual_valid - pred_valid)^2))
    MAE  <- mean(abs(actual_valid - pred_valid))
    MAPE <- mean(abs((actual_valid - pred_valid) / actual_valid)) * 100
    sMAPE <- mean(200 * abs(actual_valid - pred_valid) / 
                    (abs(actual_valid) + abs(pred_valid)))
    
    metrics <- data.frame(
      Model = "Regime Prophet",
      RMSE = round(RMSE, 2),
      MAE = round(MAE, 2),
      MAPE = round(MAPE, 2),
      sMAPE = round(sMAPE, 2)
    )
  } else {
    metrics <- data.frame(Model = "Regime Prophet", RMSE = NA, MAE = NA, MAPE = NA, sMAPE = NA)
  }
  
  list(
    models = list(pre_easter = m1, easter_covid = m2, post_covid = m3),
    combined_fit = combined_fit,
    metrics = metrics
  )
}
