# analysis/main.R
# Project structure:
# - analysis/main.R
# - models/data_prep.R
# - models/arima_model.R
# - models/prophet_normal_model.R
# - models/prophet_seperate_model.R
# - data/Tourist Arrivals (2014 Jan - 2025 Dec).xlsx
#
# Adds: TS plots, ACF/PACF, stationarity tests, saved outputs.


suppressPackageStartupMessages({
  library(here)
  library(forecast)
  library(tseries)
  library(ragg)
})

here::i_am("analysis/main.R")
cat("Project root:", here(), "\n")

# ---- output folders ----
dir.create(here("outputs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "results"), showWarnings = FALSE, recursive = TRUE)

# ---- Safe source helper ----
safe_source <- function(rel_path) {
  full_path <- here(rel_path)
  if (!file.exists(full_path)) {
    stop(
      "Missing file: ", full_path, "\n",
      "Check that the file exists and that the path is correct from the project root."
    )
  }
  source(full_path, local = FALSE)
}

# ---- Source modules from models/ ----
safe_source("models/data_prep.R")
safe_source("models/arima_model.R")
safe_source("models/prophet_normal_model.R")
safe_source("models/prophet_seperate_model.R")

# ---- Excel data file ----
file_path <- here("data", "Tourist Arrivals (2014 Jan - 2025 Dec).xlsx")
if (!file.exists(file_path)) {
  stop("Missing Excel file: ", file_path)
}

# ---- Load and prepare data ----
data_list <- load_and_prepare_data(file_path)

train_ts   <- data_list$train_ts
test_ts    <- data_list$test_ts
train_data <- data_list$train_data
test_data  <- data_list$test_data
prophet_df <- data_list$prophet_df

full_ts <- ts(
  c(as.numeric(train_ts), as.numeric(test_ts)),
  start = start(train_ts),
  frequency = frequency(train_ts)
)

# ---- Sanity checks ----
cat("\n===== Data sanity =====\n")
cat("train_ts class:", paste(class(train_ts), collapse = ","), "\n")
cat("train_ts frequency:", frequency(train_ts), "\n")
cat("train_ts start/end:", paste(start(train_ts), collapse = "-"), "to", paste(end(train_ts), collapse = "-"), "\n")
cat("test_ts  start/end:", paste(start(test_ts), collapse = "-"), "to", paste(end(test_ts), collapse = "-"), "\n")
cat("train_ts range:", min(train_ts, na.rm = TRUE), "to", max(train_ts, na.rm = TRUE), "\n")
cat("test_ts  range:", min(test_ts, na.rm = TRUE), "to", max(test_ts, na.rm = TRUE), "\n")

# ============================================================
# 1) Exploratory plots (saved)
# ============================================================
plot_path_ts <- here("outputs", "plots", "01_time_series_overview.png")
png(plot_path_ts, width = 1400, height = 700)
plot(full_ts,
     main = "Sri Lanka Tourist Arrivals (Monthly)",
     xlab = "Year", ylab = "Arrivals")
abline(v = time(train_ts)[length(train_ts)], col = "red", lwd = 2)
legend("topleft", legend = c("Train/Test split"), col = c("red"), lwd = 2, bty = "n")
dev.off()

plot_path_season <- here("outputs", "plots", "02_seasonal_plot.png")
png(plot_path_season, width = 1400, height = 700)
seasonplot(full_ts, year.labels = TRUE, main = "Seasonal Plot (Month-of-year pattern)")
dev.off()

# ACF/PACF raw
plot_path_acf_pacf_raw <- here("outputs", "plots", "03_acf_pacf_raw.png")
png(plot_path_acf_pacf_raw, width = 1400, height = 700)
par(mfrow = c(1, 2))
acf(full_ts, main = "ACF (raw)", lag.max = 48, na.action = na.pass)
pacf(full_ts, main = "PACF (raw)", lag.max = 48, na.action = na.pass)
par(mfrow = c(1, 1))
dev.off()

# ACF/PACF log1p (safe with zeros)
full_ts_log <- log1p(pmax(full_ts, 0))
plot_path_acf_pacf_log <- here("outputs", "plots", "04_acf_pacf_log1p.png")
png(plot_path_acf_pacf_log, width = 1400, height = 700)
par(mfrow = c(1, 2))
acf(full_ts_log, main = "ACF (log1p)", lag.max = 48, na.action = na.pass)
pacf(full_ts_log, main = "PACF (log1p)", lag.max = 48, na.action = na.pass)
par(mfrow = c(1, 1))
dev.off()

# ACF/PACF differenced (common for ARIMA)
full_ts_log_d1 <- diff(full_ts_log, differences = 1)
full_ts_log_d1_D12 <- diff(full_ts_log_d1, lag = 12)

plot_path_acf_pacf_diff <- here("outputs", "plots", "05_acf_pacf_diff.png")
png(plot_path_acf_pacf_diff, width = 1400, height = 700)
par(mfrow = c(1, 2))
acf(full_ts_log_d1_D12, main = "ACF (diff 1 + seasonal diff 12)", lag.max = 48, na.action = na.pass)
pacf(full_ts_log_d1_D12, main = "PACF (diff 1 + seasonal diff 12)", lag.max = 48, na.action = na.pass)
par(mfrow = c(1, 1))
dev.off()

# ============================================================
# 2) Stationarity tests (saved)
# ============================================================
# Notes:
# - ADF: H0 = unit root (non-stationary)
# - KPSS: H0 = stationary
run_tests <- function(x, series_name) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 20) {
    return(data.frame(
      series = series_name,
      adf_p_value = NA_real_,
      kpss_p_value = NA_real_,
      note = "Not enough observations for reliable tests"
    ))
  }

  adf_p <- tryCatch(adf.test(x)$p.value, error = function(e) NA_real_)
  kpss_p <- tryCatch(kpss.test(x)$p.value, error = function(e) NA_real_)

  data.frame(
    series = series_name,
    adf_p_value = adf_p,
    kpss_p_value = kpss_p,
    note = ""
  )
}

stationarity_results <- rbind(
  run_tests(full_ts, "raw"),
  run_tests(full_ts_log, "log1p"),
  run_tests(full_ts_log_d1_D12, "log1p + diff(1) + seasonal diff(12)")
)

write.csv(
  stationarity_results,
  file = here("outputs", "results", "stationarity_tests.csv"),
  row.names = FALSE
)

cat("\n===== Stationarity tests saved to outputs/results/stationarity_tests.csv =====\n")
print(stationarity_results)

# ============================================================
# 3) Run models
# ============================================================
cat("\n===== ARIMA Model =====\n")
arima_results <- run_arima_model(train_ts, test_ts)  # your function decides transformation
print(arima_results$metrics)

cat("\n===== Prophet Model =====\n")
prophet_results <- run_prophet_model(train_data, test_data, use_log1p = TRUE)
print(prophet_results$metrics)

cat("\n===== Regime Prophet Model =====\n")
regime_results <- run_regime_prophet(
  prophet_df,
  eval_mode = "both",
  add_monthly_seasonality = FALSE
)
print(regime_results$metrics)

# ============================================================
# 4) Standardize + export metrics
# ============================================================
standardize_metrics <- function(df) {
  needed <- c("Model", "ME", "RMSE", "MAE", "MAPE", "sMAPE")
  for (col in needed) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }
  df <- df[, needed]
  for (col in setdiff(needed, "Model")) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df
}

all_metrics <- rbind(
  standardize_metrics(arima_results$metrics),
  standardize_metrics(prophet_results$metrics),
  standardize_metrics(regime_results$metrics)
)

cat("\n===== All Models Comparison =====\n")
print(all_metrics)

write.csv(
  all_metrics,
  file = here("outputs", "results", "model_metrics.csv"),
  row.names = FALSE
)

cat("\nSaved metrics to outputs/results/model_metrics.csv\n")

# ---- Optional preview ----
cat("\n===== Forecast preview =====\n")
if (!is.null(arima_results$actual) && !is.null(arima_results$predicted)) {
  print(head(data.frame(actual = arima_results$actual, predicted = arima_results$predicted), 5))
}
if (!is.null(prophet_results$actual) && !is.null(prophet_results$predicted)) {
  print(head(data.frame(actual = prophet_results$actual, predicted = prophet_results$predicted), 5))
}