# analysis/main.R
# Full script for this project structure:
# - analysis/main.R
# - models/data_prep.R
# - models/arima_model.R
# - models/prophet_normal_model.R
# - models/prophet_seperate_model.R
# - data/Tourist Arrivals (2014 Jan - 2025 Dec).xlsx

suppressPackageStartupMessages({
  library(here)
})

# Anchor project root correctly from analysis/main.R
here::i_am("analysis/main.R")

cat("Project root:", here(), "\n")

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
  stop(
    "Missing Excel file: ", file_path, "\n",
    "Expected it under the data/ folder in the project root."
  )
}

# ---- Load and prepare data ----
data_list <- load_and_prepare_data(file_path)

train_ts   <- data_list$train_ts
test_ts    <- data_list$test_ts
train_data <- data_list$train_data
test_data  <- data_list$test_data
prophet_df <- data_list$prophet_df

# ---- Sanity checks ----
cat("\n===== Data sanity =====\n")
cat("train_ts class:", paste(class(train_ts), collapse = ","), "\n")
cat("train_ts frequency:", frequency(train_ts), "\n")
cat("train_ts start/end:", paste(start(train_ts), collapse = "-"), "to", paste(end(train_ts), collapse = "-"), "\n")
cat("test_ts  start/end:", paste(start(test_ts), collapse = "-"), "to", paste(end(test_ts), collapse = "-"), "\n")
cat("train_ts range:", min(train_ts, na.rm = TRUE), "to", max(train_ts, na.rm = TRUE), "\n")
cat("test_ts  range:", min(test_ts, na.rm = TRUE), "to", max(test_ts, na.rm = TRUE), "\n")

# ---- Run models ----
cat("\n===== ARIMA Model =====\n")
arima_results <- run_arima_model(train_ts, test_ts)
print(arima_results$metrics)

cat("\n===== Prophet Model =====\n")
# For monthly data, keep add_monthly_seasonality = FALSE
prophet_results <- run_prophet_model(train_data, test_data, use_log1p = TRUE)
print(prophet_results$metrics)

cat("\n===== Regime Prophet Model =====\n")
# For monthly data, keep add_monthly_seasonality = FALSE
regime_results <- run_regime_prophet(
  prophet_df,
  eval_mode = "both",
  add_monthly_seasonality = FALSE
)
print(regime_results$metrics)

# ---- Standardize metrics so rbind never fails ----
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

# ---- Optional quick check of predictions ----
cat("\n===== Forecast preview =====\n")
if (!is.null(arima_results$actual) && !is.null(arima_results$predicted)) {
  print(head(data.frame(actual = arima_results$actual, predicted = arima_results$predicted), 5))
}
if (!is.null(prophet_results$actual) && !is.null(prophet_results$predicted)) {
  print(head(data.frame(actual = prophet_results$actual, predicted = prophet_results$predicted), 5))
}