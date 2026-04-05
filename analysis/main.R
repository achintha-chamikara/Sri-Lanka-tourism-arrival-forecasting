suppressPackageStartupMessages({
  library(here)
  library(forecast)
  library(tseries)
})

here::i_am("analysis/main.R")
cat("Project root:", here(), "\n")

# ------------------------------------------------------------
# Output directories
# ------------------------------------------------------------
plots_dir   <- here("outputs", "plots")
results_dir <- here("outputs", "results")
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

cat("Plots dir   :", plots_dir, "\n")
cat("Results dir :", results_dir, "\n")

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
safe_source <- function(rel_path) {
  fp <- here(rel_path)
  if (!file.exists(fp)) stop("Missing file: ", fp)
  source(fp, local = FALSE)
}

close_all_devices <- function() {
  while (grDevices::dev.cur() > 1) try(grDevices::dev.off(), silent = TRUE)
}

save_plot_safe <- function(filename_base, plot_fn, width_px = 1400, height_px = 700, res = 144) {
  png_path <- file.path(plots_dir, paste0(filename_base, ".png"))
  pdf_path <- file.path(plots_dir, paste0(filename_base, ".pdf"))

  close_all_devices()

  ok_png <- tryCatch({
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(png_path, width = width_px, height = height_px, res = res)
    } else {
      png(png_path, width = width_px, height = height_px, res = res)
    }
    plot_fn()
    tryCatch({ grDevices::dev.off(); TRUE }, error = function(e) FALSE)
  }, error = function(e) FALSE)

  if (!ok_png) {
    close_all_devices()
    pdf(pdf_path, width = 12, height = 6)
    plot_fn()
    grDevices::dev.off()
    message("Saved PDF instead: ", pdf_path)
  } else {
    message("Saved PNG: ", png_path)
  }
}

save_rds <- function(object, filename) {
  path <- file.path(results_dir, filename)
  saveRDS(object, file = path)
  cat("Saved RDS:", path, "\n")
  path
}

try_save_csv <- function(df, filename) {
  path <- file.path(results_dir, filename)
  ok <- tryCatch({
    write.csv(df, file = path, row.names = FALSE)
    TRUE
  }, error = function(e) {
    message("CSV write failed (OneDrive lock likely): ", e$message)
    FALSE
  })
  if (ok) cat("Saved CSV:", path, "\n")
  invisible(ok)
}

standardize_metrics <- function(df) {
  needed <- c("Model", "ME", "RMSE", "MAE", "MAPE", "sMAPE")
  for (col in needed) if (!col %in% names(df)) df[[col]] <- NA_real_
  df <- df[, needed, drop = FALSE]
  for (col in setdiff(needed, "Model")) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df
}

# Extract forecast values from regime_results if available.
extract_regime_forecast_df <- function(regime_results, test_data = NULL) {
  if (!is.null(regime_results$forecast_df) && is.data.frame(regime_results$forecast_df)) {
    return(regime_results$forecast_df)
  }

  if (!is.null(regime_results$actual) && !is.null(regime_results$predicted)) {
    ds <- NULL
    if (!is.null(regime_results$ds)) ds <- as.Date(regime_results$ds)
    if (is.null(ds) && !is.null(test_data) && "ds" %in% names(test_data)) ds <- as.Date(test_data$ds)
    if (is.null(ds)) ds <- seq_along(regime_results$actual)

    return(data.frame(
      ds = ds,
      actual = as.numeric(regime_results$actual),
      predicted = as.numeric(regime_results$predicted)
    ))
  }

  if (!is.null(regime_results$forecast) && is.data.frame(regime_results$forecast) &&
      all(c("ds", "yhat") %in% names(regime_results$forecast))) {
    df <- regime_results$forecast
    actual <- if (!is.null(test_data) && "y" %in% names(test_data)) as.numeric(test_data$y) else NA_real_
    return(data.frame(
      ds = as.Date(df$ds),
      actual = actual,
      predicted = as.numeric(df$yhat)
    ))
  }

  NULL
}

# ------------------------------------------------------------
# Source your modules
# ------------------------------------------------------------
safe_source("models/data_prep.R")
safe_source("models/arima_model.R")
safe_source("models/prophet_normal_model.R")
safe_source("models/prophet_seperate_model.R")

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
file_path <- here("data", "Tourist Arrivals (2014 Jan - 2025 Dec).xlsx")
if (!file.exists(file_path)) stop("Missing Excel file: ", file_path)

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

# ------------------------------------------------------------
# Sanity checks
# ------------------------------------------------------------
cat("\n===== Data sanity =====\n")
cat("train_ts class:", paste(class(train_ts), collapse = ","), "\n")
cat("train_ts frequency:", frequency(train_ts), "\n")
cat("train_ts start/end:", paste(start(train_ts), collapse = "-"), "to", paste(end(train_ts), collapse = "-"), "\n")
cat("test_ts  start/end:", paste(start(test_ts), collapse = "-"), "to", paste(end(test_ts), collapse = "-"), "\n")
cat("train_ts range:", min(train_ts, na.rm = TRUE), "to", max(train_ts, na.rm = TRUE), "\n")
cat("test_ts  range:", min(test_ts, na.rm = TRUE), "to", max(test_ts, na.rm = TRUE), "\n")

# ------------------------------------------------------------
# Plots
# ------------------------------------------------------------
save_plot_safe("01_time_series_overview", function() {
  plot(full_ts, main = "Tourist Arrivals (Monthly)", xlab = "Year", ylab = "Arrivals")
  abline(v = time(train_ts)[length(train_ts)], col = "red", lwd = 2)
  legend("topleft", legend = "Train/Test split", col = "red", lwd = 2, bty = "n")
})

save_plot_safe("02_seasonal_plot", function() {
  seasonplot(full_ts, year.labels = TRUE, main = "Seasonal Plot (Month-of-year)")
})

save_plot_safe("03_acf_pacf_raw", function() {
  par(mfrow = c(1, 2))
  acf(full_ts, main = "ACF (raw)", lag.max = 48, na.action = na.pass)
  pacf(full_ts, main = "PACF (raw)", lag.max = 48, na.action = na.pass)
  par(mfrow = c(1, 1))
})

full_ts_d1 <- diff(full_ts, 1)
full_ts_d1_D12 <- diff(full_ts_d1, lag = 12)

save_plot_safe("04_acf_pacf_diff", function() {
  par(mfrow = c(1, 2))
  acf(full_ts_d1_D12, main = "ACF (diff 1 + seasonal diff 12)", lag.max = 48, na.action = na.pass)
  pacf(full_ts_d1_D12, main = "PACF (diff 1 + seasonal diff 12)", lag.max = 48, na.action = na.pass)
  par(mfrow = c(1, 1))
})

# ------------------------------------------------------------
# Stationarity tests
# ------------------------------------------------------------
run_tests <- function(x, series_name) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  data.frame(
    series = series_name,
    adf_p_value  = tryCatch(adf.test(x)$p.value,  error = function(e) NA_real_),
    kpss_p_value = tryCatch(kpss.test(x)$p.value, error = function(e) NA_real_)
  )
}

stationarity_results <- rbind(
  run_tests(full_ts, "raw"),
  run_tests(full_ts_d1_D12, "diff(1)+diff(12)")
)

save_rds(stationarity_results, "stationarity_tests.rds")
try_save_csv(stationarity_results, "stationarity_tests.csv")

# ------------------------------------------------------------
# Run models
# ------------------------------------------------------------
cat("\n===== ARIMA Model =====\n")
arima_results <- run_arima_model(train_ts, test_ts)
print(arima_results$metrics)

cat("\n===== Prophet Model =====\n")
prophet_results <- run_prophet_model(train_data, test_data, use_log1p = FALSE)
print(prophet_results$metrics)

cat("\n===== Regime Prophet Model =====\n")
regime_results <- run_regime_prophet(
  prophet_df,
  eval_mode = "meaningful",
  add_monthly_seasonality = FALSE
)
print(regime_results$metrics)

# ------------------------------------------------------------
# Metrics comparison
# ------------------------------------------------------------
all_metrics <- rbind(
  standardize_metrics(arima_results$metrics),
  standardize_metrics(prophet_results$metrics),
  standardize_metrics(regime_results$metrics)
)

cat("\n===== All Models Comparison =====\n")
print(all_metrics)

save_rds(all_metrics, "model_metrics.rds")
try_save_csv(all_metrics, "model_metrics.csv")

# ------------------------------------------------------------
# Save forecast values for ALL models
# ------------------------------------------------------------
if (!is.null(arima_results$forecast_test_df)) {
  save_rds(arima_results$forecast_test_df, "arima_forecast_TEST.rds")
  try_save_csv(arima_results$forecast_test_df, "arima_forecast_TEST.csv")
}
if (!is.null(arima_results$forecast_future_df)) {
  save_rds(arima_results$forecast_future_df, "arima_forecast_FUTURE.rds")
  try_save_csv(arima_results$forecast_future_df, "arima_forecast_FUTURE.csv")
}

if (!is.null(prophet_results$forecast_test_df)) {
  save_rds(prophet_results$forecast_test_df, "prophet_forecast_TEST.rds")
  try_save_csv(prophet_results$forecast_test_df, "prophet_forecast_TEST.csv")
}
if (!is.null(prophet_results$forecast_future_df)) {
  save_rds(prophet_results$forecast_future_df, "prophet_forecast_FUTURE.rds")
  try_save_csv(prophet_results$forecast_future_df, "prophet_forecast_FUTURE.csv")
}

if (!is.null(regime_results$forecast_df)) {
  save_rds(regime_results$forecast_df, "regime_forecast_FULL.rds")
  try_save_csv(regime_results$forecast_df, "regime_forecast_FULL.csv")
}
if (!is.null(regime_results$future_forecast_df)) {
  save_rds(regime_results$future_forecast_df, "regime_forecast_FUTURE.rds")
  try_save_csv(regime_results$future_forecast_df, "regime_forecast_FUTURE.csv")
}

# Also save a best-effort extracted forecast from regime model if available
best_fc <- extract_regime_forecast_df(regime_results, test_data = test_data)
if (!is.null(best_fc)) {
  save_rds(best_fc, "best_model_forecast.rds")
  try_save_csv(best_fc, "best_model_forecast.csv")
  cat("\nSaved best model forecast values.\n")
} else {
  msg <- c(
    "Could not extract forecast values from regime_results.",
    "Reason: run_regime_prophet() is not returning forecast outputs in a compatible format."
  )
  writeLines(msg, con = file.path(results_dir, "best_model_forecast_README.txt"))
  cat("\nSaved best_model_forecast_README.txt explaining the issue.\n")
}

# ------------------------------------------------------------
# Run summary
# ------------------------------------------------------------
summary_lines <- c(
  paste0("Run date/time: ", Sys.time()),
  paste0("Project root: ", here()),
  paste0("Plots dir: ", plots_dir),
  paste0("Results dir: ", results_dir)
)
writeLines(summary_lines, con = file.path(results_dir, "run_summary.txt"))
cat("\nSaved run_summary.txt\n")