# main.R

# Source all modules
source("data_preparation.R")
source("arima_model.R")
source("prophet_normal_model.R")
source("prophet_seperate_model.R")

# Load and prepare data
file_path <- "data/Tourist Arrivals (2014 Jan - 2025 Dec).xlsx"
data_list <- load_and_prepare_data(file_path)

# Extract needed objects
train_ts <- data_list$train_ts
test_ts  <- data_list$test_ts
train_data <- data_list$train_data
test_data  <- data_list$test_data
prophet_df <- data_list$prophet_df

# Run ARIMA model
cat("\n===== ARIMA Model =====\n")
arima_results <- run_arima_model(train_ts, test_ts)
print(arima_results$metrics)
# Optional: plot forecast
# plot(arima_results$forecast)

# Run Prophet model
cat("\n===== Prophet Model =====\n")
prophet_results <- run_prophet_model(train_data, test_data)
print(prophet_results$metrics)

# Run Regime Prophet model
cat("\n===== Regime Prophet Model =====\n")
regime_results <- run_regime_prophet(prophet_df)
print(regime_results$metrics)

# Combine all metrics for comparison
all_metrics <- rbind(
  arima_results$metrics,
  prophet_results$metrics,
  regime_results$metrics
)
print(all_metrics)

# Optional: plots can be added here to visualize forecasts vs actuals for each model.