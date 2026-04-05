# Sri Lanka Tourism Arrival Forecasting

## Project Summary
This project forecasts monthly tourist arrivals to Sri Lanka using historical arrival data from January 2014 to December 2025.  
It compares three forecasting approaches:

- ARIMA
- Prophet
- Regime Prophet

The goal is to identify the most practical model for tourism arrival forecasting and compare performance using standard accuracy metrics.

## Objective
The main objective of this project is to build and evaluate time series forecasting models for Sri Lanka tourism arrivals, so the results can support planning, decision-making, and trend analysis.

## Data Description
The project uses monthly Sri Lanka tourism arrival data from **2014-01 to 2025-12**.

The source data is stored in the `data/` directory as an Excel file:
- `data/Tourist Arrivals (2014 Jan - 2025 Dec).xlsx`

## Methods Used

### 1. ARIMA
ARIMA is used as a classical statistical forecasting method for monthly time series data.

### 2. Prophet
Prophet is used as a flexible forecasting model that handles trend and seasonality well.

### 3. Regime Prophet
Regime Prophet is a segmented Prophet approach that fits different behavior patterns across different time periods.

## Repository Structure

- `analysis/`  
  Main analysis scripts, including the entry point `analysis/main.R`

- `models/`  
  Model scripts for ARIMA, Prophet, and Regime Prophet

- `data/`  
  Source dataset files

- `outputs/`  
  Generated results, plots, and forecast outputs

## Workflow
The main workflow is controlled by:

- `analysis/main.R`

This script:
1. Loads the data
2. Splits it into training and testing sets
3. Runs ARIMA, Prophet, and Regime Prophet
4. Compares model performance
5. Saves plots and forecast outputs

## Evaluation Metrics
The models are compared using the following metrics:

- **ME**: Mean Error
- **RMSE**: Root Mean Squared Error
- **MAE**: Mean Absolute Error
- **MAPE**: Mean Absolute Percentage Error
- **sMAPE**: Symmetric Mean Absolute Percentage Error

### How to interpret them
- Lower **RMSE**, **MAE**, **MAPE**, and **sMAPE** are better
- **ME** shows whether the model tends to over-forecast or under-forecast
- A value of `NA` may appear if a metric is not computed for a model or if the calculation is not applicable

## How to Run the Project

### 1. Install R packages
Make sure these packages are installed:

```r
install.packages(c("here", "forecast", "tseries", "dplyr", "lubridate"))
install.packages("prophet")