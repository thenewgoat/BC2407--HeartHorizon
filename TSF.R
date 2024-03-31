library("TTR")               # For MA via SMA()
library("forecast")          # To generate h-period ahead forecasts
library("data.table")
library(lubridate)
library(ggplot2)
library(prophet)
library(forecast)
library(bayesforecast)

data <- read.csv("HDHI_adm_disc_analysis.csv")

# Ensure your dataset is a data.table

setDT(data) # Converts 'data' to data.table if it's not already

# ------------------------------------------------------------------------------------
#                                Creating Time-Series Object 
# ------------------------------------------------------------------------------------
# Calculate number of patients admitted per day
patient_admissions_per_day <- data[, .(number_of_patients = .N), by = admission_date]

# View the result
dim(patient_admissions_per_day)


patient_admissions_per_day <- patient_admissions_per_day[order(admission_date)]
full_dates <- seq(as.Date("2017-04-01"), as.Date("2019-03-31"), by = "day")

# Convert to data.frame if it's a data.table
patient_admissions_per_day <- as.data.frame(patient_admissions_per_day)

# Merge to ensure all dates are present
full_data <- merge(data.frame(admission_date = full_dates), patient_admissions_per_day, by = "admission_date", all.x = TRUE)
full_data$number_of_patients[is.na(full_data$number_of_patients)] <- 0


# ------------------------------------------------------------------------------------
#                                 Evaluation Function
# ------------------------------------------------------------------------------------

evaluate_and_plot_prophet <- function(model, historical_df) {
  # Ensure model is fitted
  fitted_model <- model
  
  # Generate future dataframe
  future_df <- make_future_dataframe(fitted_model, periods = 10)
  
  # Perform forecasting
  forecast <- predict(fitted_model, future_df)
  
  # Extract the forecasted values for the future period
  forecasted_values <- tail(forecast$yhat, 10)
  
  # Prepare results dataframe
  future_dates <- tail(future_df$ds, 10)
  results <- data.frame(
    ds = future_dates,
    Actual = testset$number_of_patients,
    Prophet = round(forecasted_values, 0)
  )
  
  # Plotting
  plot(fitted_model, forecast)
  prophet_plot_components(fitted_model, forecast)
  
  # Calculate accuracy metrics
  rmse <- sqrt(mean((testset$number_of_patients - forecasted_values)^2, na.rm = TRUE))
  mae <- mean(abs(testset$number_of_patients - forecasted_values), na.rm = TRUE)
  mape <- mean(abs((testset$number_of_patients - forecasted_values) / testset$number_of_patients), na.rm = TRUE) * 100
  
  # Print the metrics
  cat("Prophet Model Accuracy:\n")
  cat("RMSE:", rmse, "\nMAE:", mae, "\nMAPE:", mape, "%\n\n")
  
  # Return results and metrics
  list(
    Results = results,
    Metrics = list(RMSE = rmse, MAE = mae, MAPE = mape),
    Forecast_Plot = plot(fitted_model, forecast),
    Components_Plot = prophet_plot_components(fitted_model, forecast)
  )
}






# ------------------------------------------------------------------------------------
#                                Prophet Models 
# ------------------------------------------------------------------------------------

# Paper discussing Prophet: https://peerj.com/preprints/3190/

n <- 10

testset <- tail(patient_admissions_per_day, n)
df <- head(patient_admissions_per_day, -n)
colnames(df) <- c('ds', 'y')

df

# Generate Holidays dataframe
holidays <- generated_holidays
holidays <- holidays[holidays$year %in% c(2017,2018,2019) & holidays$country == "IN",]


# Fit the Prophet model
m1 <- prophet(df, growth = "linear",
             daily.seasonality = FALSE, 
             weekly.seasonality = TRUE,
             yearly.seasonality = TRUE, 
             seasonality.mode = "multiplicative", 
             holidays = holidays,
             holidays.prior.scale = 0.1)
#m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)

metrics_and_plots <- evaluate_and_plot_prophet(m1, df)
# Prophet Model Accuracy:
#   RMSE: 7.409749 
# MAE: 5.247417 
# MAPE: 35.56108 %


# Fit the Prophet model
m2 <- prophet(growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = TRUE, 
              seasonality.mode = "multiplicative", 
              holidays = holidays,
              holidays.prior.scale = 0.1)
m2 <- add_seasonality(m2, name='monthly', period=30.5, fourier.order=5)
m2 <- fit.prophet(m2,df)

metrics_and_plots <- evaluate_and_plot_prophet(m2, df)
# Prophet Model Accuracy:
#   RMSE: 7.322546 
# MAE: 5.301568 
# MAPE: 35.36923 %



# Fit the Prophet model
m3 <- prophet(df, growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = TRUE, 
              seasonality.mode = "multiplicative", 
              holidays = holidays,
              holidays.prior.scale = 1)
#m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)

metrics_and_plots <- evaluate_and_plot_prophet(m3, df)

# Prophet Model Accuracy:
# RMSE: 7.413439 
# MAE: 5.333105 
# MAPE: 36.6787 %


m4 <- prophet(df, growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = TRUE, 
              seasonality.mode = "multiplicative")
#m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)

metrics_and_plots <- evaluate_and_plot_prophet(m4, df)
# Prophet Model Accuracy:
#   RMSE: 7.401761 
# MAE: 5.226836 
# MAPE: 35.23344 %



m5 <- prophet(df, growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = TRUE, 
              seasonality.mode = "additive")
#m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)

metrics_and_plots <- evaluate_and_plot_prophet(m5, df)
# Prophet Model Accuracy:
#   RMSE: 7.542544 
# MAE: 5.337932 
# MAPE: 37.42082 %


m6 <- prophet(df, growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = FALSE,
              yearly.seasonality = TRUE, 
              seasonality.mode = "multiplicative")
#m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)

metrics_and_plots <- evaluate_and_plot_prophet(m6, df)
# Prophet Model Accuracy:
# RMSE: 8.645407 
# MAE: 6.379423 
# MAPE: 47.86284 %

m7 <- prophet(df, growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = FALSE, 
              seasonality.mode = "multiplicative")

metrics_and_plots <- evaluate_and_plot_prophet(m7, df)
# Prophet Model Accuracy:
#   RMSE: 8.524149 
# MAE: 6.739126 
# MAPE: 48.91233 %


m8 <- prophet(df, growth = "linear",
              daily.seasonality = TRUE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = TRUE, 
              seasonality.mode = "multiplicative")
#m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)

metrics_and_plots <- evaluate_and_plot_prophet(m8, df)
# Prophet Model Accuracy:
# RMSE: 7.393478 
# MAE: 5.184964 
# MAPE: 34.80235 %


m9 <- prophet(growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = FALSE, 
              seasonality.mode = "multiplicative")
m9 <- add_seasonality(m9, name='monthly', period=30.5, fourier.order=1)
m9 <- fit.prophet(m9, df)
metrics_and_plots <- evaluate_and_plot_prophet(m9, df)
# Prophet Model Accuracy:
#   RMSE: 8.640175 
# MAE: 6.745076 
# MAPE: 50.32526 %

m10 <- prophet(df, growth = "linear",
              daily.seasonality = FALSE, 
              weekly.seasonality = TRUE,
              yearly.seasonality = FALSE, 
              seasonality.mode = "additive")

metrics_and_plots <- evaluate_and_plot_prophet(m10, df)

# ------------------------------------------------------------------------------------
#                                Cross-Validation
# ------------------------------------------------------------------------------------

df1.cv <- cross_validation(m1, horizon = 10, units = 'days')
df1.p <- performance_metrics(df.cv)
average_rmse_df1 <- mean(df1.p$rmse, na.rm = TRUE)
average_rmse_df1

df2.cv <- cross_validation(m2, horizon = 10, units = 'days')
df2.p <- performance_metrics(df2.cv)
df2.p
average_rmse_df2 <- mean(df2.p$rmse, na.rm = TRUE)

df3.cv <- cross_validation(m3, horizon = 10, units = 'days')
df3.p <- performance_metrics(df3.cv)
df3.p
average_rmse_df3 <- mean(df3.p$rmse, na.rm = TRUE)

df4.cv <- cross_validation(m4, horizon = 10, units = 'days')
df4.p <- performance_metrics(df4.cv)
df4.p
average_rmse_df4 <- mean(df4.p$rmse, na.rm = TRUE)

df5.cv <- cross_validation(m5, horizon = 10, units = 'days')
df5.p <- performance_metrics(df5.cv)
df5.p
average_rmse_df5 <- mean(df5.p$rmse, na.rm = TRUE)

df6.cv <- cross_validation(m6, horizon = 10, units = 'days')
df6.p <- performance_metrics(df6.cv)
df6.p
average_rmse_df6 <- mean(df6.p$rmse, na.rm = TRUE)

df7.cv <- cross_validation(m7, horizon = 10, units = 'days')
df7.p <- performance_metrics(df7.cv)
df7.p
average_rmse_df7 <- mean(df7.p$rmse, na.rm = TRUE)

df8.cv <- cross_validation(m8, horizon = 10, units = 'days')
df8.p <- performance_metrics(df8.cv)
df8.p
average_rmse_df8 <- mean(df8.p$rmse, na.rm = TRUE)

df9.cv <- cross_validation(m9, horizon = 10, units = 'days')
df9.p <- performance_metrics(df9.cv)
df9.p
average_rmse_df9 <- mean(df9.p$rmse, na.rm = TRUE)

df10.cv <- cross_validation(m10, horizon = 10, units = 'days')
df10.p <- performance_metrics(df10.cv)
df10.p
average_rmse_df10 <- mean(df10.p$rmse, na.rm = TRUE)

# Create a dataframe for the average RMSE values
average_rmse_df <- data.frame(
  Model = c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9","m10"),
  Average_RMSE = c(average_rmse_df1, average_rmse_df2, average_rmse_df3,
                   average_rmse_df4, average_rmse_df5, average_rmse_df6,
                   average_rmse_df7, average_rmse_df8, average_rmse_df9,
                   average_rmse_df10)
)

# View the dataframe
print(average_rmse_df)


df11.cv <- cross_validation(m10, horizon = 30, units = 'days')
df11.p <- performance_metrics(df11.cv)
df11.p
average_rmse_df11 <- mean(df11.p$rmse, na.rm = TRUE)
average_rmse_df11
