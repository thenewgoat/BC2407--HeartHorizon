library("TTR")               # For MA via SMA()
library("forecast")          # To generate h-period ahead forecasts
library("data.table")
library(lubridate)
library(ggplot2)
library(prophet)

data <- read.csv("HDHI_adm_disc_analysis.csv")

# Ensure your dataset is a data.table

setDT(data) # Converts 'data' to data.table if it's not already

# ==========
# Daily ------------------------------------------------------------------------------------
# ==========
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



n <- 10
testset <- tail(patient_admissions_per_day, n)
df <- head(patient_admissions_per_day, -n)
colnames(df) <- c('ds', 'y')

df

# Generate Holidays dataframe
holidays <- generated_holidays
holidays <- holidays[holidays$year %in% c(2017,2018,2019) & holidays$country == "IN",]


# Fit the Prophet model
m <- prophet(df,
             growth = "linear",
             daily.seasonality = FALSE, 
             weekly.seasonality = TRUE,
             yearly.seasonality = TRUE, 
             seasonality.mode = "multiplicative", 
             holidays = holidays)

# Make a dataframe for future dates for forecasting
future <- make_future_dataframe(m, periods = n) # 2 weeks

# Forecast
forecast <- predict(m, future)
View(forecast)

# Plot the forecast
plot(m, forecast)
prophet_plot_components(m, forecast)

forecasted_values <- tail(forecast$yhat, n)

# If you want to view these forecasted values in R, you can simply print them
print(forecasted_values)

testset

rmse <- sqrt(mean((testset$number_of_patients - round(forecasted_values,0)) ^ 2))

# Print the RMSE
print(rmse)

testset$forecasted_admissions <- round(forecasted_values,0)

testset