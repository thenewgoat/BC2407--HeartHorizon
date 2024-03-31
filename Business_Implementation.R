setwd("C:/Users/zhang/Desktop/NTU/Y2S2/BC2407/Project Files/Final R Script")

# -----------------------------------------------------------------------
#                                 Load Packages
# -----------------------------------------------------------------------

library(data.table)
library(lubridate)
library(dplyr)
library(caret)
library(moments)
library(corrplot)
library(randomForest)
library(prophet)

# -----------------------------------------------------------------------
#                                 Get Models
# -----------------------------------------------------------------------

# Get Duration of Stay Model
load("DoS_Model.RData")
DoS_model <- m.RF.1

# TSF model will be rebuilt and updated everyday based on moving data.


# -----------------------------------------------------------------------
#                                 Get Data
# -----------------------------------------------------------------------
# Past Records
load("Past.RData")
data

# New admissions
adm <- fread("1-Apr-2019.csv")


# Past Predictions 

# ystd <- fread.csv("31-Mar-2019_pred.csv")

#       - For actual application this will be in the form of a csv file,
#         based on the output of the previous day's iteration of this 
#         script.

#         For the purposes of demonstration, the prediction will be generated
#         now, instead of being done yesterday. This will also serve as a demonstration
#         of how we intend to initialise the model.

past <- subset(data, ADMISSION_DATE > as.Date("2019-03-16"))

past_pred <- predict(m.RF.1, past, type = "response")
View(past_pred)

predicted_stays <- data.table(admission_date = past$ADMISSION_DATE, duration = round(past_pred,0))
predicted_stays
# Calculate discharge date
predicted_stays$discharge_date <- predicted_stays$admission_date + predicted_stays$duration

# Create a sequence of dates from March 17th to the last discharge date
seq_dates <- seq(from = as.Date("2019-03-17"), 
                 to = max(predicted_stays$discharge_date), 
                 by = "day")

# Initialize a vector to hold the bed occupancy count for each day
bed_occupancy <- numeric(length(seq_dates))

# Calculate bed occupancy for each day in the sequence
for (i in 1:length(seq_dates)) {
  current_date <- seq_dates[i]
  # Count how many patients are in the hospital on 'current_date'
  bed_occupancy[i] <- sum(predicted_stays$admission_date <= current_date & predicted_stays$discharge_date >= current_date)
}

# Create a data.frame for plotting
occupancy_df <- data.frame(date = seq_dates, occupancy = bed_occupancy)
occupancy_df

ggplot(occupancy_df, aes(x = date, y = occupancy)) +
  geom_line() + 
  labs(title = "Daily Bed Occupancy for Heart Disease Patients",
       x = "Date", y = "Bed Occupancy") +
  theme_minimal()
# Graph interpretation
#       - Initial Rise due to lack of previous data, bed occupancy
#         stabilises closer to March 31st (cut off point)

#       - With no new arrivals, bed occupancy falls off drastically. 
#         This will be filled in with TSF projections.


# Assumption: 
# All patients are discharged precisely on their projected discharge dates. For
# future models, a daily update on patient situation can be implemented when more
# data is available.

# Possible Extensions:
#     - Daily Updates on predictions
#     - Account for actual discharge dates
  
# -----------------------------------------------------------------------
#                       Time Series Forecasting
# -----------------------------------------------------------------------


# Source the past TSF model (please update with ~/TSF.R)
# source("C:/Users/zhang/Desktop/NTU/Y2S2/BC2407/Project Files/Final R Script/TSF.R")

# Update Admissions with new data
setDT(adm)
adm$admission_date <- as.Date(adm$ADMISSION_DATE, "%d/%m/%Y")
patient_admissions_today <- adm[, .(number_of_patients = .N),by = admission_date]
patient_admissions_today <- as.data.frame(patient_admissions_today)
patient_admissions_today$admission_date <- as.character(patient_admissions_today$admission_date)
patient_admissions_today

colnames(patient_admissions_per_day) <- c('admission_date', 'number_of_patients')
patient_admissions_per_day <- rbind(patient_admissions_per_day, patient_admissions_today)

patient_admissions_per_day$admission_date <- as.Date(patient_admissions_per_day$admission_date)
tail(patient_admissions_per_day)


# Update TSF model
colnames(patient_admissions_per_day) <- c('ds', 'y')

m <- prophet(patient_admissions_per_day,
             growth = "linear",
             daily.seasonality = FALSE, 
             weekly.seasonality = TRUE,
             yearly.seasonality = FALSE, 
             seasonality.mode = "additive")

future <- make_future_dataframe(m, periods = 10) # 2 weeks
forecast <- predict(m, future)


forecasted_values <- data.frame(admission_date = as.Date(tail(forecast$ds,10)), number_of_patients = tail(round(forecast$yhat,0), 10))
forecasted_values

forecasted_values$sampled_duration_of_stay <- NA
forecasted_values$discharge_date <- as.Date(character(nrow(forecasted_values)))
forecasted_values

duration_distribution <- 1:10

all_patients_df <- data.frame(admission_date = as.Date(character()),
                              duration = integer(),
                              discharge_date = as.Date(character()))

for(i in 1:nrow(forecasted_values)) {
  num_patients <- forecasted_values$number_of_patients[i]
  admission_date <- as.Date(forecasted_values$admission_date[i])

  sampled_durations <- sample(duration_distribution, num_patients, replace = TRUE)
  
  discharge_dates <- admission_date + sampled_durations
  
  daily_df <- data.frame(admission_date = rep(admission_date, num_patients),
                         duration = sampled_durations,
                         discharge_date = discharge_dates)
  
  all_patients_df <- rbind(all_patients_df, daily_df)
}
all_patients_df


# -----------------------------------------------------------------------
#                     Predicting eLOS for Today's Admissions
# -----------------------------------------------------------------------


# Sample today's patients from past data
# In reality, actual data should be used
load("Past.RData")
sim <- data[sample(nrow(data), 22), ]
sim$ADMISSION_DATE <- as.Date("2019-04-01")
adm_pred <- predict(m.RF.1, adm, type = "response")
adm_pred



new_predicted_stays <- data.table(admission_date = sim$ADMISSION_DATE, duration = round(adm_pred,0))
new_predicted_stays$discharge_date <- new_predicted_stays$admission_date + new_predicted_stays$duration
new_predicted_stays

carry_over <- rbind(predicted_stays,new_predicted_stays)
carry_over
names(carry_over)
names(all_patients_df)
projecting <- rbind(carry_over,all_patients_df)
projecting

seq_dates <- seq(from = as.Date("2019-04-01"), 
                 to = as.Date("2019-04-11"), 
                 by = "day")

# Initialize a vector to hold the bed occupancy count for each day
bed_occupancy <- numeric(length(seq_dates))

# Calculate bed occupancy for each day in the sequence
for (i in 1:length(seq_dates)) {
  current_date <- seq_dates[i]
  # Count how many patients are in the hospital on 'current_date'
  bed_occupancy[i] <- sum(projecting$admission_date <= current_date & projecting$discharge_date >= current_date)
}

# Create a data.frame for plotting
occupancy_df <- data.frame(date = seq_dates, occupancy = bed_occupancy)
occupancy_df

occupancy_diff <- c(NA, diff(occupancy_df$occupancy))

# Shift the occupancy values to align with the day for which the difference is calculated
previous_day_occupancy <- c(NA, head(occupancy_df$occupancy, -1))

# Calculate the percentage change correctly
occupancy_df$percentage_change <- (occupancy_diff / previous_day_occupancy) * 100
ggplot(occupancy_df, aes(x = date, y = occupancy)) +
  geom_line() + 
  geom_text(aes(label = sprintf("%.1f%%", percentage_change)), vjust = -0.5, nudge_y = 1, check_overlap = TRUE) +
  labs(title = "Projection of Daily Heart Disease Patient Count",
       x = "Date", y = "No. of Patients") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, NA))




