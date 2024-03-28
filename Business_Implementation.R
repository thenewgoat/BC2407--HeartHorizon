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

dev.off()
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
source("C:/Users/zhang/Desktop/NTU/Y2S2/BC2407/Project Files/Final R Script/TSF.R")

# Update Admissions with new data
setDT(adm)
adm$admission_date <- as.Date(adm$ADMISSION_DATE, "%d/%m/%Y")
patient_admissions_today <- adm[, .(number_of_patients = .N),by = admission_date]
patient_admissions_today <- as.data.frame(patient_admissions_today)
patient_admissions_today$admission_date <- as.character(patient_admissions_today$admission_date)
patient_admissions_today

patient_admissions_per_day <- rbind(patient_admissions_per_day, patient_admissions_today)
patient_admissions_per_day$admission_date <- as.Date(patient_admissions_per_day$admission_date)
tail(patient_admissions_per_day)


# Update TSF model
colnames(patient_admissions_per_day) <- c('ds', 'y')

m <- prophet(patient_admissions_per_day,
             growth = "linear",
             daily.seasonality = FALSE, 
             weekly.seasonality = TRUE,
             yearly.seasonality = TRUE, 
             seasonality.mode = "multiplicative", 
             holidays = holidays)

future <- make_future_dataframe(m, periods = 10) # 2 weeks
forecast <- predict(m, future)

forecasted_values <- data.frame(admission_date = as.Date(tail(forecast$ds,10)), number_of_patients = tail(round(forecast$yhat,0), n))

forecasted_values$sampled_duration_of_stay <- NA
forecasted_values$discharge_date <- as.Date(character(nrow(forecasted_values)))


patient_details <- data.frame(admission_date = as.Date(character()), 
                              duration_of_stay = integer(), 
                              discharge_date = as.Date(character()))

# Loop through each day in forecasted_values
for (i in 1:nrow(forecasted_values)) {
  # Number of patients to sample for this day
  num_patients <- forecasted_values$number_of_patients[i]
  
  # Sample duration_of_stay for each predicted patient
  sampled_durations <- sample(data$DURATION.OF.STAY, num_patients, replace = TRUE)
  
  # Compute discharge dates based on the sampled durations
  discharge_dates <- forecasted_values$admission_date[i] + as.numeric(sampled_durations)
  
  # Create a dataframe for this batch of patients
  batch_df <- data.frame(admission_date = rep(forecasted_values$admission_date[i], num_patients),
                         duration_of_stay = sampled_durations,
                         discharge_date = discharge_dates)
  
  # Append this batch to the patient_details dataframe
  patient_details <- rbind(patient_details, batch_df)
}

# Convert admission_date and discharge_date back to Date class if necessary
patient_details$admission_date <- as.Date(patient_details$admission_date)
patient_details$discharge_date <- as.Date(patient_details$discharge_date)

patient_details$duration <- patient_details$duration_of_stay
patient_details$duration_of_stay <- NULL

patient_details


# -----------------------------------------------------------------------
#                       Predicting Today's Admissions
# -----------------------------------------------------------------------


# Sample today's patients from past data
# In reality, actual data should be used
load("Past.RData")
sim <- data[sample(nrow(data), 22), ]
sim$ADMISSION_DATE <- as.Date("2019-04-01")
adm_pred <- predict(m.RF.1, sim, type = "response")
View(adm_pred)



new_predicted_stays <- data.table(admission_date = sim$ADMISSION_DATE, duration = round(adm_pred,0))
new_predicted_stays$discharge_date <- new_predicted_stays$admission_date + new_predicted_stays$duration
new_predicted_stays

carry_over <- rbind(predicted_stays,new_predicted_stays)
carry_over

projecting <- rbind(carry_over,patient_details)
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

dev.off()
ggplot(occupancy_df, aes(x = date, y = occupancy)) +
  geom_line() + 
  labs(title = "Daily Bed Occupancy for Heart Disease Patients",
       x = "Date", y = "Bed Occupancy") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, NA))




