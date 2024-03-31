setwd("C:/Users/zhang/Desktop/NTU/Y2S2/BC2407/Project Files/Final R Script")
# HDHI data
setwd("C:/Users/thadd/Downloads")
# Change the directory accordingly

library(data.table)
library(lubridate)
library(dplyr)
library(caret)
library(moments)
library(corrplot)
library(randomForest)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)

# ++==========================++
# ||   Phase 1: Data Cleaning ||
# ++==========================++

# ================================================================
# Data Extraction and Summary
#=================================================================

data <- fread("HDHI Admission Data.csv")
View(data)

nrow(data) #15757 rows
ncol(data) #56 columns

View(data$`MRD No.`)

length(unique(data$`MRD No.`)) #12244 patients, based on unique MRD No.

# ================================================================
# Data Cleaning: Checking for Duplicate Rows
#=================================================================

# Checking if there are any duplicate rows
duplicate_rows <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]
nrow(duplicate_rows) # No duplicate rows


# ================================================================
# Data Cleaning: Remove Identifiers
#=================================================================

identifiers_to_remove <- c("SNO","MRD No.","")
data <- data[, !(names(data) %in% identifiers_to_remove), with = FALSE]
View(data)

# ================================================================
# Data Cleaning: Fixing Dates
#=================================================================

# Both Admission and Discharge Dates largely follow the MMDDYYYY format with exceptions

# Fortunately, Admission dates are in chronological order. We use this trait to identify dates 
# that are using the wrong format. 

# A third column duration of stay is used to calculate Discharge Dates when admission dates fall
# on day where day is the same as month (causing the condition to fail)

parse_dates_admission_discharge <- function(admission_dates, discharge_dates, duration) {
  # Initialize vectors to store parsed dates
  # parsed_admission_dates and parsed_discharge_dates are all set to NA, final values
  parsed_admission_dates <- rep(NA, length(admission_dates))
  parsed_discharge_dates <- rep(NA, length(discharge_dates))
  # Make them into Date class
  class(parsed_admission_dates) <- "Date"
  class(parsed_discharge_dates) <- "Date"
  
  # Iterating through the 15757 rows,
  for (i in 1:length(admission_dates)) {
    current_admission_str <- admission_dates[i]
    current_discharge_str <- discharge_dates[i]
    duration_days <- duration[i]
    
    # Parse admission date with default format
    current_admission_parsed <- as.Date(current_admission_str, "%m/%d/%Y")
    current_discharge_parsed <- as.Date(current_discharge_str, "%m/%d/%Y")
    
    # if current NA OR (i>1 AND prev not NA AND (jump in days OR chronological order error))
    if (is.na(current_admission_parsed) | 
        (i > 1 && !is.na(parsed_admission_dates[i-1]) & 
         (abs(difftime(current_admission_parsed, parsed_admission_dates[i-1], units = "days")) > 1 || 
          difftime(current_admission_parsed, parsed_admission_dates[i-1], units = "days") < 0))) {
      current_admission_parsed <- as.Date(current_admission_str, "%d/%m/%Y")
      current_discharge_parsed <- as.Date(current_discharge_str, "%d/%m/%Y")
    }
    
    # If day and month is same number
    if (substr(current_admission_str, 1, 2) == substr(current_admission_str, 4, 5)){
      current_discharge_parsed <- current_admission_parsed + duration_days
    }
    
    # Put the corrected date into the final date variable
    parsed_admission_dates[i] <- current_admission_parsed
    parsed_discharge_dates[i] <- current_discharge_parsed
  }
  
  return(list(admission = parsed_admission_dates, discharge = parsed_discharge_dates))
}



parsed_dates <- parse_dates_admission_discharge(data$`D.O.A`, data$`D.O.D`, data$`DURATION OF STAY`)

# Assign parsed dates back to the dataframe
data$`admission_date` <- parsed_dates$admission
data$`discharge_date` <- parsed_dates$discharge

# Print the selected columns to view their contents
View(data[, .(`D.O.A`, `D.O.D`, `admission_date`, `discharge_date`)])

sum(is.na(data$`admission_date`)) # No error in date conversion
sum(is.na(data$`discharge_date`))

rows_with_na_dod <- data[is.na(data$`discharge_date`), ]

# View the rows with NA in discharge_date
print(rows_with_na_dod)

# One row with NA --> Manually clean
data <- data[which(data$`D.O.D` == "2-1217"), `:=`(`discharge_date`= as.Date("2017-12-02"))]

# Check if changed
rows_with_na_dod <- data[is.na(data$`discharge_date`), ]
print(rows_with_na_dod)

data[, `D.O.A` := NULL]
data[, `D.O.D` := NULL]
View(data)

# Some issues with the DURATION OF STAY and the difference between 
# discharge_date and admission_date not tallying. Here we give a leeway of 
# +- 1 day due to rounding error of admission recording.

# +1 day to get the accurate number of days (because date - date)
data$DateDifference <- as.numeric(data$discharge_date - data$admission_date) +1

matching_rows <- data[abs(data$'DURATION OF STAY' - data$'DateDifference') > 1, c('admission_date', 'discharge_date', 'DURATION OF STAY', 'DateDifference')]

nrow(matching_rows) # Here we see that there is 14 rows where the disparity is very great
View(matching_rows)

# We thus decided to drop these 14 rows.
data <- subset(data, abs(data$'DURATION OF STAY' - data$'DateDifference') <= 1)

# -------- Duration of stay in ICU ------------------------------------
# Here we can see 15007/15757 rows where difference between discharge date and 
# admission date is the same as duration of stay, thus it is safe to assume that
# DURATION OF STAY includes stay time in ICU
nrow(data[data$`DURATION OF STAY` == data$`DateDifference`, ])

# Filter rows where 'DURATION OF STAY' is smaller than 'duration of intensive unit stay'
# We can see that there are 73 erroneous rows where duration of ICU stay is
# longer than duration of stay which is not possible
nrow(data[data$`DURATION OF STAY` < data$`duration of intensive unit stay`, ])

# Thus we decided to drop these 73 rows
data <- data[data$`DURATION OF STAY` >= data$`duration of intensive unit stay`, ]

data[DateDifference < 0,]

# Drop the DateDifference column
data[, DateDifference := NULL]

View(data)

write.csv(data,"HDHI_adm_disc_analysis.csv")



# ================================================================
# Data Cleaning: Handling Null Values
#=================================================================

data[data == ''] <- NA

for (col_name in names(data)) {
  # Skip the replacement for date columns
  if (!inherits(data[[col_name]], "Date")) {
    # Replace 'EMPTY' with NA for non-date columns
    data[[col_name]][data[[col_name]] == 'EMPTY'] <- NA
  }
}

colSums(is.na(data))

dim(data)
nrow(data)

View(data)

# Drop Columns with excessive missing values (9081 missing values)
data[, BNP := NULL]

# ================================================================
# Data Cleaning: Finding Categorical Variables
#=================================================================

threshold <- 20

# Calculate the number of unique values for each column
num_unique_values_per_column <- sapply(data, function(x) length(unique(x)))
num_unique_values_per_column

# Find columns with unique values below the threshold
columns_to_convert <- names(num_unique_values_per_column[num_unique_values_per_column < threshold])
columns_to_convert
# Convert these columns to categorical variables (factors)
data[, (columns_to_convert) := lapply(.SD, factor), .SDcols = columns_to_convert]


column_classes <- sapply(data, class)

# Print the data types of all columns
print(column_classes)

# ================================================================
# Data Cleaning: Converting Continuous Variables to numeric
#=================================================================

data <- data[, lapply(.SD, function(x) {
  if (!(class(x) %in% c("factor", "Date"))) {
    return(as.numeric(x))
  } else {
    return(x)
  }
})]


column_classes <- sapply(data, class)

# Print the data types of all columns
print(column_classes)

colSums(is.na(data))

# Drop month year as it is redundant information
data[, `month year`:= NULL]


# ================================================================
# Data Cleaning: Missing Value imputation
#=================================================================

# Assuming 'data' is your dataframe
# Step 1: Identify columns where number of NA values > 1
columns_with_na <- sapply(data, function(x) sum(is.na(x)) > 1)

# Step 2 and 3: Check skewness and impute
threshold <- 1  # Define your skewness threshold here

for (col_name in names(data)[columns_with_na]) {
  column_data <- data[[col_name]]
  
  # Exclude NA values for skewness calculation
  non_na_data <- column_data[!is.na(column_data)]
  
  # Skip if all values are NA or if the column is non-numeric
  if (length(non_na_data) == 0 || !is.numeric(non_na_data)) next
  
  skewness_value <- skewness(non_na_data)
  
  # Decide on median or mean based on skewness
  if (abs(skewness_value) > threshold) {
    # Use median for imputation
    imputed_value <- median(non_na_data, na.rm = TRUE)
  } else {
    # Use mean for imputation
    imputed_value <- mean(non_na_data, na.rm = TRUE)
  }
  
  # Impute missing values in the column
  data[[col_name]][is.na(data[[col_name]])] <- imputed_value
}

colSums(is.na(data))

# ================================================================
# Data Cleaning: Handling Erroneous entries
#=================================================================

# Using eyepower, we can see that there is a wrong entry under chest infection

nrow(data[data$`CHEST INFECTION` == '1', ])
nrow(data[data$`CHEST INFECTION` == '0', ])
15330 + 339 # 15669
nrow(data) #15670

# Finding the problematic row
subset_rows <- subset(data, !(`CHEST INFECTION` %in% c('0', '1')))
print(subset_rows)

# Drop the row where 'CHEST INFECTION' is '\\'
data <- data %>%
  filter(`CHEST INFECTION` != "\\")

nrow(data) #15669, successfully dropped

# ========================================
# Cleaning: Standardising column names
# ========================================
View(data)

setnames(data, old = "TYPE OF ADMISSION-EMERGENCY/OPD", new = "ADMISSION_WAY")
setnames(data, old = "DURATION OF STAY", new = "DURATION_OF_STAY")
setnames(data, old = "duration of intensive unit stay", new = "DURATION_OF_ICU_STAY")
setnames(data, old = "PRIOR CMP", new = "PRIOR_CMP")
setnames(data, old = "RAISED CARDIAC ENZYMES", new = "RAISED_CARDIAC_ENZYMES")
setnames(data, old = "SEVERE ANAEMIA", new = "SEVERE_ANAEMIA")
setnames(data, old = "STABLE ANGINA", new = "STABLE_ANGINA")
setnames(data, old = "ATYPICAL CHEST PAIN", new = "ATYPICAL_CHEST_PAIN")
setnames(data, old = "HEART FAILURE", new = "HEART_FAILURE")
setnames(data, old = "CVA INFRACT", new = "CVA_INFRACT")
setnames(data, old = "CVA BLEED", new = "CVA_BLEED")
setnames(data, old = "NEURO CARDIOGENIC SYNCOPE", new = "NEURO_CARDIOGENIC_SYNCOPE")
setnames(data, old = "INFECTIVE ENDOCARDITIS", new = "INFECTIVE_ENDOCARDITIS")
setnames(data, old = "CARDIOGENIC SHOCK", new = "CARDIOGENIC_SHOCK")
setnames(data, old = "PULMONARY EMBOLISM", new = "PULMONARY_EMBOLISM")
setnames(data, old = "CHEST INFECTION", new = "CHEST_INFECTION")
setnames(data, old = "admission_date", new = "ADMISSION_DATE")
setnames(data, old = "discharge_date", new = "DISCHARGE_DATE")


View(data)


#---------------------------------------End of Phase 1-------------------------------------------


# ++=================++
# ||   Phase 2: EDA  ||
# ++=================++

nrow(data) #15669 rows
ncol(data) # 52 columns left from 56 columns initially
View(data)


# ================================================================
# Uni-variate EDA 
# Understanding the dataset
#=================================================================

# Boxplot on patient duration of stay
ggplot(data, aes(x = `DURATION_OF_STAY`)) + geom_boxplot() + 
  labs(x = "Duration of Stay", title = "How Long Patients Stay in Hospital (in days)") +
  theme(plot.title = element_text(hjust = 0.5))

#Histogram on patient duration of stay
ggplot(data, aes(x = `DURATION_OF_STAY`)) + geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + 
  labs(x = "Duration of Stay", y = "Frequency", title = "Distribution of Patient Duration of Stay") +
  theme_minimal()


# Boxplot on patient duration of intensive unit stay
ggplot(data, aes(x = `DURATION_OF_ICU_STAY`)) + geom_boxplot() + 
  labs(x = "Duration of Intensive Unit Stay", title = "How Long Patients Stay in ICU (in days)") +
  theme(plot.title = element_text(hjust = 0.5))


# Boxplot on patient age
# We can see that most of the patients are around 50-70 years old
ggplot(data, aes(x = `AGE`)) + geom_boxplot() + 
  labs(x = "Age", title = "Patient Age Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Pie Chart on the distribution of genders
# We can see that about 2/3 of this dataset comprises of male data
ggplot(data, aes(x = "", fill = GENDER)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(fill = "Gender", x = NULL, y = NULL, title = "Distribution of Gender") +
  scale_fill_manual(values = c("F" = "skyblue", "M" = "salmon"), 
                    labels = c("F" = "Female", "M" = "Male"))

# Pie Chart on the distribution of patients' area of stay (Rural or Urban)
# More than 3/4 of patients are from Urban areas
ggplot(data, aes(x = "", fill = RURAL)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(fill = "", x = NULL, y = NULL, title = "Distribution of Patient's Area of Stay (Rural or Urban)") +
  scale_fill_manual(values = c("R" = "skyblue", "U" = "salmon"), 
                    labels = c("R" = "Rural", "U" = "Urban"))

# Pie Chart on the distribution of Patient Type of Admission (Emergency or Outpatient)
ggplot(data, aes(x = "", fill = `ADMISSION_WAY`)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(fill = "", x = NULL, y = NULL, title = "Distribution of Patient Type of Admission") +
  scale_fill_manual(values = c("E" = "skyblue", "O" = "salmon"), 
                    labels = c("E" = "Emergency", "O" = "Outpatient"))

View(data)

# ================================================================
# Bi-variate EDA 
#
# =================================================================

# Scatter plot of duration of stay vs age
ggplot(data, aes(x = AGE, y = DURATION_OF_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of Stay vs. Age", x = "Age", y = "Duration of Stay") +
  theme_minimal()

# Scatter plot of duration of ICU stay vs age
ggplot(data, aes(x = AGE, y = DURATION_OF_ICU_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of ICU Stay vs. Age", x = "Age", y = "Duration of Stay") +
  theme_minimal()

# Boxplot of duration of stay by gender
ggplot(data, aes(x = GENDER, y = DURATION_OF_STAY, fill = GENDER)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Duration of Stay", title = "Boxplot of Duration of Stay by Gender") +
  scale_fill_manual(values = c("#6D9EC1", "#E46726"), labels = c("Female", "Male")) +  # Custom fill colors for the boxplot
  theme_minimal()

# Bar Plot of duration of ICU stay by gender
ggplot(data, aes(x = DURATION_OF_ICU_STAY, fill = GENDER)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot of Duration of ICU Stay by Gender", x = "Duration of ICU Stay", y = "Count") +
  theme_minimal()

# Boxplot of duration of stay by admission way
ggplot(data, aes(x = ADMISSION_WAY, y = DURATION_OF_STAY, fill = ADMISSION_WAY)) +
  geom_boxplot() +
  labs(x = "Admission Way", y = "Duration of Stay", title = "Boxplot of Duration of Stay by Admission Way") +
  scale_fill_manual(values = c("#6D9EC1", "#E46726"), labels = c("Emergency", "Outpatient")) +  # Custom fill colors for the boxplot
  theme_minimal()

# Bar Plot of duration of ICU stay by admission way
ggplot(data, aes(x = DURATION_OF_ICU_STAY, fill = ADMISSION_WAY)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot of Duration of ICU Stay by Admission Way", x = "Duration of ICU Stay", y = "Count") +
  theme_minimal()

# Boxplot of duration of stay by place of residence
ggplot(data, aes(x = RURAL, y = DURATION_OF_STAY, fill = RURAL)) +
  geom_boxplot() +
  labs(x = "Rural", y = "Duration of Stay", title = "Boxplot of Duration of Stay by Place of Residence") +
  scale_fill_manual(values = c("#6D9EC1", "#E46726"), labels = c("Rural", "Urban")) +  # Custom fill colors for the boxplot
  theme_minimal()

# Bar Plot of duration of ICU stay by place of residence
ggplot(data, aes(x = DURATION_OF_ICU_STAY, fill = RURAL)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot of Duration of ICU Stay by Rural", x = "Duration of ICU Stay", y = "Count") +
  theme_minimal()


View(data)

require(gridExtra)
# Scatter plot of duration of stay vs HB
plot1 <- ggplot(data, aes(x = HB, y = DURATION_OF_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of Stay vs. HB", x = "Haemoglobin (HB)", y = "Duration of Stay") +
  theme_minimal()


# Scatter plot of duration of stay vs TLC
plot2 <- ggplot(data, aes(x = TLC, y = DURATION_OF_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of Stay vs. TLC", x = "Total Leukocytes Count (TLC)", y = "Duration of Stay") +
  theme_minimal()


# Scatter plot of duration of stay vs PLATELETS
plot3 <- ggplot(data, aes(x = PLATELETS, y = DURATION_OF_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of Stay vs. PLATELETS", x = "Platelets", y = "Duration of Stay") +
  theme_minimal()


# Scatter plot of duration of stay vs GLUCOSE
plot4 <- ggplot(data, aes(x = GLUCOSE, y = DURATION_OF_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of Stay vs. Glucose", x = "Glucose", y = "Duration of Stay") +
  theme_minimal()


# Scatter plot of duration of stay vs UREA
plot5 <- ggplot(data, aes(x = UREA, y = DURATION_OF_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of Stay vs. UREA", x = "Urea", y = "Duration of Stay") +
  theme_minimal()


# Scatter plot of duration of stay vs CREATININE
plot6 <- ggplot(data, aes(x = CREATININE, y = DURATION_OF_STAY)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration of Stay vs. CREATININE", x = "Creatinine", y = "Duration of Stay") +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3)
# -----------------------------------------------------------------------

# ================================================================
# EDA: Check for Outliers
#=================================================================

# The key variable we want to predict is DURATION_OF_STAY
summary(data$`DURATION_OF_STAY`)
boxplot(data$`DURATION_OF_STAY`)

summary(data$`DURATION_OF_ICU_STAY`)
boxplot(data$`DURATION_OF_ICU_STAY`)

# Function to count outliers
count_outliers <- function(data, column_name) {
  quartiles <- quantile(data[[column_name]], na.rm = TRUE, probs = c(0.25, 0.75))
  IQR_val <- IQR(data[[column_name]],na.rm=TRUE)
  
  lower_bound <- quartiles[1] - 1.5 * IQR_val #Lower whisker
  upper_bound <- quartiles[2] + 1.5 * IQR_val #Upper whisker
  
  outliers_count <- sum(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, na.rm = TRUE) #IQR Method to identify outliers
  
  return(outliers_count)
}

# Function to drop outliers
drop_outliers <- function(data, column_name) {
  quartiles <- quantile(data[[column_name]], na.rm = TRUE, probs = c(0.25, 0.75))
  IQR_val <- IQR(data[[column_name]],na.rm=TRUE)
  
  lower_bound <- quartiles[1] - 1.5 * IQR_val #Lower whisker
  upper_bound <- quartiles[2] + 1.5 * IQR_val #Upper whisker
  
  outliers <- data[[column_name]] < lower_bound | data[[column_name]] > upper_bound #IQR Method to identify outliers
  data <- data[!outliers, ]
  
  return(data)
}


# https://stats.stackexchange.com/questions/187200/how-are-random-forests-not-sensitive-to-outliers
# While some machine learning algorithms are more robust to outliers, we should
# still remove them where we can

count_outliers(data,'DURATION_OF_STAY') #727 outliers

data <- drop_outliers(data, 'DURATION_OF_STAY')
nrow(data)  # Check the number of rows after dropping outliers

count_outliers(data,'DURATION_OF_ICU_STAY') #224 outliers

data <- drop_outliers(data, 'DURATION_OF_ICU_STAY')
nrow(data)  # Check the number of rows after dropping outliers


# ================================================================
# EDA: Removal of Irrelevant Values
# For variable OUTCOME, at the point of entry of patients, we will not know
# the OUTCOME (whether they live, die or Discharged Against Medical Advice),
# thus it does not make sense to use it as a predictor for how long a patient
# stays in the hospital at the time of admission. 
#=================================================================


# Drop OUTCOME column
data[, OUTCOME := NULL]



#---------------------------------------------------------------------------------------------------------------------------------
# ================================================================
# Data Cleaning For Regressions: Multicollinearity


# Data Cleaning: Find Redundant Columns through Domain knowledge
#=================================================================

# 1. Anaemia
# -------------------------------------------------------------
sum(data$`SEVERE_ANAEMIA` == 1) # 275 rows
sum(data$`ANAEMIA` == 1) # 2763 rows
sum(data$`SEVERE_ANAEMIA` == 1 & data$`ANAEMIA` == 1) #275 rows

# Might have issue of multicollinearity with 2 columns on anaemia, so
# combine them into 1 column and factor it where combined_anaemia = 1 when
# patient only has anaemia, and 2 if patient has severe anaemia. Else 0.
data$COMBINED_ANAEMIA <- ifelse(data$ANAEMIA == 1 & data$`SEVERE_ANAEMIA` == 0, 1,
                                ifelse(data$ANAEMIA == 1 & data$`SEVERE_ANAEMIA` == 1, 2, 0))
str(data)
data$COMBINED_ANAEMIA <- as.factor(data$COMBINED_ANAEMIA)
str(data)

# Drop the redundant columns
data <- subset(data, select = -c(ANAEMIA, `SEVERE_ANAEMIA`))
View(data)


# 2. Heart failure
# ------------------------------------------------------------------
# HFREF refers to Heart Failure with Reduced Ejection Fraction (EF)
# HRNEF refers to Heart Failure with Normal Ejection Fraction (EF)

sum(data$`HEART_FAILURE` == 1) # 4059 rows
sum(data$`HFREF` == 1) # 2136 rows
sum(data$`HFNEF` == 1) # 1933 rows
2136 + 1933 # 4069, issue because there are more rows than HEART FAILURE == 1


sum(data$`HFNEF` == 1 & data$`HFREF` == 1) # 6 rows
# Issue, logic error, cant have normal and reduced EF at the same time
# drop these 6 rows first
rows_to_drop <- which(data$HFNEF == 1 & data$HFREF == 1)
data <- data[-rows_to_drop, ]


# The dataset only considers HFREF and HFNEF, so we remove these 4 rows with neither
sum(data$`HEART_FAILURE` == 1 & data$`HFNEF` == 0 & data$`HFREF` == 0) # 4 rows

rows_to_drop_2 <- which(data$`HEART_FAILURE` == 1 & data$HFREF == 0 & data$HFNEF == 0)
data <- data[-rows_to_drop_2, ]


# For these rows, make HEART FAILURE == 1
sum(data$`HEART_FAILURE` == 0 & data$`HFREF` == 1) # 6 rows
sum(data$`HEART_FAILURE` == 0 & data$`HFNEF` == 1) # 2 rows

data$`HEART_FAILURE`[data$`HEART_FAILURE` == 0 & data$HFREF == 1] <- 1
data$`HEART_FAILURE`[data$`HEART_FAILURE` == 0 & data$HFNEF == 1] <- 1

sum(data$`HEART_FAILURE` == 1) # 4057 rows
sum(data$`HFREF` == 1) # 2130 rows
sum(data$`HFNEF` == 1) # 1927 rows
2130 + 1927 # 4057 rows, the column results tally

# Combine them into 1 categorical variable where 1 indicates HFREF and 
# 2 indicates HFNEF and 0 for no heart failure
data$HEART_FAILURE_TYPE <- ifelse(data$HFREF == 1, 1, ifelse(data$HFNEF == 1, 2, 0))

str(data)
data$HEART_FAILURE_TYPE <- as.factor(data$HEART_FAILURE_TYPE)
str(data)

# Drop the redundant columns
data <- subset(data, select = -c(`HEART_FAILURE`, `HFREF`, `HFNEF`))
View(data)


# ================================================================
# Data Cleaning: Find Redundant Columns through Correlation
#=================================================================

# Select only numeric columns
numeric_data <- data[, sapply(data, is.numeric), with = FALSE]

# Create correlation matrix for numeric data
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print(correlation_matrix)

ggcorrplot(correlation_matrix, lab = TRUE)

# We arbitrarily set the correlation threshold to be 0.80.
# From the plot we can see that none of the variables have correlations with
# each other above 0.80, thus we do not remove any columns in this section.

# Export the final version of cleaned data as a csv file:
write.csv(data, "HDHI.csv")


#---------------------------------------End of Phase 2-------------------------------------------

# ++=================================================================++
# ||   Phase 3: Machine Learning Models to Predict DURATION_OF_STAY  ||
# ++=================================================================++

# ================================================================
# Linear Regression is inadequate
#=================================================================
set.seed(1)
train_lm <- sample(nrow(data), 0.7 * nrow(data))  # 70% for training
trainset_lm <- data[train_lm, ]
testset_lm <- data[-train_lm, ]

summary(data$DURATION_OF_STAY)
fit.lm <- lm(data$DURATION_OF_STAY ~ . -DURATION_OF_ICU_STAY -ADMISSION_DATE -DISCHARGE_DATE, data = data)
summary(fit.lm)
# Adjusted R-squared is 0.1612

plot(data$DURATION_OF_STAY, main = 'Regressions HDHI Data', xlab = 'X-VARIABLES', ylab = 'DURATION OF STAY')
abline(fit.lm,lty=2,col="red")

par(mfrow=c(2,2))
plot(fit.lm)
par(mfrow=c(1,1))

lm_predict <- predict(fit.lm, newdata=testset_lm)
lm_error <- testset_lm$DURATION_OF_STAY - lm_predict
lm_RMSE <- sqrt(mean(lm_error^2))
lm_RMSE
#2.840641


# ================================================================
# Quantile Regression
#=================================================================

# install.packages("quantreg")
library(quantreg)
library(data.table)
str(data)

# Split data into training and testing sets
set.seed(1)  # for reproducibility
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

# Train quantile regression models
taus <- c(0.1, 0.5, 0.9)  # Quantiles of interest
quantile_models <- lapply(taus, function(tau) rq(DURATION_OF_STAY ~ .-DURATION_OF_ICU_STAY -ADMISSION_DATE -DISCHARGE_DATE, data = data[train_index, ], tau = tau))

# Forecast using quantile regression models
forecasts <- matrix(NA, nrow = length(taus), ncol = length(test_index))  # Initialize matrix to store forecasts
for (i in 1:length(taus)) {
  forecasts[i, ] <- predict(quantile_models[[i]], newdata = data[test_index, ])
}

# Plot actual vs. forecasted values
par(mfrow = c(1, length(taus)))  # Set up the layout for plots
actual_test <- data$DURATION_OF_STAY[test_index]
for (i in 1:length(taus)) {
  plot(actual_test, forecasts[i, ], main = paste("Quantile", taus[i]), xlab = "Actual Duration of Stay", ylab = "Forecasted Duration of Stay", col = "blue", pch = 16)
  abline(0, 1, col = "red")  # Add a diagonal reference line
}
par(mfrow = c(1, 1))  # Reset layout to default

# We will use Q=0.5 (median) for our forecast
quantile_model_median <- rq(DURATION_OF_STAY ~ .-DURATION_OF_ICU_STAY -ADMISSION_DATE -DISCHARGE_DATE, data = data[train_index, ], tau = 0.5)
qr_median = predict(quantile_model_median, newdata = data[test_index, ])

change_negatives_to_zero <- function(data_vector) {
  # Replace negative values with 0
  data_vector[data_vector < 0] <- 0
  return(data_vector)
}

qr_median <- change_negatives_to_zero(qr_median)

plot(actual_test, qr_median, main = paste("Quantile = 0.5"), xlab = "Actual Duration of Stay", ylab = "Forecasted Duration of Stay", col = "blue", pch = 16)
abline(0, 1, col = "red")

# Calculate forecast performance metrics (RMSE)
RMSE_duration_median = sqrt(mean((abs(actual_test - qr_median))^2))
RMSE_duration_median
# 2.885284


# ================================================================
# MARS
#=================================================================
set.seed(1)
library(earth)
library(caret)

# Filtering the data to include only rows where DURATION_OF_ICU_STAY > 0
data_mars <- subset(data, select = -c(`ADMISSION_DATE`, `DISCHARGE_DATE`, `DURATION_OF_ICU_STAY`))

# Creating a partition to split the data into training and testing sets
index <- createDataPartition(data_mars$DURATION_OF_STAY, p=0.7, list=FALSE)

# Splitting the data based on the partition
train_data <- data_mars[index, ]
test_data <- data_mars[-index, ]

mars_model <- earth(DURATION_OF_STAY ~ ., degree=1,trace=3, data=train_data)
mars_model <- earth(DURATION_OF_STAY ~ ., degree=2,trace=3, data=train_data)

mars_model <- earth(DURATION_OF_STAY ~ ., degree=2, data=train_data)
summary(mars_model)

predictions <- predict(mars_model, newdata=test_data)

#finding rmse
actual_values <- test_data$DURATION_OF_STAY
differences <- predictions - actual_values
mean_squared_error <- mean(differences^2)
rmse_mars <- sqrt(mean_squared_error)
print(rmse_mars)
#2.828845


# ================================================================
# Random Forest
#=================================================================

set.seed(1)  # for Bootstrap sampling & RSF selection.

View(data)

save(data, file = "Past.RData")

# RF to predict DURATION_OF_STAY, excludes DURATION_OF_ICU_STAY because
# at the point of admission, the duration of ICU stay will be unknown so we
# can't use it to predict Duration of stay
m.RF.1 <- randomForest(DURATION_OF_STAY ~ . - DURATION_OF_ICU_STAY - ADMISSION_DATE - DISCHARGE_DATE, 
                       data = data, 
                       na.action = na.omit, 
                       importance = TRUE)


m.RF.1  ## shows defaults are B = 500, RSF size = floor(M/3) = 14

var.impt <- importance(m.RF.1)
var.impt
varImpPlot(m.RF.1, type = 1)
plot(m.RF.1) # error rate settled down around B=250 trees

m.RF.1$oob.times
m.RF.1$predicted


# Make predictions on the training data
predictions_rf <- predict(m.RF.1, type = "response")

# Calculate residuals
residuals <- data$DURATION_OF_STAY - predictions_rf

# Calculate mean squared residuals
rmse_rf <- sqrt(mean(residuals^2))

# Print RMSE
print(rmse_rf)
#2.541498

#---------------------------------------End of Phase 3-------------------------------------------
