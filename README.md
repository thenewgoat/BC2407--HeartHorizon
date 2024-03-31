# Project HeartHorizon

## Executive Summary


With the ever-increasing demand for the healthcare sector in Singapore, hospitals face the challenge of providing sufficient beds, staff and resources to patients, especially for those suffering from critical, time-sensitive diseases such as heart disease. Better demand prediction models are needed for a more optimised allocation of resources, thereby streamlining hospitals’ operations and increasing the likelihood of meeting their demand.

ProjectHeartHorizon aims to enhance the ability of hospitals in forecasting demand through 2 main stages: estimating patients’ length of hospital stay, and predicting future admissions, combining which would give us what we call “Predictive Analytics for Resource & Inpatient Care Optimization” (PARICO). To achieve this, sample data from India was used. Thereafter, data cleaning and exploration were performed to prepare the dataset for modelling.

To predict a patient’s estimated length of stay, three machine learning models – Quantile Regression, MARS, and Random Forest – were used to build a prediction model which takes into account various health-related factors of the patient. The best-performing model, Random Forest, was chosen as our final prediction model for the 1st stage of PARICO.

Time-series forecasting through Prophet was used to forecast the number of patients admitted in the future, which constitutes the 2nd stage of PARICO. This forecasting model takes into account any patterns and trends contained in the admission and discharge dates of the dataset.

Integrating the 2 stages, PARICO offers a comprehensive solution that accounts for both individual patient conditions and the broader inpatient admission demand, thereby enhancing efficiency in bed, staffing, and other critical resource allocation.

Limitations of this project were also elaborated in this report, including the limited, non-current data used, as well as potential differences arising from the non-Singapore-based data. Lastly, this report covers suggested improvements that could be done to improve this project in the future, such as the use of more recent data and regular re-training of models to ensure they are updated with the latest public health trends.

