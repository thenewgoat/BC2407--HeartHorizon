Hi, this is BC2407 Seminar Group S01 Group 4's scripts for the group project.

In this folder you will find a few things.

First, HDHI Admission data.csv is the raw data file which we used for our analysis.

Next, BC2407_S01_Grp4_Rscript Final.R contains all of our data exploration, analysis and modelling done for our 'Prediction' segment, where we estimate Length of Stay of current patients. Note that this script will take a while to run, especially for the Random Forest section.

From this script, HDHI.csv and HDHI_adm_disc_analysis.csv are written and exported. These are cleaned versions of our data which are used for modelling. HDHI.csv is used for 'Prediction', while HDHI_adm_disc_analysis.csv is used for time series forecasting under 'Forecast' segment.

Separate from the Main R script is TSF.R. As it uses a slightly different dataset, the time series analysis and forecasting was peerformed in this separate file. This script will not take as long as the main R script, but the cross validation can be expected to take up to 10 minutes to run.

Finally, we have Business_Implementation.R. This script combines TSF.R and BC2407_S01_Grp4_Rscript Final.R to generate the final projections for 10 days. Although not a complete implementation with recurring, daily projections, it nonetheless demonstrates how a singular day's projection will be performed, with the relevant inputs and outputs.

For this script, it will take in 1-Apr-2019.csv, Past.RData and DoS_Model.RData. 1-Apr-2019.csv is a dataset containing simulated patient data for patients admitted on 1st April 2019. Past.RData contains the environment and all past data used for model training. DoS_Model.RData contains solely the trained Random Forest model.

Finally, a summary can be found in HeartHorizon.png, which is a flowchart of our proposed solution will function. For more detailed explanations, please do view the R Scripts which contains comments on why certain decisions were made.

An additional folder titled Zigong records our analysis and attempted modelling for a separate dataset as well as the dataset itself. The contents of this folder are no longer used in our project and thus it is not necessary to view it.