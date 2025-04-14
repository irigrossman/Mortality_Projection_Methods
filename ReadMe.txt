#########################
How to Replicate the Forecasts

This document provides instructions and details to replicate the results of our demographic forecasting study. 

#########################
Getting Started

    1. Preparing Your Environment:
        File: Forecast_and_evaluate_19092024.R
        Action: Change the working directory to the folder containing the data and scripts.
        Example: setwd('C:/your/project/path')

    2. Human Mortality Database (HMD) Registration:
        Register an account at HMD to access the necessary mortality data (link: https://www.mortality.org/Account/Auth). 
        Insert your HMD username and password into the provided R script section:

        Country_Data <- hmd.mx(
          country = country_label1, 
          username = "your username", 
          password = "your password"
        )

    3. Configuration:
        Specify the jump-off year and the target year for the projection within the script.
        Note: The script may require adjustments for use with datasets from other countries or for different age ranges.

    4. Running the Script:
        Run the script to generate forecasts. Outputs will be organized in folders by model and jump-off year, and will also update several Excel workbooks with aggregate results.

#########################
Output Files and Folders

    Excel Workbooks:
    These workbooks aggregate forecast results and are updated with each script execution.
        forecasts_database.xlsx: Contains age-specific death rates by model, year, and sex.
        life_expectancies_at_age_65.xlsx: Aggregates life expectancies at age 65.
        life_expectancies_at_birth.xlsx: Aggregates life expectancies at birth.
        life_expectancies_database.xlsx: Contains life expectancies for all forecasted ages.
        Mean_Absolute_Forecast_errors.xlsx: Aggregates mean absolute forecast errors.
        Mean_Forecast_errors.xlsx: Aggregates mean forecast errors.
        Weighted_Absolute_Forecast_errors.xlsx: Aggregates weighted absolute forecast errors.
        Weighted_Forecast_errors.xlsx: Aggregates weighted forecast errors.

    Folders:
        forecasts: Contains the forecasts for each model and jump-off year.

#########################
Additional Scripts

    Extrapolative_Smoothing_model_240723.R: Script for the Extrapolative Model (EM).
    user_defined_values.xlsx: Configuration file for the extrapolative smoothing model.
#########################
Software and Package Versions

We used the following software and package versions:

    R Version: 4.3.1 (Beagle Scouts)
    RStudio Version: 2023.06.0 Build 421

Packages and Versions:

    demography: version 2.0
    StMoMo: version 0.4.1.9000
    openxlsx: version 4.2.5.2
    writexl: version 1.4.2
    purrr: version 1.0.1
    lubridate: version 1.9.2
    pracma: version 2.4.2
    ggplot2: version 3.4.2
    readxl: version 1.4.2
#########################
