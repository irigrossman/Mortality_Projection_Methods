### please update to your file path
##########
setwd('C:/programming/Mortality Packages/src')
##########

# R version 4.3.1 Beagle Scouts 
# Rstudio 2023.06.0 Build 421
library(demography) #version 2.0
library(StMoMo) #version 0.4.1.9000
library(openxlsx) #version 4.2.5.2
library(writexl) #version 1.4.2
library(purrr) #version 1.0.1
library(lubridate) #version 1.9.2
source("Extrapolative_Smoothing_model_240723.R")


### data to change between forecasts
##########
datacol1 <- 4
jump_off_year <- 1950
project_to_this_year <- 1981
max_age = 105
country_label1<-"AUS"
##########

#data from HMD
#please enter your username and password in the code below

Country_Data <- hmd.mx(
  country = country_label1, 
  username = "your username", 
  password = "your password"
)

#as data is sparse we combine upper ages.
Country_Data <- extract.ages(
  Country_Data,
  0:max_age,
  combine.upper = TRUE
)
##########

#clean data

data_cols_in_rates<-1:ncol(Country_Data$rate$male)

Country_Data$rate$male<-clean_unsmoothed(Country_Data$rate$male,data_cols_in_rates)
Country_Data$rate$female<-clean_unsmoothed(Country_Data$rate$female,data_cols_in_rates)
Country_Data$rate$total<-clean_unsmoothed(Country_Data$rate$total,data_cols_in_rates)

#####
# Function to prepare data
prepare_data <- function(deaths1, populations1, sex) {
  deaths <- subset(deaths1, Sex == sex)
  pop <- subset(populations1, Sex == sex) 
  
  list(deaths = deaths, pop = pop)
}


clean_data<-function(data_to_clean){
  
  #remove spaces in Sex
  data_to_clean$Sex<-gsub("[ ]", "", data_to_clean$Sex)
  
  #remove "+" in Age column
  data_to_clean$Age <- as.numeric(gsub("[+]", "", data_to_clean$Age))
  
  #convert data to numeric
  num_columns <- ncol(data_to_clean)
  for (i in 4:num_columns) {
    data_to_clean[, i] <- as.numeric(data_to_clean[, i])
  }
  
  #change column names to years
  names(data_to_clean)[4:ncol(data_to_clean)] <- gsub("\\D", "", names(data_to_clean)[4:ncol(data_to_clean)])
  
  return(data_to_clean)
}

calc_unsmoothed <- function(populations_for_calc, deaths_for_calc, data_cols) {
  unsmoothed_ASDRs <- populations_for_calc
  
  unsmoothed_ASDRs[, data_cols] <- deaths_for_calc[, data_cols] / populations_for_calc[, data_cols]
  
  count_changes<-0
    for (check_finite_rows in 1:nrow(unsmoothed_ASDRs)) {
      for (check_finite_cols in data_cols) {
        # Identify surrounding cells
        if (
          (is.infinite(unsmoothed_ASDRs[check_finite_rows,check_finite_cols]))||
          (is.na(unsmoothed_ASDRs[check_finite_rows,check_finite_cols]))||
          ((unsmoothed_ASDRs[check_finite_rows,check_finite_cols])==0)
          ){
          count_changes<-count_changes+1
          row_range <- pmax(1, check_finite_rows - 1):pmin(nrow(unsmoothed_ASDRs), check_finite_rows + 1)
          col_range <- pmax(min(data_cols), check_finite_cols - 1):pmin(max(data_cols), check_finite_cols + 1)
          
          # Calculate average of surrounding cells (excluding NA and Inf values)
          surrounding_rates <- unsmoothed_ASDRs[row_range, col_range]
          
          surrounding_avg <- mean(surrounding_rates[!sapply(surrounding_rates, function(x) is.infinite(x) | is.na(x))], na.rm = TRUE)
          prev_value<-unsmoothed_ASDRs[check_finite_rows,check_finite_cols]
          unsmoothed_ASDRs[check_finite_rows,check_finite_cols]<-surrounding_avg

          #if there are any 0s, infs or NAs remaining. Calculate an average of the finite value surrounding cells that aren't 0, NA, or inf.
          if (surrounding_avg == 0 || is.infinite(surrounding_avg) || is.na(surrounding_avg)) {
            surrounding_avg <- mean(unsmoothed_ASDRs[surrounding_rates != 0 & !is.infinite(surrounding_rates) & !is.na(surrounding_rates)], na.rm = TRUE)
          }
        }
        
      }
    }
    
    #replace values greater than 1 with 1
    temp_array<-unsmoothed_ASDRs[,data_cols]
    temp_array[temp_array > 1] <- 1
    unsmoothed_ASDRs[,data_cols] <- temp_array
    
    
    return(unsmoothed_ASDRs)
  }


life_table_function<-function(Smoothed_ASDRs_data,the_ages,data_cols_original,write_excel_lifetable=NULL){
  Smoothed_ASDRs<-cbind(as.data.frame(the_ages),as.data.frame(the_ages),as.data.frame(the_ages),as.data.frame(Smoothed_ASDRs_data))
  colnames(Smoothed_ASDRs)[1]<-"Variable"
  colnames(Smoothed_ASDRs)[2]<-"Sex"
  colnames(Smoothed_ASDRs)[3]<-"Age"
  data_cols<-data_cols_original+3 #to account for the additional columns with variables
  start_data_col<-min(data_cols)
  nmx <- as.data.frame(Smoothed_ASDRs)
  #step 1 create lifetable using the demography package
  demogdata_lt <- demogdata(
    data = Smoothed_ASDRs[,data_cols],
    ages = Smoothed_ASDRs$Age,
    pop = Smoothed_ASDRs[,data_cols], #input not used for lifetables.
    years = 1:length(data_cols),
    type = "mortality",
    label = "label",
    name = "name"
  )
  

  
  max_age<-max(the_ages)
  
  lt1 <- lifetable(demogdata_lt,max.age=max_age)
  
  info_cols_to_add<-Smoothed_ASDRs[,1:(min(data_cols)-1)]

  mx = cbind(info_cols_to_add,lt1$mx)
  mx$Variable<-"mx"
  
  qx = cbind(info_cols_to_add,lt1$qx)
  qx$Variable<-"qx"
  
  lx = cbind(info_cols_to_add,lt1$lx)
  lx$Variable<-"lx"
  
  dx = cbind(info_cols_to_add,lt1$dx)
  dx$Variable<-"dx"
  
  Lx = cbind(info_cols_to_add,lt1$Lx)
  Lx$Variable<-"Lx"
  
  Tx = cbind(info_cols_to_add,lt1$Tx)
  Tx$Variable<-"Tx"
    
  ex = cbind(info_cols_to_add,lt1$ex)
  ex$Variable<-"ex"
  
  rx = cbind(info_cols_to_add,lt1$rx)
  rx$Variable<-"rx"
  

  #if a filename has been provided let's write to excel
  if (!is.null(write_excel_lifetable)){


    data_list1 <-list(nmx = mx, nqx = qx, ndx = dx, nlx = lx, Lx = Lx, Tx=Tx, ex = ex, rx=rx)
    combined_full_life_table <- do.call(rbind, lapply(data_list1, function(x) rbind(x, rep("", ncol(x)))))
    
    # Convert specified columns to numeric
    combined_full_life_table[, data_cols] <- sapply(combined_full_life_table[, data_cols], as.numeric)
    
    
    # Create workbook and add sheets with dataframes
    wb <- createWorkbook()
    addWorksheet(wb, "life table")
    writeData(wb, "life table", combined_full_life_table)
    
    
    saveWorkbook(wb, write_excel_lifetable, overwrite = TRUE)
    
  }
  
  
  
  return(list(nmx = mx, nqx = qx, ndx = dx, nlx = lx, Lx = Lx, Tx=Tx, ex = ex, rx=rx))
  
}

#rates have no demographic variables, only data
create_life_table_and_combined_file_for_saving<-function(rates1,ages1)
{
 
  life_table1 <- life_table_function(
    rates1,
    ages1,
    1:ncol(rates1)
  )
  column_after_AGE <- which(colnames(life_table1$ndx)=="Age")+1
  data_cols_life_tables=column_after_AGE:ncol(life_table1$ndx)

  
  data_list1 <-list(nmx = life_table1$nmx, 
                         nqx = life_table1$nqx, 
                         ndx = life_table1$ndx,
                         nlx = life_table1$nlx,
                         Lx = life_table1$Lx, 
                         Tx=life_table1$Tx, 
                         ex = life_table1$ex
                         )
  
  combined_full_life_table1 <- do.call(rbind, lapply(data_list1, function(x) rbind(x, rep("", ncol(x)))))
  
  
  # Convert specified columns to numeric
  combined_full_life_table1[, data_cols_life_tables] <- sapply(combined_full_life_table1[, data_cols_life_tables], as.numeric)
  
  return(list(ndx = life_table1$ndx, nlx = life_table1$nlx, Lx = life_table1$Lx, combined_full_life_table = combined_full_life_table1, ex = life_table1$ex))
  
}

#year 1 is the jump off year
#year 2 is the year to forecast to
#HMD_data is the data object downloaded from the HMD using the hmd.mx function
#label 1 is a required input for the methods in the demography package
forecast_mortality <- function(
    HMD_data,
    year1, year2, label1
) {
  forecast_horizon <- year2 - year1
  base_year <- min(HMD_data$year)
  end_year <- year2
  name1="projections"
  

  #we may need to create projections beyond the last year in the data. However we can only do evaluations to the last year of the data. 
  #to deal with this pad data with NAs to final year required for projections. 
  HMD_data_original<-HMD_data
  print(paste("creating forecasts with base year:", base_year, ", forecasting to: ", end_year))
  
  # Determine the maximum year in the data
  
  max_year_data <- max(HMD_data$year)
  
  # Check if year2 is greater than max_year_data. If it is, pad with NAs to allow forecasts longer than the data.
  if (year2 > max_year_data) {
    warning("Forecasts extend beyond the last available year in the data.Data padded with NA_real_s for evaluation")
    missing_years <- (max_year_data + 1):year2
    
    # Create data frames with NA values for the missing years
    missing_data_pop_female <- as.data.frame(matrix(NA_real_, nrow = nrow(HMD_data$pop$female), ncol = length(missing_years)))
    colnames(missing_data_pop_female) <- missing_years
    # For pop$male
    missing_data_pop_male <- as.data.frame(matrix(NA_real_, nrow = nrow(HMD_data$pop$male), ncol = length(missing_years)))
    colnames(missing_data_pop_male) <- missing_years
    
    # For pop$total
    missing_data_pop_total <- as.data.frame(matrix(NA_real_, nrow = nrow(HMD_data$pop$total), ncol = length(missing_years)))
    colnames(missing_data_pop_total) <- missing_years
    
    # For rate$female
    missing_data_rate_female <- as.data.frame(matrix(NA_real_, nrow = nrow(HMD_data$rate$female), ncol = length(missing_years)))
    colnames(missing_data_rate_female) <- missing_years
    
    # For rate$male
    missing_data_rate_male <- as.data.frame(matrix(NA_real_, nrow = nrow(HMD_data$rate$male), ncol = length(missing_years)))
    colnames(missing_data_rate_male) <- missing_years
    
    # For rate$total
    missing_data_rate_total <- as.data.frame(matrix(NA_real_, nrow = nrow(HMD_data$rate$total), ncol = length(missing_years)))
    colnames(missing_data_rate_total) <- missing_years
    
    HMD_data$year <- c(HMD_data$year, missing_years)
    
    # Combine the missing data with the original data
    # Update pop$female
    HMD_data$pop$female <- cbind(HMD_data$pop$female, missing_data_pop_female)
    
    # Update pop$male
    HMD_data$pop$male <- cbind(HMD_data$pop$male, missing_data_pop_male)
    
    # Update pop$total
    HMD_data$pop$total <- cbind(HMD_data$pop$total, missing_data_pop_total)
    
    # Update rate$female
    HMD_data$rate$female <- cbind(HMD_data$rate$female, missing_data_rate_female)
    
    # Update rate$male
    HMD_data$rate$male <- cbind(HMD_data$rate$male, missing_data_rate_male)
    
    # Update rate$total
    HMD_data$rate$total <- cbind(HMD_data$rate$total, missing_data_rate_total)
    
  }

  
  #male rates
  full_male_rates <- HMD_data$rate$male
  full_female_rates <- HMD_data$rate$female
  
  
  full_male_populations <- HMD_data$pop$male
  full_female_populations <- HMD_data$pop$female
  
  
  #find the column after the jump off year year1. 
  #assume it is the same for males and females
  
  which_col_first_year_projection_period<-
    which(
      as.numeric(colnames(full_male_rates))==
        (year1+1)
    )
  

  which_col_last_year_projection_period<-
    which(
      as.numeric(colnames(full_male_rates))==
        (year2))

  
  ACTUAL_rates_male <- full_male_rates[,which_col_first_year_projection_period:which_col_last_year_projection_period]
  ACTUAL_rates_female <- full_female_rates[,which_col_first_year_projection_period:which_col_last_year_projection_period]
  
  first_year<-min(HMD_data$year)
  
  #let's subset the hmd data that is in the base period
  HMD_data_BASE <- extract.years(
    HMD_data_original,
    first_year:year1
  )
  
  max_age<-length(HMD_data_BASE$age)-1
  
  
  # Create a list to store the results
  forecasts <- list()
  
  # Extrapolative Smoothing [START]
  #####

  #lets take the male and female death rates, and for each
  # 1 create an "Age" column
  # 2 create a "Sex" column
  # Creates a Variable column
  #
  #put data into the correct structure for the Extrapolative smoothing code
  death_rates_males_main<-as.data.frame(HMD_data_BASE$rate$male)
  death_rates_females_main<-as.data.frame(HMD_data_BASE$rate$female)
  
  descriptive_cols <- death_rates_males_main[,1:3]
  colnames(descriptive_cols) <- c("Variable", "Sex", "Age")
  descriptive_cols$Age <- 0:(nrow(death_rates_males_main)-1)
  descriptive_cols$Variable<-"death rates"
  
  death_rates_males_main <- cbind(descriptive_cols,death_rates_males_main)
  death_rates_males_main$Sex <- "Males"
  
  death_rates_females_main <- cbind(descriptive_cols,death_rates_females_main)
  death_rates_females_main$Sex <- "Females"
  
  death_rates_combined <- rbind(death_rates_females_main,death_rates_males_main)
  rownames(death_rates_combined) <- 1:nrow(death_rates_combined)
  
  forecast_object <-create_forecast_object(
    base_pop=NULL,
    base_deaths=NULL,
    death_rates=death_rates_combined,
    4,
    year1,
    year2)
  
  
  print("forecasting with ExS")
  HMD_projection_ExS <- create_projections(forecast_object,
                                        is_evaluation = 1)
  #ExS output contains descriptive columns. The other methods do not. Therefore we want to remove
  #these when we save them to the forecast object. 
  ExS_males_projections_without_descriptive_cols <- HMD_projection_ExS$ProjectD_Males[,4:ncol(HMD_projection_ExS$ProjectD_Males)]
  ExS_females_projections_without_descriptive_cols <- HMD_projection_ExS$ProjectD_Females[,4:ncol(HMD_projection_ExS$ProjectD_Females)]
    
  forecasts[["ExS"]] <- list(
      males_projections = as.matrix(ExS_males_projections_without_descriptive_cols),
      females_projections = as.matrix(ExS_females_projections_without_descriptive_cols)
    )
  
    
  # Extrapolative Smoothing [END]
  #####

  
  # DEMOGRAPHY forecasts [START]
  #####
  
  #Lee Carter [START]#
  print("forecasting with Lee Carter from the Demography Package")
  lc_fit_male <- lca(HMD_data_BASE, 
                     series="male", 
                     max.age = max_age) 

  lc_fit_female <- lca(HMD_data_BASE, 
                       series="female", 
                       max.age = max_age) 

  
  
  lc_forecast_males <- forecast(lc_fit_male,
                                h = forecast_horizon)
  
  lc_forecast_females <- forecast(lc_fit_female, 
                                  h = forecast_horizon)
  
  
  # Store the results
  forecasts[["lc_D"]] <- list(
    males_projections = lc_forecast_males$rate$male,
    females_projections = lc_forecast_females$rate$female
  )
  #Lee Carter [END]#
  
  
  #Lee Carter, select base period, [START]#
  print("forecasting with Lee Carter, with automated base period selection, from the Demography Package")
  lc_fit_choose_period_males <- lca(HMD_data_BASE, 
                                    series="male", 
                                    max.age = max_age, 
                                    # interpolate = TRUE, 
                                    chooseperiod = TRUE)
  lc_fit_choose_period_females <- lca(HMD_data_BASE, 
                                      series="female", 
                                      max.age = max_age, 
                                      # interpolate = TRUE, 
                                      chooseperiod = TRUE)
  
  lc_choose_period_forecast_males <- forecast(lc_fit_choose_period_males, h = forecast_horizon)
  lc_choose_period_forecast_females <- forecast(lc_fit_choose_period_females, h = forecast_horizon)
  
  
  # Store the results
  forecasts[["lc_choose_period_D"]] <- list(
    males_projections = lc_choose_period_forecast_males$rate$male,
    females_projections = lc_choose_period_forecast_females$rate$female
    
  )
  
  #Lee Carter, select base period, [END]#    
  
  
  # Coherent fdm for males and females [START] #
  
  # Fit the coherent functional data model
  coherent_fit <- coherentfdm(HMD_data_BASE)
  # coherent_fit <- coherentfdm(HMD_data_BASE, order1 = 4, order2 = 4)
  print("forecasting with coherentFDM from the Demography Package")
  coherent_fit <- coherentfdm(HMD_data_BASE)
  # summary(coherent_fit)
  
  coherent_forecast <- forecast(coherent_fit,h = forecast_horizon)
  
  
  # assuming coherent_forecast$ratio$name1_males is a data frame with 'time' and 'logged_death_rates' columns
  df_coherentfdm_males <- coherent_forecast$male$rate$male
  
  df_coherentfdm_females <- coherent_forecast$female$rate$female
  
  
  # Store the results
  forecasts[["coherentfdm_D"]] <- list(
    males_projections = df_coherentfdm_males,
    females_projections = df_coherentfdm_females
    
  )
  
  # Coherent fdm for males and females [END] #
  
  
  # Booth-Maindonald-Smith model [START] #  
  print("forecasting with bms from the Demography Package")
  bms_fit_males <- bms(HMD_data_BASE, 
                       series="male", 
                       max.age = max_age) 
                       # interpolate = TRUE)
  
  bms_fit_females <- bms(HMD_data_BASE, 
                         series="female", 
                         max.age = max_age) 
                         # interpolate = TRUE)
  
  
  
  bms_forecast_males <- forecast(bms_fit_males, h = forecast_horizon)
  bms_forecast_females <- forecast(bms_fit_females, h = forecast_horizon)
  
  
  # Store the results
  forecasts[["bms_D"]] <- list(
    males_projections = bms_forecast_males$rate$male,
    females_projections = bms_forecast_females$rate$female
  )
  # Booth-Maindonald-Smith model [END] #  
  
  forecasts[['D_combo']] <- list(
    males_projections = (forecasts$lc_D$males_projections+
                           forecasts$coherentfdm_D$males_projections+
                           forecasts$bms_D$males_projections)/3,
    females_projections = (forecasts$lc_D$females_projections+
                             forecasts$coherentfdm_D$females_projections+
                             forecasts$bms_D$females_projections)/3
  )
  
  ##### DEMOGRAPHY forecasts [END]
  #####
  
  
  ## Forecasts with STMOMO [START]
  #####
  CountryData_male <- StMoMoData(HMD_data_BASE, series = "male")
  CountryData_female <- StMoMoData(HMD_data_BASE, series = "female")
  
  ages<-CountryData_male$ages
  years<-CountryData_male$years
  
  
  #model definition using predefined functions for common models
  LC <- lc()
  APC <- apc()
  CBD <- cbd()
  PLAT <-plat()

  print("fitting LC for males, StMoMo")
  LCfit_male <- fit(LC, data=CountryData_male)

  print("fitting APC for males, StMoMo")
  APCfit_male <- fit(APC, data=CountryData_male)
  

  print("fitting CBD for males, StMoMo")
  CBDfit_male <- fit(CBD, data=central2initial(CountryData_male))
  print("fitting Plat for males, StMoMo")
  Platfit_male <- fit(PLAT,  data=(CountryData_male))
  

  #forecasting
  print("forecasting LC for males, StMoMo")
  LCfor_male <- forecast(LCfit_male, h=forecast_horizon)

  print("forecasting APC for males, StMoMo")
  APCfor_male <- forecast(APCfit_male, h=forecast_horizon)
  
  print("forecasting CBD for males, StMoMo")
  CBDfor_male <- forecast(CBDfit_male, h=forecast_horizon)

  print("forecasting Plat for males, StMoMo")
  Platfor_male <- forecast(Platfit_male, h = forecast_horizon)

  
  print("fitting LC for females, StMoMo")
  LCfit_female <- fit(LC, data=CountryData_female)
  
 
  
  print("fitting APC for females, StMoMo")
  APCfit_female <- fit(APC, data=CountryData_female)
  
  print("fitting CBD for females, StMoMo")
  CBDfit_female <- fit(CBD, data=central2initial(CountryData_female))
  

  print("fitting PLAT for females, StMoMo")
  Platfit_female <- fit(PLAT,  data=(CountryData_female))
  
 
  #forecasting
  print("forecasting LC for females, StMoMo")
  LCfor_female <- forecast(LCfit_female, h=forecast_horizon)
  
  print("forecasting APC for females, StMoMo")
  APCfor_female <- forecast(APCfit_female, h=forecast_horizon)

  
  print("forecasting CBD for females, StMoMo")
  CBDfor_female <- forecast(CBDfit_female, h=forecast_horizon)
  

  print("forecasting Plat for females, StMoMo")
  Platfor_female <- forecast(Platfit_female, h = forecast_horizon)
  #
  
  # Store the results
  forecasts[["lc_StMoMo"]] <- list(
    males_projections = LCfor_male$rates,
    females_projections = LCfor_female$rates
  )
  
  forecasts[["ACP_StMoMo"]] <- list(
    males_projections = APCfor_male$rates,
    females_projections = APCfor_female$rates
  )
  

  forecasts[["CBD_StMoMo"]] <- list(
    males_projections = -log(1-CBDfor_male$rates),
    females_projections = -log(1-CBDfor_female$rates)
  )
  
  
  forecasts[["Plat_StMoMo"]] <- list(
    males_projections = Platfor_male$rates,
    females_projections = Platfor_female$rates
  )

  
  forecasts[['StMoMo_combo']] <- list(
    males_projections = (forecasts$lc_StMoMo$males_projections+
                           forecasts$ACP_StMoMo$males_projections+
                           forecasts$CBD_StMoMo$males_projections+
                           forecasts$Plat_StMoMo$males_projections)/4,
    females_projections = (forecasts$lc_StMoMo$females_projections+
                             forecasts$ACP_StMoMo$females_projections+
                             forecasts$CBD_StMoMo$females_projections+
                             forecasts$Plat_StMoMo$females_projections)/4
  )
  
  ## Forecasts with STMOMO [END]

  forecasts[['unique_combo']] <- list(
    males_projections = (forecasts$lc_D$males_projections+
                           forecasts$coherentfdm_D$males_projections+
                           forecasts$ExS$males_projections+
                           forecasts$ACP_StMoMo$males_projections+
                           forecasts$CBD_StMoMo$males_projections+
                           forecasts$Plat_StMoMo$males_projections)/6,
    females_projections =  (forecasts$lc_D$females_projections+
                              forecasts$coherentfdm_D$females_projections+
                              forecasts$ExS$females_projections+
                              forecasts$ACP_StMoMo$females_projections+
                              forecasts$CBD_StMoMo$females_projections+
                              forecasts$Plat_StMoMo$females_projections)/6
  )
  
  
    
  #####
  #ExS [START]
  #lets take the male and female death rates, and for each
  # 1 create an "Age" column
  # 2 create a "Sex" column
  # Creates a Variable column
  #
  #put data into the correct structure for the Extrapolative smoothing code
  death_rates_males_main<-as.data.frame(HMD_data_BASE$rate$male)
  death_rates_females_main<-as.data.frame(HMD_data_BASE$rate$female)

  descriptive_cols <- death_rates_males_main[,1:3]
  colnames(descriptive_cols) <- c("Variable", "Sex", "Age")
  descriptive_cols$Age <- 0:(nrow(death_rates_males_main)-1)
  descriptive_cols$Variable<-"death rates"

  death_rates_males_main <- cbind(descriptive_cols,death_rates_males_main)
  death_rates_males_main$Sex <- "Males"

  death_rates_females_main <- cbind(descriptive_cols,death_rates_females_main)
  death_rates_females_main$Sex <- "Females"

  death_rates_combined <- rbind(death_rates_females_main,death_rates_males_main)
  rownames(death_rates_combined) <- 1:nrow(death_rates_combined)

  forecast_object <-create_forecast_object(
    base_pop=NULL,
    base_deaths=NULL,
    death_rates=death_rates_combined,
    4,
    year1,
    year2)



  HMD_projections_ExS <- create_projections(forecast_object,
                                        is_evaluation = 1)
  #EDIEV [End]
  
  #####
 
  #adding actual rates, and other details to the lists in the forecasts object
    for (add_detail_counter in 1:length(forecasts)){
      print(add_detail_counter)
      forecasts[[add_detail_counter]]$ACTUAL_rates_male <- ACTUAL_rates_male 
      forecasts[[add_detail_counter]]$ACTUAL_rates_female <- ACTUAL_rates_female
      
      
      forecasts[[add_detail_counter]]$pop_base_male <- HMD_data_BASE$pop$male
      forecasts[[add_detail_counter]]$pop_base_female <- HMD_data_BASE$pop$female
      
      forecasts[[add_detail_counter]]$death_rates_base_male <- HMD_data_BASE$rate$male
      forecasts[[add_detail_counter]]$death_rates_base_female <- HMD_data_BASE$rate$female
      
      
      forecasts[[add_detail_counter]]$age <- HMD_data_BASE$age
      
      #calc life_table_male_projections
      
      ###projected life table calc
      
      life_table_data_males <- create_life_table_and_combined_file_for_saving(
        forecasts[[add_detail_counter]]$males_projections,
        forecasts[[add_detail_counter]]$age)
      
      life_table_data_females <- create_life_table_and_combined_file_for_saving(
        forecasts[[add_detail_counter]]$females_projections,
        forecasts[[add_detail_counter]]$age)
      #####
      #calculate data for the pseudo-projections for females
      temp_data_female<-life_table_data_females$Lx
      temp_data_female$Age <-as.numeric(as.character(temp_data_female$Age))
      index_age_101<-which(temp_data_female$Age == 101)
      temp_data_female[, 4:ncol(temp_data_female)] <- temp_data_female[, 4:ncol(temp_data_female)] * 100000
      sum_age_101_to_max <- colSums(temp_data_female[index_age_101:nrow(temp_data_female), 4:ncol(temp_data_female)], na.rm = TRUE)
      temp_data_female2<-temp_data_female[1:index_age_101,]
      #last row is aggregate of values for 101 and over
      temp_data_female2[index_age_101,4:ncol(temp_data_female2)]<-sum_age_101_to_max
      #we need to create a new dataframe that has means of adjacent columns
      column_means_female <- data.frame(matrix(ncol = ncol(temp_data_female2) - 1, nrow = nrow(temp_data_female2)))
      column_means_female[,1:3]<-temp_data_female2[,1:3]
      colnames(column_means_female)<-colnames(temp_data_female2[-(length(temp_data_female2))])
      # Loop through each row
      print('calc pseudo projection things')
      for (i2 in 1:nrow(column_means_female)) {
        # Loop through each pair of adjacent columns, starting from column 4
        for (j2 in 4:(ncol(column_means_female) )) {
          # Calculate the mean of adjacent columns for each row
          column_means_female[i2, j2] <- (temp_data_female2[i2, j2] + temp_data_female2[i2, j2 + 1])/2
        }
      }
      
      #calculate data for the pseudo-projections for males
      temp_data_male<-life_table_data_males$Lx
      temp_data_male$Age <-as.numeric(as.character(temp_data_male$Age))
      index_age_101<-which(temp_data_male$Age == 101)
      temp_data_male[, 4:ncol(temp_data_male)] <- temp_data_male[, 4:ncol(temp_data_male)] * 100000
      sum_age_101_to_max <- colSums(temp_data_male[index_age_101:nrow(temp_data_male), 4:ncol(temp_data_male)], na.rm = TRUE)
      temp_data_male2<-temp_data_male[1:index_age_101,]
      #last row is aggregate of values for 101 and over
      temp_data_male2[index_age_101,4:ncol(temp_data_male2)]<-sum_age_101_to_max
      #we need to create new dataframe that has means of adjacent columns
      column_means_male <- data.frame(matrix(ncol = ncol(temp_data_male2) - 1, nrow = nrow(temp_data_male2)))
      column_means_male[,1:3]<-temp_data_male2[,1:3]
      colnames(column_means_male)<-colnames(temp_data_male2[-(length(temp_data_male2))])
      # Loop through each row
      for (i2 in 1:nrow(column_means_male)) {
        # Loop through each pair of adjacent columns, starting from column 4
        for (j2 in 4:(ncol(column_means_male) )) {
          # Calculate the mean of adjacent columns for each row
          column_means_male[i2, j2] <- (temp_data_male2[i2, j2] + temp_data_male2[i2, j2 + 1])/2
        }
      }
      
      
      
      forecasts[[add_detail_counter]]$pseudoproj_Lx_female<-column_means_female
      forecasts[[add_detail_counter]]$pseudoproj_Lx_male<-column_means_male 
      

      
      ###
      forecasts[[add_detail_counter]]$lt_male_proj <- life_table_data_males$combined_full_life_table
      forecasts[[add_detail_counter]]$lt_female_proj <- life_table_data_females$combined_full_life_table
      
      forecasts[[add_detail_counter]]$ex_male_proj <- life_table_data_males$ex
      forecasts[[add_detail_counter]]$ex_female_proj <- life_table_data_females$ex
      
      ### actual life table calc
      ACTUAL_life_table_data_males <- create_life_table_and_combined_file_for_saving(
        ACTUAL_rates_male,
        forecasts[[add_detail_counter]]$age)
      
      ACTUAL_life_table_data_females <- create_life_table_and_combined_file_for_saving(
        ACTUAL_rates_female,
        forecasts[[add_detail_counter]]$age)
      
      forecasts[[add_detail_counter]]$ACTUAL_lt_male <- ACTUAL_life_table_data_males$combined_full_life_table
      forecasts[[add_detail_counter]]$ACTUAL_lt_female <- ACTUAL_life_table_data_females$combined_full_life_table
      
      forecasts[[add_detail_counter]]$ACTUAL_ex_male <- ACTUAL_life_table_data_males$ex
      forecasts[[add_detail_counter]]$ACTUAL_ex_female <- ACTUAL_life_table_data_females$ex
      
      #residual
      forecasts[[add_detail_counter]]$ERROR_rate_male <-  
        forecasts[[add_detail_counter]]$ACTUAL_rates_male - forecasts[[add_detail_counter]]$males_projections
      
      forecasts[[add_detail_counter]]$ERROR_rate_female <- 
        forecasts[[add_detail_counter]]$ACTUAL_rates_female - forecasts[[add_detail_counter]]$females_projections
      ###
      forecasts[[add_detail_counter]]$year <- HMD_data_BASE$year
  }
  
  return(forecasts)
}

# Function to evaluate forecasts
evaluate_forecast <- function(forecast, deaths, pop, year1, year2) {
  base_year <- which(colnames(deaths) == year1)
  end_year <- which(colnames(deaths) == year2)
  
  # Calculate observed rates
  observed_rates <- deaths[, (base_year + 1):end_year] / pop[, (base_year + 1):end_year]
  
  # Calculate forecast errors
  errors <- forecast$forecast$rate - observed_rates
  
  # Calculate evaluation metrics
  jaggedness <- sqrt(sum(diff(log(errors))^2) / (length(errors) - 1))
  absolute_total_error <- sum(abs(errors))
  total_absolute_error <- sum(abs(errors / observed_rates))
  
  list(jaggedness = jaggedness,
       absolute_total_error = absolute_total_error,
       total_absolute_error = total_absolute_error)
}

# Function to write each list to Excel
write_lists_to_excel <- function(forecasts) {
  
  # Get current date and time
  datestring <- Sys.time() %>% as.character() %>% gsub(" ", "_", .) %>% gsub(":", "-", .)
  
  # Create the new directory
  newdir <- paste0(getwd(), "/forecasts/forecasts_","from_", jump_off_year, "_to_",project_to_this_year,"_created_", datestring)
  dir.create(newdir)
  
  # Write each list to a separate Excel file
  for (name in names(forecasts)) {
    # Ensure all list elements are data frames
    data_to_write <- map(forecasts[[name]], ~{
      if (is.data.frame(.)) return(.)
      else return(as.data.frame(.))
    })
    
    # Define the Excel file path
    filepath <- paste0(newdir, "/", name, ".xlsx")
    
    # Write to Excel
    write_xlsx(data_to_write, path = filepath)
  }
}

# Function to calculate weighted absolute mean error
calculate_weighted_absolute_error <- function(forecasts,data_cols_weighted_error) {
  
  # Initialize an empty list to store the results
  weighted_errors <- list()
  
  # Loop over each forecast method
  for (method in names(forecasts)) {
    print(method)
    # Apply life table function to base death rates
    life_table_male <- life_table_function(
      forecasts[[method]]$males_projections,
      forecasts[[method]]$age,
      1:ncol(forecasts[[method]]$males_projections))
    
    life_table_female <- life_table_function(
      forecasts[[method]]$females_projections,
      forecasts[[method]]$age,
      1:ncol(forecasts[[method]]$females_projections))
    

    
    # Calculate absolute error
    abs_ERROR_rate_male <- abs(forecasts[[method]]$ERROR_rate_male)
    abs_ERROR_rate_female <- abs(forecasts[[method]]$ERROR_rate_female)
    
    # Calculate weighted absolute mean error for each year
    weighted_ERROR_rate_male <- colSums(life_table_male$ndx[,data_cols_weighted_error] * abs_ERROR_rate_male) / colSums(life_table_male$ndx[,data_cols_weighted_error])
    weighted_ERROR_rate_female <- colSums(life_table_female$ndx[,data_cols_weighted_error] * abs_ERROR_rate_female) / colSums(life_table_female$ndx[,data_cols_weighted_error])
    
    # Store the results
    weighted_errors[[method]] <- list(male = weighted_ERROR_rate_male, female = weighted_ERROR_rate_female)
  }
  
  return(weighted_errors)
}

# Function to calculate weighted absolute mean error
calculate_weighted_error <- function(forecasts,data_cols_weighted_error_weighted_error) {
  
  # Initialize an empty list to store the results
  weighted_errors <- list()
  
  # Loop over each forecast method
  for (method in names(forecasts)) {
    
    # Apply life table function to base death rates
    life_table_male <- life_table_function(
      forecasts[[method]]$males,
      forecasts[[method]]$age,
      1:ncol(forecasts[[method]]$males))
    
    life_table_female <- life_table_function(
      forecasts[[method]]$females,
      forecasts[[method]]$age,
      1:ncol(forecasts[[method]]$females))
    

    
    # Calculate absolute error
    ERROR_rate_male <- (forecasts[[method]]$ERROR_rate_male)
    ERROR_rate_female <- (forecasts[[method]]$ERROR_rate_female)
    
    # Calculate weighted absolute mean error for each year
    weighted_ERROR_rate_male <- colSums(life_table_male$ndx[,data_cols_weighted_error] * ERROR_rate_male) / colSums(life_table_male$ndx[,data_cols_weighted_error])
    weighted_ERROR_rate_female <- colSums(life_table_female$ndx[,data_cols_weighted_error] * ERROR_rate_female) / colSums(life_table_female$ndx[,data_cols_weighted_error])
    
    # Store the results
    weighted_errors[[method]] <- list(male = weighted_ERROR_rate_male, female = weighted_ERROR_rate_female)
  }
  
  return(weighted_errors)
}

# Function to calculate unweighted absolute mean error
calculate_unweighted_absolute_error <- function(forecasts,data_cols_weighted_error_weighted_error) {
  
  # Initialize an empty list to store the results
  unweighted_absolute_errors <- list()
  
  # Loop over each forecast method
  for (method in names(forecasts)) {
    

    
    # Calculate absolute error
    abs_ERROR_rate_male <- abs(forecasts[[method]]$ERROR_rate_male)
    abs_ERROR_rate_female <- abs(forecasts[[method]]$ERROR_rate_female)
    
    # Calculate weighted absolute mean error for each year
    mean_absolute_ERROR_rate_male <- colMeans(abs_ERROR_rate_male)
    mean_absolute_ERROR_rate_female <- colMeans(abs_ERROR_rate_female)
    
    # Store the results
    unweighted_absolute_errors[[method]] <- list(male = mean_absolute_ERROR_rate_male, female = mean_absolute_ERROR_rate_female)
  }
  
  return(unweighted_absolute_errors)
}

# Function to calculate unweighted absolute mean error
calculate_unweighted_error <- function(forecasts,data_cols_weighted_error_weighted_error) {
  
  # Initialize an empty list to store the results
  unweighted_errors <- list()
  
  # Loop over each forecast method
  for (method in names(forecasts)) {
    
    
    
    # Calculate absolute error
    ERROR_rate_male <- abs(forecasts[[method]]$ERROR_rate_male)
    ERROR_rate_female <- abs(forecasts[[method]]$ERROR_rate_female)
    
    # Calculate weighted absolute mean error for each year
    mean_ERROR_rate_male <- colMeans(ERROR_rate_male)
    mean_ERROR_rate_female <- colMeans(ERROR_rate_female)
    
    # Store the results
    unweighted_errors[[method]] <- list(male = mean_ERROR_rate_male, female = mean_ERROR_rate_female)
  }
  
  return(unweighted_errors)
}

# calculate life expectancy at birth
calculate_life_expectancy_at_birth <- function(forecasts,data_cols_weighted_error_weighted_error) {
  
  # Initialize an empty list to store the results
  life_expectancies_at_birth_list <- list()
  
  which_col_after_age <- which(colnames(forecasts[[1]]$ex_male_proj)=='Age')+1
  # which_col_after_age<-1
  last_data_col<-ncol(forecasts[[1]]$ex_male_proj)
  
  
  actual_life_expectancy_at_birth_male<-t(forecasts[[1]]$ACTUAL_ex_male[1,which_col_after_age:last_data_col])
  actual_life_expectancy_at_birth_female<-t(forecasts[[1]]$ACTUAL_ex_female[1,which_col_after_age:last_data_col])
  
  life_expectancies_at_birth_list[['actual']]<-list(
    male = actual_life_expectancy_at_birth_male,
    female = actual_life_expectancy_at_birth_female
    )
  
  
  # Loop over each forecast method
  for (method in names(forecasts)) {
    
    print(method)
    # store life expectancy at birth
    projected_life_expectancy_at_birth_male <- t((forecasts[[method]]$ex_male_proj)[1,which_col_after_age:last_data_col])
    projected_life_expectancy_at_birth_female <- t((forecasts[[method]]$ex_female_proj)[1,which_col_after_age:last_data_col])
    

    # Store the results
    life_expectancies_at_birth_list[[method]] <- list(
      male = projected_life_expectancy_at_birth_male, 
      female = projected_life_expectancy_at_birth_female)
  }
  
  return(life_expectancies_at_birth_list)
}

# calculate life expectancy at birth
calculate_life_expectancy_at_selected_age <- function(forecasts,data_cols_weighted_error_weighted_error,selected_age) {
  
 
  # Initialize an empty list to store the results
  life_expectancies_at_selected_age_list <- list()
  
  age_col<-which(colnames(forecasts[[1]]$ex_male_proj)=='Age')
  which_col_after_age <- which(colnames(forecasts[[1]]$ex_male_proj)=='Age')+1
  
  last_data_col<-ncol(forecasts[[1]]$ex_male_proj)
  
  selected_age_row<-which(forecasts[[1]]$ACTUAL_ex_male$Age==selected_age)
  
  actual_life_expectancy_at_selected_age_male<-t(forecasts[[1]]$ACTUAL_ex_male[selected_age_row,age_col:last_data_col])  #changed ACTUAL_lt_male to actual_ex_male
  actual_life_expectancy_at_selected_age_female<-t(forecasts[[1]]$ACTUAL_ex_female[selected_age_row,age_col:last_data_col])
  
  life_expectancies_at_selected_age_list[['actual']]<-list(
    male = actual_life_expectancy_at_selected_age_male,
    female = actual_life_expectancy_at_selected_age_female
  )
  
  
  # Loop over each forecast method
  for (method in names(forecasts)) {
    
    print(method)
    # store life expectancy at birth
    projected_life_expectancy_at_selected_age_male <- t((forecasts[[method]]$ex_male_proj)[selected_age_row,age_col:last_data_col])
    projected_life_expectancy_at_selected_age_female <- t((forecasts[[method]]$ex_female_proj)[selected_age_row,age_col:last_data_col])
    
    
    # Store the results
    life_expectancies_at_selected_age_list[[method]] <- list(
      male = projected_life_expectancy_at_selected_age_male, 
      female = projected_life_expectancy_at_selected_age_female)
  }
  
  return(life_expectancies_at_selected_age_list)
}

#updates files with new forecast evaluation results
update_forecast_errors <- function(file_name, errors, jump_off_year, project_to_this_year, country_label) {
  require(openxlsx)
  

  old_data_male <- read.xlsx(file_name, sheet = "male")
  old_data_female <- read.xlsx(file_name, sheet = "female")
  
  # Transform errors into suitable format
  transformed_weighted_absolute <- transform_forecast_errors(errors, jump_off_year, project_to_this_year, country_label)
  
  # Male data
  if(is.null(old_data_male)) {
    combined_data_male <- transformed_weighted_absolute$df_males
  } else {
    rows_diff <- nrow(transformed_weighted_absolute$df_males) - nrow(old_data_male)
    if (rows_diff > 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(old_data_male), nrow = rows_diff))
      colnames(data_to_pad)<-colnames(old_data_male)      
      
      old_data_male <- rbind(old_data_male, data_to_pad)
    } else if (rows_diff < 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(transformed_weighted_absolute$df_males), nrow = abs(rows_diff)))
      colnames(data_to_pad)<-colnames(transformed_weighted_absolute$df_males)
      
      transformed_weighted_absolute$df_males <- rbind(transformed_weighted_absolute$df_males, data_to_pad)
    }
    combined_data_male <- cbind(old_data_male, transformed_weighted_absolute$df_males)
  }
  
  # Female data
  if(is.null(old_data_female)) {
    combined_data_female <- transformed_weighted_absolute$df_females
  } else {
    rows_diff <- nrow(transformed_weighted_absolute$df_females) - nrow(old_data_female)
    if (rows_diff > 0) {
      data_to_pad<-data.frame(matrix(ncol = ncol(old_data_female), nrow = rows_diff))
      colnames(data_to_pad)<-colnames(old_data_female)
      old_data_female <- rbind(old_data_female, data_to_pad)
    } else if (rows_diff < 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(transformed_weighted_absolute$df_females), nrow = abs(rows_diff)))
      colnames(data_to_pad)<-colnames(transformed_weighted_absolute$df_females)
      
      transformed_weighted_absolute$df_females <- rbind(transformed_weighted_absolute$df_females, data_to_pad)
    }
    combined_data_female <- cbind(old_data_female, transformed_weighted_absolute$df_females)
  }
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add the sheets
  addWorksheet(wb, "male")
  addWorksheet(wb, "female")
  
  # Write the data
  writeData(wb, "male", combined_data_male)
  writeData(wb, "female", combined_data_female)
  
  # Save the workbook
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

#updates files with new forecast evaluation results
update_forecasts_and_life_expectancy <- function(file_name2, full_data_male,full_data_female, jump_off_year, project_to_this_year, country_label) {
  require(openxlsx)
  

  old_data_male <- read.xlsx(file_name2, sheet = "male")
  old_data_female <- read.xlsx(file_name2, sheet = "female")
  

  # Male data
  if(is.null(old_data_male)) {
    combined_data_male <- full_data_male
  } else {
    rows_diff <- nrow(full_data_male) - nrow(old_data_male)
    if (rows_diff > 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(old_data_male), nrow = rows_diff))
      colnames(data_to_pad)<-colnames(old_data_male)      
      
      old_data_male <- rbind(old_data_male, data_to_pad)
    } else if (rows_diff < 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(full_data_male), nrow = abs(rows_diff)))
      colnames(data_to_pad)<-colnames(full_data_male)
      
      full_data_male <- rbind(full_data_male, data_to_pad)
    }
    combined_data_male <- cbind(old_data_male, full_data_male)
  }
  
  # Female data
  if(is.null(old_data_female)) {
    combined_data_female <- full_data_female
  } else {
    rows_diff <- nrow(full_data_female) - nrow(old_data_female)
    if (rows_diff > 0) {
      data_to_pad<-data.frame(matrix(ncol = ncol(old_data_female), nrow = rows_diff))
      colnames(data_to_pad)<-colnames(old_data_female)
      old_data_female <- rbind(old_data_female, data_to_pad)
    } else if (rows_diff < 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(full_data_female), nrow = abs(rows_diff)))
      colnames(data_to_pad)<-colnames(full_data_female)
      
      full_data_female <- rbind(full_data_female, data_to_pad)
    }
    combined_data_female <- cbind(old_data_female, full_data_female)
  }
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add the sheets
  addWorksheet(wb, "male")
  addWorksheet(wb, "female")
  
  # Write the data
  writeData(wb, "male", combined_data_male)
  writeData(wb, "female", combined_data_female)
  
  # Save the workbook
  saveWorkbook(wb, file_name2, overwrite = TRUE)
}

# Create a function to transform your forecast errors to the desired format:
transform_forecast_errors <- function(errors, jump_off_year, project_to_this_year, country_label) {
  
  #we get the maximum length of a sublist
  sublist_lengths <- unlist(lapply(errors, function(x) {
    sapply(x, length)
  }))
  
  # Return maximum length
  max_length <- max(sublist_lengths)
  
  
  #create empty dataframe to store these results
  df_males<-data.frame(matrix(ncol = length(errors), nrow = max_length + 4))
  df_females<-df_males
  
  current.date <- Sys.time()
  date_string <- format(current.date, "%Y-%m-%d %H:%M")
  
  
  for (go_through_methods in 1:length(errors)){
    
    
    
    temp_col_males <- data.frame(
      Value = c(
        names(errors)[go_through_methods],
        "males",
        jump_off_year,
        project_to_this_year,
        country_label,
        date_string,
        errors[[go_through_methods]]$male
      ),
      stringsAsFactors = FALSE
    )
    
    temp_col_females <- data.frame(
      Value = c(
        names(errors)[go_through_methods],
        "females",
        jump_off_year,
        project_to_this_year,
        country_label,
        date_string,
        errors[[go_through_methods]]$female
      ),
      stringsAsFactors = FALSE
    )
    
    df_males[1:nrow(temp_col_males),go_through_methods] <- temp_col_males
    df_females[1:nrow(temp_col_females),go_through_methods] <- temp_col_females
    
  }
  
  
  return(list(df_males = as.data.frame(df_males), df_females = as.data.frame(df_females)))
}

# Create a function to transform your forecast errors to the desired format:
bind_forecast_results <- function(forecasts, jump_off_year, project_to_this_year, country_label,list_of_list_elements,sex_label1,actuals_label) 
  {
  

  # Get the number of rows in the "males_projections" object of each sublist
  #assume same for males and females
  num_rows <- lapply(forecasts, function(x) nrow(x[[list_of_list_elements]]))
  
  # Get the maximum number of rows
  max_rows <- max(unlist(num_rows))
  
  current.date <- Sys.time()
  date_string <- format(current.date, "%Y-%m-%d %H:%M")
  
  temp_list_of_lists<-list()
  
  #create empty dataframe to store these results
  for (go_through_methods in 1:(length(forecasts)+1))
    {

    if (go_through_methods ==(length(forecasts)+1))
      {
       df1<-data.frame(matrix(ncol = ncol(forecasts[[1]][[actuals_label]]), nrow = max_rows + 7))
       df1[1,]<-'ACTUAL'
       df1[2,]<-jump_off_year
       df1[3,]<-project_to_this_year
       df1[4,]<-country_label
       df1[5,]<-date_string
       df1[6,]<-sex_label1
       df1[7,]<-colnames(forecasts[[1]][[actuals_label]])
       df1[8:(max_rows+7),]<-forecasts[[1]][[actuals_label]]
     }else{
         df1<-data.frame(matrix(ncol = ncol(forecasts[[go_through_methods]][[list_of_list_elements]]), nrow = max_rows + 7))
         df1[1,]<-names(forecasts)[go_through_methods]
         df1[2,]<-jump_off_year
         df1[3,]<-project_to_this_year
         df1[4,]<-country_label
         df1[5,]<-date_string
         df1[6,]<-sex_label1
         df1[7,]<-colnames(forecasts[[go_through_methods]][[list_of_list_elements]])
         df1[8:(max_rows+7),]<-forecasts[[go_through_methods]][[list_of_list_elements]]
       }

     temp_list_of_lists[[go_through_methods]]<-df1
    
    }
    
  
  
  #now let's bind the arrays into one dataframe each for males and females
  # Initialize empty dataframes for males and females
  df_combined <- data.frame()
  
  
  # Determine the max number of rows for males and females
  max_rows1 <- max(sapply(temp_list_of_lists, function(x) nrow(x)))
  
  
  
  # Bind columns together for males
  for(i in seq_along(temp_list_of_lists)) {
  
    temp_df <- temp_list_of_lists[[i]]
    rows_diff <- max_rows1 - nrow(temp_df)
    
    # If the temp_df has less rows than max_rows_males, add rows
    if (rows_diff > 0) {
      temp_df <- rbind(temp_df, data.frame(matrix(NA, ncol = ncol(temp_df), nrow = rows_diff)))
    }
    
    if (i==1){
      df_combined<-temp_df
    }else{
    
      # Column bind the temp_df to the combined dataframe
      df_combined <- cbind(df_combined, temp_df)
    }
  }
  
  
  
  return(df_combined)
}

# This function will create a new Excel file with a worksheet for each year in the forecast horizon. 
#Each column in each worksheet will represent the forecast results for the corresponding year in the forecast horizon, 
#for a specific method. The first 4 rows of each column will contain the method name, the jump off year, the last year
#of the forecast horizon, and the label. From the 5th row onwards, the forecast results will be written.

write_the_forecasts_to_file <- function(forecasts, data_cols, year1, year2, label1) {
  
  # Load necessary library
  # require(openxlsx)
  
  # Set the file name
  file_name <- "the_forecasts_by_year.xlsx"
  
  # Initialize the workbook
  if (!file.exists(file_name)) {
    wb <- createWorkbook()
    saveWorkbook(wb, file_name, overwrite = TRUE)
  }
  
  wb <-loadWorkbook(file_name)
  # Loop over the forecast years
  for (year in 1:(year2-year1)) {
    
    # Read the existing data for the current year
    
    #check if the sheet exists. If it does load the data. Otherwise set old_data to be blank
    for (check_if_sheet_exists in names(wb)){
      if (check_if_sheet_exists == paste("Year", year)){
        old_data<-wb[[check_if_sheet_exists]]
      }else{
        old_data<-NULL
      }
    }
    
    # Initialize the new data for the current year
    #+1 as we will also save the actual data
    new_data <- data.frame(matrix(ncol = length(forecasts)+1, nrow = length(data_cols)))
    
    # Loop over the methods
    for (method in 1:length(forecasts)) {
      
      # Write the method details
      new_data[1, method] <- names(forecasts)[method]
      new_data[2, method] <- year1
      new_data[3, method] <- year2
      new_data[4, method] <- label1
      
      # Write the forecast results
      new_data[5:nrow(new_data), method] <- forecasts[[method]]$males_projections[, year]
      new_data[(nrow(new_data)+1):(nrow(new_data)+nrow(forecasts[[method]]$females_projections)), method] <- forecasts[[method]]$females_projections[, year]
    }
    
    # Combine the old and new data
    combined_data <- rbind(old_data, new_data)
    
    # Write the combined data to the file
    write.xlsx(combined_data, file_name, sheetName = paste("Year", year), append = TRUE, row.names = FALSE)
  }
}

#updates files with new forecast evaluation results
update_forecasts_and_life_expectancy <- function(file_name2, full_data_male,full_data_female, jump_off_year, project_to_this_year, country_label) {
  require(openxlsx)
  

  old_data_male <- read.xlsx(file_name2, sheet = "male")
  old_data_female <- read.xlsx(file_name2, sheet = "female")
  

  # Male data
  if(is.null(old_data_male)) {
    combined_data_male <- full_data_male
  } else {
    rows_diff <- nrow(full_data_male) - nrow(old_data_male)
    if (rows_diff > 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(old_data_male), nrow = rows_diff))
      colnames(data_to_pad)<-colnames(old_data_male)      
      
      old_data_male <- rbind(old_data_male, data_to_pad)
    } else if (rows_diff < 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(full_data_male), nrow = abs(rows_diff)))
      colnames(data_to_pad)<-colnames(full_data_male)
      
      full_data_male <- rbind(full_data_male, data_to_pad)
    }
    combined_data_male <- cbind(old_data_male, full_data_male)
  }
  
  # Female data
  if(is.null(old_data_female)) {
    combined_data_female <- full_data_female
  } else {
    rows_diff <- nrow(full_data_female) - nrow(old_data_female)
    if (rows_diff > 0) {
      data_to_pad<-data.frame(matrix(ncol = ncol(old_data_female), nrow = rows_diff))
      colnames(data_to_pad)<-colnames(old_data_female)
      old_data_female <- rbind(old_data_female, data_to_pad)
    } else if (rows_diff < 0) {
      
      data_to_pad<-data.frame(matrix(ncol = ncol(full_data_female), nrow = abs(rows_diff)))
      colnames(data_to_pad)<-colnames(full_data_female)
      
      full_data_female <- rbind(full_data_female, data_to_pad)
    }
    combined_data_female <- cbind(old_data_female, full_data_female)
  }
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add the sheets
  addWorksheet(wb, "male")
  addWorksheet(wb, "female")
  
  # Write the data
  writeData(wb, "male", combined_data_male)
  writeData(wb, "female", combined_data_female)
  
  # Save the workbook
  saveWorkbook(wb, file_name2, overwrite = TRUE)
}

#forecasts with the demography package
forecasts <- forecast_mortality(
  HMD_data = Country_Data,
  year1 = jump_off_year,
  year2 = project_to_this_year,
  label1 = country_label1
)


# write the forecast errors to file for each forecasting method
write_lists_to_excel(forecasts)

#write the summary workbooks
data_cols=1:ncol(forecasts$lc_D$males_projections)

# Run the function

#Error calc
#
 #we have the +3 to the data_cols because the life table function has 3 columns before the data begins
 weighted_absolute_errors <- calculate_weighted_absolute_error(forecasts,(data_cols+3))
 weighted_error<- calculate_weighted_absolute_error(forecasts,(data_cols+3))
 unweighted_absolute_errors<-calculate_unweighted_absolute_error(forecasts,(data_cols+3))
 unweighted_errors<-calculate_unweighted_error(forecasts,(data_cols+3))

 life_expectancy_at_selected_age<-calculate_life_expectancy_at_selected_age(forecasts,(data_cols+3),65)

 life_expectancies_at_birth<-calculate_life_expectancy_at_selected_age(forecasts,(data_cols+3),0)
 
 filename1=paste0(getwd(), "/forecasts/","Weighted_Absolute_Forecast_errors.xlsx")
 update_forecast_errors(filename1, weighted_absolute_errors, jump_off_year, project_to_this_year, country_label1)

 filename2=paste0(getwd(), "/forecasts/","Weighted_Forecast_errors.xlsx")
 update_forecast_errors(filename2, weighted_error, jump_off_year, project_to_this_year, country_label1)

 filename3=paste0(getwd(), "/forecasts/","Mean_Absolute_Forecast_errors.xlsx")
 update_forecast_errors(filename3, unweighted_absolute_errors, jump_off_year, project_to_this_year, country_label1)

 filename4=paste0(getwd(), "/forecasts/","Mean_Forecast_errors.xlsx")
 update_forecast_errors(filename4, unweighted_errors, jump_off_year, project_to_this_year, country_label1)

 filename5=paste0(getwd(), "/forecasts/","life_expectancies_at_birth.xlsx")
 update_forecast_errors(filename5, life_expectancies_at_birth, jump_off_year, project_to_this_year, country_label1)

 filename6=paste0(getwd(), "/forecasts/","life_expectancies_at_age_65.xlsx")
 update_forecast_errors(filename6, life_expectancy_at_selected_age, jump_off_year, project_to_this_year, country_label1)

 #####
 #bind the forecast of mortality rates

 filename_forecasts_database=paste0(getwd(), "/forecasts/","forecasts_database.xlsx")

 bind_forecast_results_males <- bind_forecast_results(forecasts, jump_off_year, project_to_this_year, country_label1,"males_projections","male","ACTUAL_rates_male")

 bind_forecast_results_females <- bind_forecast_results(forecasts, jump_off_year, project_to_this_year, country_label1,"females_projections","female","ACTUAL_rates_female")

 update_forecasts_and_life_expectancy(filename_forecasts_database, bind_forecast_results_males,bind_forecast_results_females, jump_off_year, project_to_this_year, country_label1)
 #####

 #bind the life expectancy values
 filename_life_expectancy_database=paste0(getwd(), "/forecasts/","life_expectancies_database.xlsx")
 bind_life_expectancy_results_males <- bind_forecast_results(forecasts, jump_off_year, project_to_this_year, country_label1,"ex_male_proj","male","ACTUAL_ex_male")

 bind_life_expectancy_results_females <- bind_forecast_results(forecasts, jump_off_year, project_to_this_year, country_label1,"ex_female_proj","female","ACTUAL_ex_female")

 update_forecasts_and_life_expectancy(filename_life_expectancy_database, bind_life_expectancy_results_males,bind_life_expectancy_results_females, jump_off_year, project_to_this_year, country_label1)

