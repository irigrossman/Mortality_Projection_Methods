#Script Name: Ediev-like mortality forecasting functions
#Author: Irina Grossman
#Contact: irina.grossman@unimelb.edu.au
#Date: 20/03/2024


# This script contains a set of functions designed to perform mortality projections using input population and deaths data. 
# The input data must contain a column called 'Sex' with data for males and females organized in blocks for each sex. 
# Ages must be in rows and years must be in columns, with a separate column called 'Age' preceding the year columns. 
# Users must specify the starting column of the yearly data with the 'col_first_year_base' argument, 
# and the last column of the yearly data is assumed to be the last column of the worksheet. 
# Projections will be created for 'x' years past the jump-off year, where 'x' is defined by the user.
# 
# The functions in this script follow a multi-step procedure for creating mortality projections. 
# First, the  death rates are calculated for each age/sex group. Next, death rates are smoothed to age 110. 
# Then, optimal fitting periods are calculated for each age/sex group, and a linear model is fit to each age/sex group to obtain the slope and intercept of the model.
# The long-run slope is then heavily smoothed, and adjustments are made to ensure that death rates increase with age starting from age 15. 
# The script then creates Projection A: ASDRs from basic log-linear extrapolation without adjustments (for analysis purposes only).
# 
# Next, the script smooths the age pattern of the final year in the base period from the log-linear model. 
# This smoothing is necessary to prevent a bumpy age profile of ASDRs for the final year of the base period.
# The script then trends in the long-run smooth slopes, with the default trend-in period set to 20 years. 
# If the trend-in period is long, it may take a long time to transition from the jagged slope from the last year 
# of the base period to the smooth slopes, which may create jagged ASDR age profiles.
# 
# The script then performs spline knots and calculates the slope at the knots. The spline is then applied to the age pattern of slope values. 
# The projection of ln(ASDRs) is then created using the fitted model with adjusted slope, resulting in Projection B: ASDRs from log-linear extrapolation with adjusted slope.

# Next, the script creates Projection C: ASDRs from log-linear extrapolation with adjusted slope and jump-off adjustment. 
# Finally, the script creates the final projection, Projection D: Trending in jump-off shifted ASDRs to slope-adjusted ASDRs.
# Next we have a function to calculate life tables using ASDRs
# Overall, this script provides a set of functions for performing Ediev-like mortality projections.

####Change to your folder path

# R version 4.3.1 Beagle Scouts 
# Rstudio 2023.06.0 Build 421

#Required packages
library(pracma) #version 2.4.2
library(ggplot2) #version 3.4.2
library(openxlsx) #version 4.2.5.2
library(readxl) #version 1.4.2
library(demography) #version 2.0



#Functions

read_user_define_values <- function(){
  
  #read the user_defined_values file
  user_defined_xlsx <- read.xlsx("user_defined_values.xlsx", sheet = "values", colNames=FALSE)
  
  user_defined_values_list <- list()
  
  user_defined_values_list$use_pop_and_deaths <- user_defined_xlsx[3,2]
  user_defined_values_list$use_death_rates <- user_defined_xlsx[4,2]
  user_defined_values_list$populations_file_path <- user_defined_xlsx[7,2]
  user_defined_values_list$deaths_file_path <- user_defined_xlsx[8,2]
  user_defined_values_list$death_rates_file_path <- user_defined_xlsx[10,2]
  user_defined_values_list$output_file_path <- user_defined_xlsx[12,2]
  user_defined_values_list$first_data_col <- as.numeric(user_defined_xlsx[14,2])
  user_defined_values_list$jump_off <- as.numeric(user_defined_xlsx[18,2])
  user_defined_values_list$project_to <- as.numeric(user_defined_xlsx[19,2])
  
  user_defined_values_list$male_smoothing_type <- as.numeric(user_defined_xlsx[22,2])
  user_defined_values_list$female_smoothing_type <- as.numeric(user_defined_xlsx[23,2])
  
  user_defined_values_list$forecast_max_age <- as.numeric(user_defined_xlsx[26,2])
  user_defined_values_list$output_max_age <- as.numeric(user_defined_xlsx[27,2])
  user_defined_values_list$set_max_values <- user_defined_xlsx[30,2]
  user_defined_values_list$female_max_value <- as.numeric(user_defined_xlsx[31,2])
  user_defined_values_list$male_max_value <- as.numeric(user_defined_xlsx[32,2])
  user_defined_values_list$min_length_base_data <- as.numeric(user_defined_xlsx[34,2])
  user_defined_values_list$trend_in_long_run_slopes <- as.numeric(user_defined_xlsx[35,2])
  user_defined_values_list$trend_in_for_projection <- as.numeric(user_defined_xlsx[36,2])
  user_defined_values_list$include_jump_off_year <- as.numeric(user_defined_xlsx[37,2])
  
  start_row_spline<-which(user_defined_xlsx[,2]=='START_S')+1
  end_row_spline<-which(user_defined_xlsx[,2]=='END_S')-1
  
  user_defined_values_list$spline_values <- user_defined_xlsx[start_row_spline:end_row_spline, 2:3]
  user_defined_values_list$spline_values[] <- lapply(user_defined_values_list$spline_values, as.numeric)
  
  start_row_heavy_spline<-which(user_defined_xlsx[,2]=='START_HS')+1
  end_row_heavy_spline<-which(user_defined_xlsx[,2]=='END_HS')-1
  
  user_defined_values_list$heavy_spline_values <- user_defined_xlsx[start_row_heavy_spline:end_row_heavy_spline, 2:3]
  user_defined_values_list$heavy_spline_values[] <- lapply(user_defined_values_list$heavy_spline_values, as.numeric)
  
  start_row_spline2<-which(user_defined_xlsx[,2]=='START_S2')+1
  end_row_spline2<-which(user_defined_xlsx[,2]=='END_S2')-1
  
  user_defined_values_list$spline_values2 <- user_defined_xlsx[start_row_spline2:end_row_spline2, 2:3]
  user_defined_values_list$spline_values2[] <- lapply(user_defined_values_list$spline_values2, as.numeric)
  
  if (is.na(user_defined_values_list$death_rates_file_path) &&
      (is.na(user_defined_values_list$deaths_file_path)||
       is.na(user_defined_values_list$populations_file_path))){
    stop("Please provide age specific death rates, or base populations and base deaths data")
  }
  
  # read pop, deaths, ASDRs
  matches <- grepl("\\s*yes\\s*", user_defined_values_list$use_pop_and_deaths, ignore.case = TRUE)
  
  if (sum(matches)>=1){
    user_defined_values_list$deaths1<-read.csv(user_defined_values_list$deaths_file_path)
    user_defined_values_list$populations1<-read.csv(user_defined_values_list$populations_file_path)
    user_defined_values_list$death_rates1<- NULL
    
  }else{
    user_defined_values_list$deaths1 <- NULL
    user_defined_values_list$populations1 <- NULL
    user_defined_values_list$death_rates1 <- read.csv(user_defined_values_list$death_rates_file_path)
  }
  
  
  return(user_defined_values_list)
}

diff_array<-function(excel_array,r_array){
  diff_array<-abs((excel_array-r_array)/excel_array)*100
  return(diff_array)
}

spline_knot<-function(Ages_splines_values2,smoothing_windows_spline_values2,death_rates,spline_data_cols,max_value=NULL){
  #spline_data_cols is the column index in the death_rates dataframe containing the required data
  #death_rates needs a column called "Age"
  
  ages_knots_array<-Ages_splines_values2
  Ages_splines_row_number <- ages_knots_array +1
  
  Knots_forASDR_splines<-death_rates[Ages_splines_row_number,]
  
  Knots_forASDR_splines$Age<-ages_knots_array
  
  n_ages_knots<-length(ages_knots_array)
  
  col_first_year_base <- min(spline_data_cols)
  col_last_year_base <- max(spline_data_cols)
  
  for (count_through_cols in (col_first_year_base):(col_last_year_base))
    for (count_through_ages in 1:(n_ages_knots)){
      current_window_width<-smoothing_windows_spline_values2[count_through_ages]
      
      if (current_window_width == -1){
        Knots_forASDR_splines[count_through_ages,count_through_cols]<-max_value
      }else if (current_window_width == 0) {
        Knots_forASDR_splines[count_through_ages,count_through_cols]<-0
      }
      else {
        either_side<-(current_window_width-1)*0.5
        Knots_forASDR_splines[count_through_ages,count_through_cols]<-
          sum( death_rates[ (Ages_splines_row_number[count_through_ages]-either_side):(Ages_splines_row_number[count_through_ages]+either_side) ,count_through_cols])/current_window_width
      }
      
    }
  
  return(Knots_forASDR_splines)
  
}

increase_df_size<-function(dataframe1,max_age_ExS_data,max_age_ExS_after_smoothing,col_first_year_base){
  
  smoothed_dataframe1<-dataframe1
  if (max_age_ExS_after_smoothing>max_age_ExS_data){
    
    #create new rows with 0 values
    smoothed_dataframe1[((nrow(smoothed_dataframe1)+1):(nrow(smoothed_dataframe1)+(max_age_ExS_after_smoothing-max_age_ExS_data))),]<-0
    
    #let's initiate the variables in the columns before the time series data to be the same as the one in the last row
    smoothed_dataframe1[((nrow(dataframe1)+1):(nrow(dataframe1)+(max_age_ExS_after_smoothing-max_age_ExS_data))),(1:(col_first_year_base-1))]<-smoothed_dataframe1[nrow(dataframe1),1:(col_first_year_base-1)]
    
    #now update the ages so that they go up in single ages to the max age after smoothing
    smoothed_dataframe1$Age[((nrow(dataframe1)+1):(nrow(dataframe1)+(max_age_ExS_after_smoothing-max_age_ExS_data)))]<-(max_age_ExS_data+1):max_age_ExS_after_smoothing
    
  }
  return(smoothed_dataframe1)
}

change_age_output<-function(ASDRs_data,proj_cols1,max_age_ExS_output){

  out_lt<-life_table_function_ES(ASDRs_data,proj_cols1)
  
  max_row_smoothed <- nrow(ASDRs_data)
  max_row_output   <- max_age_ExS_output+1 
  
  dr_max_age_ExS_unsmoothed_plus <- ASDRs_data[max_row_output:max_row_smoothed, proj_cols1]
  
  temp_lx<-out_lt$nlx
  
  if (nrow(out_lt$nlx)>max_row_smoothed)
  {
    new_lx<-temp_lx[1:max_row_smoothed,]
    new_lx[nrow(new_nlx),proj_cols1 ]<-colSums(temp_lx[max_row_smoothed:nrow(temp_lx),proj_cols1])
    
    
  }else{
    new_lx <- temp_lx
  }
  
  lx_max_age_ExS_unsmoothed_plus <- new_lx[max_row_output:max_row_smoothed, proj_cols1 ]
  
  # Sum the deaths and person-years for ages 105 and above
  deaths_max_output_plus <- as.data.frame(t(colSums(dr_max_age_ExS_unsmoothed_plus * lx_max_age_ExS_unsmoothed_plus)))
  person_years_max_output_plus <- as.data.frame(t(colSums(lx_max_age_ExS_unsmoothed_plus)))
  
  # Calculate the death rate for all people age 105 and above
  death_rate_max_output_plus <- deaths_max_output_plus / person_years_max_output_plus
  
  projects_age_adjusted<-ASDRs_data[1:max_row_output,]
  projects_age_adjusted[max_row_output,proj_cols1]<-death_rate_max_output_plus
  
  return(projects_age_adjusted)
  
}

#the life_table_function calculates life table functions given a dataframe of age-specific death rates (ASDRs) and 
#additional columns containing age and variable descriptors. The function first calculates nmx, which represents 
#the number of people surviving to age x, and then uses nmx to calculate nax, which is the average number of years
#lived between ages x and x+n by those dying in the interval. The function then calculates nqx, which is the 
#probability of dying between ages x and x+n, and npx, which is the probability of surviving between ages x and x+n. 
#Using nqx and npx, the function calculates lx, the number of people surviving to age x, and ndx, the number of deaths 
#in the age interval x to x+n. Using ndx and nax, the function calculates nlx, the number of person-years lived between
#ages x and x+n, and tx, the total number of person-years lived after age x. Finally, the function calculates ex, 
#the life expectancy at age x. The function returns a list of all the life table functions: nmx, nax, nqx, nlx, lx, tx, and ex.
life_table_function_ES<-function(Smoothed_ASDRs,data_cols_ES,write_excel_lifetable=NULL){
  
  start_data_col<-data_cols_ES[1]
  nmx <- as.data.frame(Smoothed_ASDRs)
  #step 1 create lifetable using the demography package
  demogdata_lt <- demogdata(
    data = Smoothed_ASDRs[,data_cols_ES],
    ages = Smoothed_ASDRs$Age,
    pop = Smoothed_ASDRs[,data_cols_ES], #input not used for lifetables.
    years = 1:length(data_cols_ES),
    type = "mortality",
    label = "label",
    name = "name"
  )
  
  
  
  max_age_ExS<-max(Smoothed_ASDRs$Age)
  
  lt1 <- lifetable(demogdata_lt,max.age=max_age_ExS)
  
  info_cols_to_add<-Smoothed_ASDRs[,1:(min(data_cols_ES)-1)]
  
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
    combined_full_life_table[, data_cols_ES] <- sapply(combined_full_life_table[, data_cols_ES], as.numeric)
    
    
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

  temp_data1<-cbind(as.data.frame(ages1),as.data.frame(ages1),as.data.frame(ages1),rates1)
  colnames(temp_data1)[1]<-'Variable' #buffer column
  colnames(temp_data1)[2]<-'Sex' #buffer column
  colnames(temp_data1)[3]<-'Age'
  
  life_table1 <- life_table_function_ES(
    temp_data1,
    4:ncol(temp_data1)
  )
  column_after_AGE <- which(colnames(life_table1$ndx)=="Age")+1
  data_cols_life_tables <- column_after_AGE:ncol(life_table1$ndx)
  
  
  data_list1 <-list(nmx = life_table1$nmx, 
                    nqx = life_table1$nqx, 
                    ndx = life_table1$ndx,
                    nlx = life_table1$nlx,
                    Lx = life_table1$Lx, 
                    tx=life_table1$Tx, 
                    ex = life_table1$ex,
                    rx = life_table1$rx
  )
  
  combined_full_life_table1 <- do.call(rbind, lapply(data_list1, function(x) rbind(x, rep("", ncol(x)))))
  
  
  # Convert specified columns to numeric
  combined_full_life_table1[, data_cols_life_tables] <- sapply(combined_full_life_table1[, data_cols_life_tables], as.numeric)
  
  return(list(ndx = life_table1$ndx, nlx = life_table1$nlx, combined_full_life_table = combined_full_life_table1, ex = life_table1$ex))
  
}

#3 Find ideal start year of the fitting period for each age

BestLinearFit <- function(selected_x, selected_y, min_num) {
  
  # This function calculates the initial year (or data point) of a base period
  # which gives the best R^2 when fitting a straight line. The end of the base period
  # is assumed to be the last available data point selected by the user.
  
  # selected_y = vector of y-axis variables
  # selected_x = vector of x-axis variables
  # min_num  = miniumum number of data pairs used for fitting the straight line
  # best_x = best initial year (or data point) for fitting the straight line
  # slope = slope of the linear model that is fit when the fitting period begins at best_x
  # intercept = intercept of the linear model that is fit when the fitting period begins at best_x
  
  #----------------------------------------------------------------------------------------
  
  # Variable declarations
  totlength <- length(selected_x)
  r_sq <- numeric(totlength - min_num)
  best_x <- 0
  best_r_sq <- 0
  best_slope <- 0
  best_intercept <- 0
  
  for (z in 1:(totlength - min_num)) {
    # Fill progressively smaller temporary vectors of y and x values
    temp_y <- selected_y[z:totlength]
    temp_x <- selected_x[z:totlength]
    # Fit a linear model to the temporary data
    
    if ((sd(temp_y)>1e-5)){
      fit <- lm(as.numeric(temp_y) ~ as.numeric(temp_x))
      # Find out the r squared of the fitted model
      r_sq[z] <- summary(fit)$r.squared
      # Keep track of the best model so far
      if (r_sq[z] > best_r_sq) {
        best_r_sq <- r_sq[z]
        best_x <- selected_x[z]
        best_slope <- coef(fit)[2]
        best_intercept <- coef(fit)[1]
      }
    }else{
      print("skipped year due to low variance")
    }
  }
  
  return(list(best_x = best_x, slope = best_slope, intercept = best_intercept))
}


clean_unsmoothed <- function(unsmoothed_ASDRs, data_cols_ES) {
  
  #replace values greater than 1 with 1
  temp_array<-unsmoothed_ASDRs[,data_cols_ES]
  temp_array[temp_array > 1] <- 1 
  unsmoothed_ASDRs[,data_cols_ES] <- temp_array
  
  count_changes<-0
  for (check_finite_rows in 1:nrow(unsmoothed_ASDRs)) {
    for (check_finite_cols in data_cols_ES) {
      # Identify surrounding cells
      
      if ((is.infinite(unsmoothed_ASDRs[check_finite_rows,check_finite_cols])) || 
          (is.na(unsmoothed_ASDRs[check_finite_rows,check_finite_cols])) ||
          (unsmoothed_ASDRs[check_finite_rows,check_finite_cols] == 0) ||
          (unsmoothed_ASDRs[check_finite_rows,check_finite_cols] == -Inf)) {
        count_changes <- count_changes + 1
        row_range <- pmax(1, check_finite_rows - 1):pmin(nrow(unsmoothed_ASDRs), check_finite_rows + 1)
        col_range <- pmax(min(data_cols_ES), check_finite_cols - 1):pmin(max(data_cols_ES), check_finite_cols + 1)
        
        # Calculate average of surrounding cells (excluding NA, Inf, -Inf, and 0 values)
        surrounding_rates <- unsmoothed_ASDRs[row_range, col_range]
        
        surrounding_avg <- mean(surrounding_rates[!sapply(surrounding_rates, function(x) is.infinite(x) | is.na(x) | x == 0 | x == -Inf)], na.rm = TRUE)
        prev_value <- unsmoothed_ASDRs[check_finite_rows,check_finite_cols]
        unsmoothed_ASDRs[check_finite_rows,check_finite_cols] <- surrounding_avg
        # cat("row: ",check_finite_rows," col: ", check_finite_cols, "had value: ",prev_value, " new value: ",surrounding_avg)
      }
    }
  }
  
  

  
  return(unsmoothed_ASDRs)
}



   
#function to create spline knots using the user defined values
smoothe_death_rates<-function(user_defined_values_list,smoothing_variable_name){

  
  
  deaths1 <- user_defined_values_list$deaths1
  populations1 <- user_defined_values_list$populations1
  death_rates1 <- user_defined_values_list$death_rates1
  max_age_ExS_after_smoothing <- user_defined_values_list$forecast_max_age
  col_first_year_base <- user_defined_values_list$first_data_col
  
  if (is.null(death_rates1)){
    col_last_year_base <- ncol(populations1)
  }else{
    col_last_year_base <- ncol(death_rates1)
  }
  
  Ages_splines_values2 <- user_defined_values_list[[smoothing_variable_name]][,1]
  
  smoothing_windows_spline_values2 <- user_defined_values_list[[smoothing_variable_name]][,2]
  
  data_cols_ES<-col_first_year_base:col_last_year_base  
  
  
  if (is.null(death_rates1)){
    #if the input is deaths and populations, we clean the data, and remove NA, infinite values and 0s.
    base_period <- as.numeric(sub("[^0-9]*([0-9]+)$", "\\1", colnames(populations1)[data_cols_ES]))
    years_array_full_base <- base_period
    
    #let's remove spaces from the Sex column
    populations1$Sex<-(gsub("[ ]","",populations1$Sex))
    
    populations1$Age<-as.numeric(gsub("[+]","",populations1$Age))
    
    deaths1$Sex<-(gsub("[ ]","",deaths1$Sex))
    
    #2 remove the + from the Age column
    deaths1$Age<-as.numeric(gsub("[+]","",deaths1$Age))
    
    ages_index<-unique(deaths1$Age)
    
   Females_0_row<- which(deaths1$Sex=="Females" & deaths1$Age==min(ages_index))
    Females_max_row<- which(deaths1$Sex=="Females" & deaths1$Age==max(ages_index))
    
    Males_0_row<- which(deaths1$Sex=="Males" & deaths1$Age==min(ages_index))
    Males_max_row<- which(deaths1$Sex=="Males" & deaths1$Age==max(ages_index))
    
    #get the required data format to calculate persons. It is important that the label be persons as the
    #smoothing is done differently for persons than for males and females
    
    
    deaths_persons<-deaths1[Females_0_row:Females_max_row,]
    deaths_persons$Sex="Persons"
    
    populations1_persons<-populations1[Females_0_row:Females_max_row,]
    populations1_persons$Sex<-"Persons"
    
    
    deaths_persons[,col_first_year_base:col_last_year_base]<-deaths1[Females_0_row:Females_max_row,col_first_year_base:col_last_year_base]+
      deaths1[Males_0_row:Males_max_row,col_first_year_base:col_last_year_base]
    
    populations1_persons[,col_first_year_base:col_last_year_base]<-populations1[Females_0_row:Females_max_row,col_first_year_base:col_last_year_base]+
      populations1[Males_0_row:Males_max_row,col_first_year_base:col_last_year_base]
    
    
    females_unsmoothed_ASDRs_original <- populations1[Females_0_row:Females_max_row,]
    
    females_unsmoothed_ASDRs_original[, data_cols_ES] <- deaths1[Females_0_row:Females_max_row,data_cols_ES]/ populations1[Females_0_row:Females_max_row,data_cols_ES]
    
    females_unsmoothed_ASDRs<-clean_unsmoothed(females_unsmoothed_ASDRs_original,
                                               data_cols_ES)
    
    males_unsmoothed_ASDRs_original <- populations1[Males_0_row:Males_max_row,]
    males_unsmoothed_ASDRs_original[, data_cols_ES] <- deaths1[Males_0_row:Males_max_row,data_cols_ES]/ populations1[Males_0_row:Males_max_row,data_cols_ES] 
    
    males_unsmoothed_ASDRs<-clean_unsmoothed(males_unsmoothed_ASDRs_original,
                                             data_cols_ES)
    
  }else{
    #if the input is death rates, we clean the data, and remove NA, infinite values and 0s.
    
    base_period <- as.numeric(sub("[^0-9]*([0-9]+)$", "\\1", colnames(death_rates1)[data_cols_ES]))
    years_array_full_base <- base_period
    
    #let's remove spaces from the Sex column
    death_rates1$Sex<-(gsub("[ ]","",death_rates1$Sex))
    
    death_rates1$Age<-as.numeric(gsub("[+]","",death_rates1$Age))
    
    
    ages_index<-unique(death_rates1$Age)
    
    
    
    Females_0_row<- which(death_rates1$Sex=="Females" & death_rates1$Age==min(ages_index))
    Females_max_row<- which(death_rates1$Sex=="Females" & death_rates1$Age==max(ages_index))
    
    Males_0_row<- which(death_rates1$Sex=="Males" & death_rates1$Age==min(ages_index))
    Males_max_row<- which(death_rates1$Sex=="Males" & death_rates1$Age==max(ages_index))
    
    females_unsmoothed_ASDRs_original <- death_rates1[Females_0_row:Females_max_row,]
    
    females_unsmoothed_ASDRs<-clean_unsmoothed(females_unsmoothed_ASDRs_original,
                                               data_cols_ES)
    
    males_unsmoothed_ASDRs_original <- death_rates1[Males_0_row:Males_max_row,]
    
    
    males_unsmoothed_ASDRs<-clean_unsmoothed(males_unsmoothed_ASDRs_original,
                                             data_cols_ES)
    
  }
  

  if (toupper(user_defined_values_list$set_max_values) %in% c("YES", "Y")){
    # print("Inside if statement")
    max_value_female<-(user_defined_values_list$female_max_value)
    max_value_male<-(user_defined_values_list$male_max_value)
  } else {
    # print("Inside else statement")
    max_value_female<- NULL
    max_value_male<- NULL
  }
  
  Females_Spline_knot<-spline_knot(Ages_splines_values2,
                                   smoothing_windows_spline_values2,
                                   females_unsmoothed_ASDRs,
                                   col_first_year_base:col_last_year_base,
                                   max_value_female)
  
  females_unsmoothed_ASDRs<-increase_df_size(females_unsmoothed_ASDRs,max(females_unsmoothed_ASDRs$Age),max_age_ExS_after_smoothing,col_first_year_base)
  
  Females_Smoothed_ASDRs_smoothed_over_age<-females_unsmoothed_ASDRs
  Females_Smoothed_ASDRs_smoothed_over_age[1:3,]<-females_unsmoothed_ASDRs[1:3,]
  
  
  for (year_count in (col_first_year_base):col_last_year_base){
    
    Females_Smoothed_ASDRs_smoothed_over_age[(4):(nrow(Females_Smoothed_ASDRs_smoothed_over_age)),year_count]=cubicspline(Ages_splines_values2[3:(length(Ages_splines_values2))],
                                                                                                                          as.numeric(Females_Spline_knot[3:(length(Ages_splines_values2)),year_count]),
                                                                                                                          Females_Smoothed_ASDRs_smoothed_over_age$Age[(4):(nrow(Females_Smoothed_ASDRs_smoothed_over_age))])
    
  }
  
  num_na <- sum(is.na(Females_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES]))
  
  # Count the number of NaN values
  # Apply is.nan() to each vector in the list
  nan_flags <- lapply(Females_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES], is.nan)
  
  # Convert the flags to counts
  num_nan <- sum(unlist(nan_flags))
  
  inf_flags <- lapply(Females_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES], is.infinite)
  
  # Convert the flags to counts
  num_inf <- sum(unlist(inf_flags))
  
  num_neg_inf <- sum(Females_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES] == -Inf)
  # Count the number of values less than or equal to zero
  #num_le_zero <- sum(Females_Smoothed_ASDRs_smoothed_over_age <= 0, na.rm = TRUE)
  
  # Count the number of values less than or equal to zero
  num_le_zero <- sum(Females_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES] < 0, na.rm = TRUE)
  
  # 
  # # Print the counts
  print(paste("Number of NA female smoothed by age ASDR values replaced: ", num_na))
  print(paste("Number of NaN female smoothed by age ASDR values replaced: ", num_nan))
  print(paste("Number of inf female smoothed by age ASDR values replaced: ", num_inf))
  print(paste("Number of inf female smoothed by age ASDR values replaced: ", num_neg_inf))
  print(paste("Number of values less than zero  female smoothed by age ASDR values replaced: ", num_le_zero))
  # 

  
  Females_Smoothed_ASDRs_smoothed_over_age_and_time<-Females_Smoothed_ASDRs_smoothed_over_age
  
  for (year_count in (col_first_year_base+1):(col_last_year_base)){
    
    if (year_count==col_last_year_base){
      for (age_count in 1:nrow(Females_Smoothed_ASDRs_smoothed_over_age_and_time)){
        
        Females_Smoothed_ASDRs_smoothed_over_age_and_time[age_count,year_count]=sum(Females_Smoothed_ASDRs_smoothed_over_age[age_count,((year_count-1):(year_count))])/2
      }
    }else{
      
      for (age_count in 1:nrow(Females_Smoothed_ASDRs_smoothed_over_age_and_time)){
        
        Females_Smoothed_ASDRs_smoothed_over_age_and_time[age_count,year_count]=sum(Females_Smoothed_ASDRs_smoothed_over_age[age_count,((year_count-1):(year_count+1))])/3
      }
    }
  }
  
  
  
  #Now same for males
  Males_Spline_knot<-spline_knot(Ages_splines_values2,smoothing_windows_spline_values2,males_unsmoothed_ASDRs,col_first_year_base:col_last_year_base,(max_value_male))
  
  males_smoothed_ASDRs<-increase_df_size(males_unsmoothed_ASDRs,max(males_unsmoothed_ASDRs$Age),max_age_ExS_after_smoothing,col_first_year_base)
  
  Males_Smoothed_ASDRs_smoothed_over_age<-males_smoothed_ASDRs
  Males_Smoothed_ASDRs_smoothed_over_age[1:3,]<-males_unsmoothed_ASDRs[1:3,]
  
  
  for (year_count in (col_first_year_base):col_last_year_base){
    
    Males_Smoothed_ASDRs_smoothed_over_age[(4):(nrow(Males_Smoothed_ASDRs_smoothed_over_age)),year_count]=cubicspline(Ages_splines_values2[3:(length(Ages_splines_values2))],
                                                                                                                      as.numeric(Males_Spline_knot[3:(length(Ages_splines_values2)),year_count]),
                                                                                                                      Males_Smoothed_ASDRs_smoothed_over_age$Age[(4):(nrow(Males_Smoothed_ASDRs_smoothed_over_age))])
    
    
    
    
    
  }
  

  num_na <- sum(is.na(Males_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES]))
  
  # Count the number of NaN values
  # Apply is.nan() to each vector in the list
  nan_flags <- lapply(Males_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES], is.nan)
  
  # Convert the flags to counts
  num_nan <- sum(unlist(nan_flags))
  
  inf_flags <- lapply(Males_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES], is.infinite)
  
  # Convert the flags to counts
  num_inf <- sum(unlist(inf_flags))
  
  num_neg_inf <- sum(Males_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES] == -Inf)

  
  # Count the number of values less than or equal to zero
  num_le_zero <- sum(Males_Smoothed_ASDRs_smoothed_over_age[,data_cols_ES] < 0, na.rm = TRUE)
  
  
  # Print the counts
  print(paste("Number of NA male smoothed by age ASDR values replaced: ", num_na))
  print(paste("Number of NaN male smoothed by age ASDR values replaced: ", num_nan))
  print(paste("Number of inf male smoothed by age ASDR values replaced: ", num_inf))
  print(paste("Number of inf male smoothed by age ASDR values replaced: ", num_neg_inf))
  print(paste("Number of values less than zero  male smoothed by age ASDR values replaced: ", num_le_zero))

  
  Males_Smoothed_ASDRs_smoothed_over_age_and_time<-Males_Smoothed_ASDRs_smoothed_over_age
  
  for (year_count in (col_first_year_base+1):(col_last_year_base)){
    
    if (year_count==col_last_year_base){
      for (age_count in 1:nrow(Males_Smoothed_ASDRs_smoothed_over_age_and_time)){
        
        Males_Smoothed_ASDRs_smoothed_over_age_and_time[age_count,year_count]=sum(Males_Smoothed_ASDRs_smoothed_over_age[age_count,((year_count-1):(year_count))])/2
      }
    }else{
      
      for (age_count in 1:nrow(Males_Smoothed_ASDRs_smoothed_over_age_and_time)){
        
        Males_Smoothed_ASDRs_smoothed_over_age_and_time[age_count,year_count]=sum(Males_Smoothed_ASDRs_smoothed_over_age[age_count,((year_count-1):(year_count+1))])/3
      }
    }
  }
  
  
  return(list(females_unsmoothed_ASDRs=females_unsmoothed_ASDRs,
              # females_knots=females_knots,
              Females_Smoothed_ASDRs_smoothed_over_age=Females_Smoothed_ASDRs_smoothed_over_age,
              Females_Smoothed_ASDRs_smoothed_over_age_and_time=Females_Smoothed_ASDRs_smoothed_over_age_and_time,
              males_unsmoothed_ASDRs=males_unsmoothed_ASDRs,
              # males_knots=males_knots,
              Males_Smoothed_ASDRs_smoothed_over_age=Males_Smoothed_ASDRs_smoothed_over_age,
              Males_Smoothed_ASDRs_smoothed_over_age_and_time=Males_Smoothed_ASDRs_smoothed_over_age_and_time))
}


#function to create Ediev- like projections. 
#If is_Evaluation is 0 the variables can be defined when the function is called. 
#This allows the function to be called in an automated way when conducting an evaluation
#otherwise values are taken from the user_define_values.xlsx spreadsheet.



spline_smoothing_function<-function(spline_data, array1){
  #col 1 of spline_data is the ages for the knots
  #col 2 is the smoothing window
  #array1 contains the data for all ages. It has one column for ages and another for values
  #data in array1 should be arranged in single years, with the first row being for age 0
  ages_data1<-spline_data[,1]
  smoothing_window_data1<-spline_data[,2]
  
  out_array<- as.data.frame(matrix(data = NA,nrow = length(ages_data1),ncol = 1))
  
  nr <- nrow(array1)
  
  for (count_through_ages_for_knots in 1:length(ages_data1)){
    current_smoothing_window <- smoothing_window_data1[count_through_ages_for_knots]
    current_age <- ages_data1[count_through_ages_for_knots]
    
    #if less than 1 set it to whatever value is specified
    if (current_smoothing_window<1){
      
      out_array[count_through_ages_for_knots,1] <- current_smoothing_window
      
    }else if(current_smoothing_window==1){
      
      row_in_array1<-which(array1$age==current_age)
      out_array[count_through_ages_for_knots,1] <- array1$values[row_in_array1]
      
    }else{
      
      row_in_array1<-which(array1$age==current_age)

      window_half <- (current_smoothing_window-1) * 0.5
      out_array[count_through_ages_for_knots,1]<-mean(array1$values[(row_in_array1-window_half):(row_in_array1+window_half)])
      
    }
    
  }
  
  return(out_array)
}

create_projections<-function(evaluation_variables_list = NULL,
                             is_evaluation = 0){
  
  if (is_evaluation==0){
    user_defined_values_list <- read_user_define_values()
  } else{
    user_defined_values_list <- evaluation_variables_list
  }
  
  

  base_populations_for_projection = user_defined_values_list$populations1
  base_deaths_for_projection=user_defined_values_list$deaths1
  projection_years=(user_defined_values_list$jump_off):user_defined_values_list$project_to
  male_smoothing_type = user_defined_values_list$male_smoothing_type
  female_smoothing_type = user_defined_values_list$female_smoothing_type
  write_excel_projections=user_defined_values_list$output_file_path
  unsmoothed_ASDRs = user_defined_values_list$death_rates
  Ages_splines_values2 = user_defined_values_list$spline_values2 #row 1 is the ages, row 2 is the smoothing window
  first_data_col<-user_defined_values_list$first_data_col
  col_first_year_base<-first_data_col 

  #heavily smooth the long run slope
  Ages_heavy_splines_values = user_defined_values_list$heavy_spline_values
  
  max_age_ExS_after_smoothing <- user_defined_values_list$forecast_max_age
  
  if (!(is.null(user_defined_values_list$populations1))){
    data_cols_for_projection = first_data_col:ncol(base_populations_for_projection)
    base_period_for_proj <- as.numeric(sub("[^0-9]*([0-9]+)$", "\\1", colnames(base_populations_for_projection)[data_cols_for_projection]))
    years_array <- base_period_for_proj
  }else{
    data_cols_for_projection = first_data_col:ncol(unsmoothed_ASDRs)
    base_period_for_proj <- as.numeric(sub("[^0-9]*([0-9]+)$", "\\1", colnames(unsmoothed_ASDRs)[data_cols_for_projection]))
    years_array <- base_period_for_proj
  }
  
  min_num <- user_defined_values_list$min_length_base_data #20 #min length of base data to use to fit the best linear model
  trended_in_over_this_many_years <- user_defined_values_list$trend_in_for_projection  #50 #for projection D
  #Trending in of long-run smooth slopes
  trend_in_over_this_many_years <- user_defined_values_list$trend_in_long_run_slopes  #20
  
  
  smoothed_list<-smoothe_death_rates(user_defined_values_list,
                                     "spline_values2")
  #ASDR proj
  
  if (male_smoothing_type == 1){
    smoothest_ASDR_males   <- smoothed_list$Males_Smoothed_ASDRs_smoothed_over_age
  }else{
    smoothest_ASDR_males   <- smoothed_list$Males_Smoothed_ASDRs_smoothed_over_age_and_time
  }
  
  if (female_smoothing_type == 1){
    smoothest_ASDR_females <- smoothed_list$Females_Smoothed_ASDRs_smoothed_over_age
  }else{
    smoothest_ASDR_females <- smoothed_list$Females_Smoothed_ASDRs_smoothed_over_age_and_time
  }
  
  
  smoothed_ASDRs <- rbind(smoothest_ASDR_females,smoothest_ASDR_males)
  

  smoothest_ASDR_females <- subset(smoothed_ASDRs, Sex == "Females")
  smoothest_ASDR_males <- subset(smoothed_ASDRs, Sex == "Males")
  
  ln_smoothest_ASDR_females <- smoothest_ASDR_females
  ln_smoothest_ASDR_males   <- smoothest_ASDR_males
  
  
  ln_smoothest_ASDR_females[,data_cols_for_projection] <- log(smoothest_ASDR_females[,data_cols_for_projection])
  ln_smoothest_ASDR_males[,data_cols_for_projection]   <- log(smoothest_ASDR_males[,data_cols_for_projection])
  
  
  #find slope and intercept of straight lines
  #use the ln_smoothest_ASDR_females and the ln_smoothest_ASDR_males data
  
  best_year_slope_intercept_female <- ln_smoothest_ASDR_females[,1:(data_cols_for_projection[1]-1)]
  best_year_slope_intercept_male   <- ln_smoothest_ASDR_males[,1:(data_cols_for_projection[1]-1)]
  
  
  
  best_year_slope_intercept_female$best_base_start_year <- tail(base_period_for_proj,1)-20 
  best_year_slope_intercept_female$best_slope <- 0
  best_year_slope_intercept_female$best_intercept <- 0
  
  
  best_year_slope_intercept_male$best_base_start_year <- tail(base_period_for_proj,1)-20 
  best_year_slope_intercept_male$best_slope <- 0
  best_year_slope_intercept_male$best_intercept <- 0
  
  for (calc_best_year in 1:nrow(best_year_slope_intercept_female)){
    

    #The linear model requires that the data have some variation.
    #therefore we only run the BestLinearFit function if the standard variation
    #of the time series is >1e-5. If it is 0 we set the slope and intercept to 0
    #and we set the best_x as the first year in the base period
    if (sd(ln_smoothest_ASDR_females[calc_best_year,data_cols_for_projection])>1e-5){
      temp_array<- BestLinearFit((years_array), ln_smoothest_ASDR_females[calc_best_year,data_cols_for_projection], min_num)
      best_year_slope_intercept_female$best_base_start_year[calc_best_year] <- temp_array$best_x
      best_year_slope_intercept_female$best_slope[calc_best_year] <- temp_array$slope
      best_year_slope_intercept_female$best_intercept[calc_best_year] <- temp_array$intercept
      
    }else{
      
      best_year_slope_intercept_female$best_base_start_year[calc_best_year] <- 0
      best_year_slope_intercept_female$best_slope[calc_best_year] <- 0
      best_year_slope_intercept_female$best_intercept[calc_best_year] <- 0 
    }
    
  }
  
  for (calc_best_year in 1:nrow(best_year_slope_intercept_male)){
    if (sd(ln_smoothest_ASDR_males[calc_best_year,data_cols_for_projection])>1e-5){
      temp_array<- BestLinearFit((years_array), ln_smoothest_ASDR_males[calc_best_year,data_cols_for_projection], min_num)
      best_year_slope_intercept_male$best_base_start_year[calc_best_year] <- temp_array$best_x
      best_year_slope_intercept_male$best_slope[calc_best_year] <- temp_array$slope
      best_year_slope_intercept_male$best_intercept[calc_best_year] <- temp_array$intercept
      
    }else{
      
      best_year_slope_intercept_male$best_base_start_year[calc_best_year] <- 0
      best_year_slope_intercept_male$best_slope[calc_best_year] <- 0
      best_year_slope_intercept_male$best_intercept[calc_best_year] <- 0 
    }
  }
  
  
  #heavily smoothed for females
  heavily_smoothed_array_female=as.data.frame(matrix(data=NA,nrow=length(Ages_heavy_splines_values[,1]),ncol=2))
  colnames(heavily_smoothed_array_female) <- c("Age","values")
  heavily_smoothed_array_female$Age<-Ages_heavy_splines_values[,1]
  heavily_smoothed_array_female$values<-best_year_slope_intercept_female$best_slope[Ages_heavy_splines_values[,1]+1]
  
  #heavily smoothed for males
  heavily_smoothed_array_male=as.data.frame(matrix(data=NA,nrow=length(Ages_heavy_splines_values[,1]),ncol=2))
  colnames(heavily_smoothed_array_male) <- c("Age","values")
  heavily_smoothed_array_male$Age<-Ages_heavy_splines_values[,1]
  heavily_smoothed_array_male$values<-best_year_slope_intercept_male$best_slope[Ages_heavy_splines_values[,1]+1]
  
  heavily_smoothed_array_female$values <- spline_knot(Ages_heavy_splines_values[,1],
                                                      Ages_heavy_splines_values[,2],
                                                      best_year_slope_intercept_female[,c('Age','best_slope')],
                                                      2:2)[,2]
  
  heavily_smoothed_array_male$values <- spline_knot(Ages_heavy_splines_values[,1],
                                                    Ages_heavy_splines_values[,2],
                                                    best_year_slope_intercept_male[,c('Age','best_slope')],
                                                    2:2)[,2]
  
  best_year_slope_intercept_female$spline <- cubicspline(heavily_smoothed_array_female$Age,
                                                         heavily_smoothed_array_female$values,
                                                         best_year_slope_intercept_female$Age)
  
  
  
  best_year_slope_intercept_male$spline <- cubicspline(heavily_smoothed_array_male$Age,
                                                       heavily_smoothed_array_male$values,
                                                       best_year_slope_intercept_male$Age)
  
  
  best_year_slope_intercept_female$spline[best_year_slope_intercept_female$spline > 0] <- 0
  best_year_slope_intercept_male$spline[best_year_slope_intercept_male$spline > 0] <- 0
  
  #we need to ensure that the values of the slope are >= for higher age groups and >- for males than for females
  best_year_slope_intercept_female$spline2 <- best_year_slope_intercept_female$spline
  
  for (slope_adjustment_ages in 21:max_age_ExS_after_smoothing){
    if (best_year_slope_intercept_female$spline2[slope_adjustment_ages+1]<best_year_slope_intercept_female$spline2[slope_adjustment_ages]){
      (best_year_slope_intercept_female$spline2[slope_adjustment_ages+1] = best_year_slope_intercept_female$spline2[slope_adjustment_ages])
    }
  }
  
  best_year_slope_intercept_female$spline3 <- best_year_slope_intercept_female$spline
  
  
  #then go backwards
  for (slope_adjustment_ages in max_age_ExS_after_smoothing:16){
    if (best_year_slope_intercept_female$spline[slope_adjustment_ages] < best_year_slope_intercept_female$spline3[slope_adjustment_ages+1]){
      (best_year_slope_intercept_female$spline3[slope_adjustment_ages] = best_year_slope_intercept_female$spline[slope_adjustment_ages])
    }
    else{
      (best_year_slope_intercept_female$spline3[slope_adjustment_ages] = best_year_slope_intercept_female$spline3[slope_adjustment_ages+1])
    }
  }
  best_year_slope_intercept_female$spline_average=rowMeans(best_year_slope_intercept_female[,8:9])
  
  
  #MALES
  
  best_year_slope_intercept_male$spline2 <- best_year_slope_intercept_male$spline
  
  for (slope_adjustment_ages in 21:max_age_ExS_after_smoothing){
    if (best_year_slope_intercept_male$spline2[slope_adjustment_ages+1]<best_year_slope_intercept_male$spline2[slope_adjustment_ages]){
      (best_year_slope_intercept_male$spline2[slope_adjustment_ages+1] = best_year_slope_intercept_male$spline2[slope_adjustment_ages])
    }
  }
  
  best_year_slope_intercept_male$spline3 <- best_year_slope_intercept_male$spline
  
  #then go backwards
  for (slope_adjustment_ages in max_age_ExS_after_smoothing:16){
    if (best_year_slope_intercept_male$spline[slope_adjustment_ages] < best_year_slope_intercept_male$spline3[slope_adjustment_ages+1]){
      (best_year_slope_intercept_male$spline3[slope_adjustment_ages] = best_year_slope_intercept_male$spline[slope_adjustment_ages])
    }
    else{
      (best_year_slope_intercept_male$spline3[slope_adjustment_ages] = best_year_slope_intercept_male$spline3[slope_adjustment_ages+1])
    }
  }
  best_year_slope_intercept_male$spline_average=rowMeans(best_year_slope_intercept_male[,8:9])
  
  
  #
  
  spline_ages_2=user_defined_values_list$spline_values[,1]
  spline_2_smoothing_windows=user_defined_values_list$spline_values[,2]
  
  spline_round2_female <- (spline_knot(spline_ages_2,
                                       spline_2_smoothing_windows,
                                       best_year_slope_intercept_female[,c('Age','spline_average')],
                                       2:2)[,2])
  spline_round2_female[spline_round2_female > 0] <- 0
  
  spline_round2_male <- (spline_knot(spline_ages_2,
                                     spline_2_smoothing_windows,
                                     best_year_slope_intercept_male[,c('Age','spline_average')],
                                     2:2)[,2])
  spline_round2_male[spline_round2_male > 0] <- 0
  
  
  
  
  #long run slope
  
  #females
  best_year_slope_intercept_female$long_run_slope<-cubicspline(spline_ages_2,
                                                               spline_round2_female,
                                                               best_year_slope_intercept_female$Age)
  #males
  
  best_year_slope_intercept_male$long_run_slope<-cubicspline(spline_ages_2,
                                                             spline_round2_male,
                                                             best_year_slope_intercept_male$Age)
  
  
  projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female <- data.frame(matrix(NA,nrow=nrow(best_year_slope_intercept_female),ncol=length(projection_years)))
  colnames(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female)<-as.character(projection_years)
  projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male <- data.frame(matrix(NA,nrow=nrow(best_year_slope_intercept_male),ncol=length(projection_years)))
  colnames(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male)<-projection_years
  
  for (count_through_ages in 1:nrow(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female)){
    for (count_through_years in 1:length(projection_years)){

      #the unsmoothed ASDR is the intercept at age count_through_ages + slope at that age x year
      current_year<-as.numeric(colnames(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female)[count_through_years])
      print(current_year)
      projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female[count_through_ages,count_through_years] <-
        best_year_slope_intercept_female$best_intercept[count_through_ages] +
        (best_year_slope_intercept_female$best_slope[count_through_ages] * current_year)
      
      
    }
  }
  
  
  #MALES
  
  projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male <- data.frame(matrix(NA,nrow=nrow(best_year_slope_intercept_male),ncol=length(projection_years)))
  colnames(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male)<-as.character(projection_years)
  projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male <- data.frame(matrix(NA,nrow=nrow(best_year_slope_intercept_male),ncol=length(projection_years)))
  colnames(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male)<-projection_years
  
  for (count_through_ages in 1:nrow(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male)){
    for (count_through_years in 1:length(projection_years)){
      
      #the unsmoothed ASDR is the intercept at age count_through_ages + slope at that age x year
      current_year<-as.numeric(colnames(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male)[count_through_years])
      projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male[count_through_ages,count_through_years] <-
        best_year_slope_intercept_male$best_intercept[count_through_ages] +
        (best_year_slope_intercept_male$best_slope[count_through_ages] * current_year)
      
    }
  }
  
  Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female<-(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female)
  Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male<-(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male)
  
  for (count_rows in 1:nrow(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female)){
    for (count_cols in 1:ncol(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female)){
      Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female[count_rows,count_cols]<-
        exp(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female[count_rows,count_cols])
    }
    
  }
  
  
  for (count_rows in 1:nrow(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male)){
    for (count_cols in 1:ncol(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male)){
      Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male[count_rows,count_cols]<-
        exp(projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male[count_rows,count_cols])
    }
    
  }
  
  
  #create spline knots, line 572. Use first year of projection_of_ln_ASDR_fitted_model_without_any_adjustment_male
  spline_knot_ages_line573  <- user_defined_values_list$spline_values2[,1]
  smoothing_windows_line573 <- user_defined_values_list$spline_values2[,2]
  
  ###Creating the array found in line 572 of Female_ASDR_proj
  spline_knots_female<-as.data.frame(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female
                                     [spline_knot_ages_line573+1,1])
  spline_knots_female<-cbind(spline_knots_female,spline_knots_female)
  colnames(spline_knots_female)<-cbind("Age","values")
  
  spline_knots_female$Age<-spline_knot_ages_line573
  
  array1 <- as.data.frame(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female[,1])
  array1<-cbind(array1,array1)
  colnames(array1)<-c("Age","values")
  array1$Age<-as.numeric(rownames(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female))-1
  
  spline_knots_female$values <- (spline_knot(spline_knot_ages_line573,
                                             smoothing_windows_line573 ,
                                             array1,
                                             2:2, max_value=(user_defined_values_list$female_max_value))[,2])
  
  spline_knots_male<-as.data.frame(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male
                                   [spline_knot_ages_line573+1,1])
  spline_knots_male<-cbind(spline_knots_male,spline_knots_male)
  colnames(spline_knots_male)<-cbind("Age","values")
  
  spline_knots_male$Age<-spline_knot_ages_line573
  
  array1 <- as.data.frame(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male[,1])
  array1<-cbind(array1,array1)
  colnames(array1)<-c("Age","values")
  array1$Age<-as.numeric(rownames(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male))-1
  
  spline_knots_male$values <- (spline_knot(spline_knot_ages_line573,
                                           smoothing_windows_line573 ,
                                           array1,
                                           2:2, max_value=(user_defined_values_list$male_max_value))[,2])
  
  
  #####
  
  all_ages<-0:max_age_ExS_after_smoothing
  #Females
  smoothing_age_pattern_of_final_year_in_base_female<-as.data.frame(
    Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female[,1])
  colnames(smoothing_age_pattern_of_final_year_in_base_female)<-colnames(
    Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_female)[1]
  smoothing_age_pattern_of_final_year_in_base_female$Age<-all_ages
  
  smoothing_age_pattern_of_final_year_in_base_female[(4):(nrow(smoothing_age_pattern_of_final_year_in_base_female)),1]=
    cubicspline(spline_knots_female$Age[3:(length(spline_knots_female$Age))],
                as.numeric(spline_knots_female$values[3:(length(spline_knots_female$values))]),
                smoothing_age_pattern_of_final_year_in_base_female$Age[(4):(nrow(smoothing_age_pattern_of_final_year_in_base_female))])
  
  
  smoothing_age_pattern_of_final_year_in_base_female$colP=0
  smoothing_age_pattern_of_final_year_in_base_female$colP[2:nrow(smoothing_age_pattern_of_final_year_in_base_female)]<-
    smoothest_ASDR_females[2:nrow(smoothest_ASDR_females),(ncol(smoothest_ASDR_females)-2)]/
    smoothest_ASDR_females[1:(nrow(smoothest_ASDR_females)-1),(ncol(smoothest_ASDR_females)-2)]
  
  
   smoothing_age_pattern_of_final_year_in_base_female$colQ=smoothing_age_pattern_of_final_year_in_base_female[,1]
  
  for (counter_smoothing1 in 2:nrow(smoothing_age_pattern_of_final_year_in_base_female)){
    smoothing_age_pattern_of_final_year_in_base_female$colQ[counter_smoothing1] <- 
      smoothing_age_pattern_of_final_year_in_base_female$colQ[counter_smoothing1-1]*
      smoothing_age_pattern_of_final_year_in_base_female$colP[counter_smoothing1]
  }
  
  
  smoothing_age_pattern_of_final_year_in_base_female$colR=smoothing_age_pattern_of_final_year_in_base_female[,1]
  
  for (counter_smoothing1 in (nrow(smoothing_age_pattern_of_final_year_in_base_female)-1):1){
    smoothing_age_pattern_of_final_year_in_base_female$colR[counter_smoothing1] <- 
      smoothing_age_pattern_of_final_year_in_base_female$colR[counter_smoothing1+1]/
      smoothing_age_pattern_of_final_year_in_base_female$colP[counter_smoothing1+1]
  }
  
  
  
  smoothing_age_pattern_of_final_year_in_base_female$colS=0
  #for ages 0 to 14 S is the same as col 1
  smoothing_age_pattern_of_final_year_in_base_female$colS=smoothing_age_pattern_of_final_year_in_base_female[,1]
  
  #for ages 110 to 17
  max_age_ExS<-max(smoothing_age_pattern_of_final_year_in_base_female$Age)
  for (counter1 in (max_age_ExS+1):18){
    smoothing_age_pattern_of_final_year_in_base_female$colS[counter1] <- 
      smoothing_age_pattern_of_final_year_in_base_female$colR[counter1]*(smoothing_age_pattern_of_final_year_in_base_female$Age[counter1]/max_age_ExS)+
      smoothing_age_pattern_of_final_year_in_base_female$colQ[counter1]*((max_age_ExS-smoothing_age_pattern_of_final_year_in_base_female$Age[counter1])/max_age_ExS)
  }
  
  #for ages 16 and 15 (rows 17 and 16)
  count=17
  smoothing_age_pattern_of_final_year_in_base_female$colS[count] <- smoothing_age_pattern_of_final_year_in_base_female$colS[count-2]+
    2/3*(smoothing_age_pattern_of_final_year_in_base_female$colS[count+1]-smoothing_age_pattern_of_final_year_in_base_female$colS[count-2])
  
  count=16
  smoothing_age_pattern_of_final_year_in_base_female$colS[count] <- smoothing_age_pattern_of_final_year_in_base_female$colS[count-1]+
    1/3*(smoothing_age_pattern_of_final_year_in_base_female$colS[count+2]-smoothing_age_pattern_of_final_year_in_base_female$colS[count-1])
  
  ###
  
  #males
  smoothing_age_pattern_of_final_year_in_base_male<-as.data.frame(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male[,1])
  colnames(smoothing_age_pattern_of_final_year_in_base_male)<-colnames(Projection_A_ASDRs_from_basic_log_linear_extrapolation_without_adjustments_male)[1]
  smoothing_age_pattern_of_final_year_in_base_male$Age<-all_ages
  
  smoothing_age_pattern_of_final_year_in_base_male[(4):(nrow(smoothing_age_pattern_of_final_year_in_base_male)),1]=cubicspline(spline_knots_male$Age[3:(length(spline_knots_male$Age))],
                                                                                                                               as.numeric(spline_knots_male$values[3:(length(spline_knots_male$values))]),
                                                                                                                               smoothing_age_pattern_of_final_year_in_base_male$Age[(4):(nrow(smoothing_age_pattern_of_final_year_in_base_male))])
  
  
  smoothing_age_pattern_of_final_year_in_base_male$colP=0
  smoothing_age_pattern_of_final_year_in_base_male$colP[2:nrow(smoothing_age_pattern_of_final_year_in_base_male)]<-
    smoothest_ASDR_males[2:nrow(smoothest_ASDR_males),(ncol(smoothest_ASDR_males)-2)]/
    smoothest_ASDR_males[1:(nrow(smoothest_ASDR_males)-1),(ncol(smoothest_ASDR_males)-2)]
  
  
  
  #For col P, for ages 1 to 110, that is rows 2 to end row, the value at that age = 
  
  smoothing_age_pattern_of_final_year_in_base_male$colQ=smoothing_age_pattern_of_final_year_in_base_male[,1]
  
  for (counter_smoothing1 in 2:nrow(smoothing_age_pattern_of_final_year_in_base_male)){
    smoothing_age_pattern_of_final_year_in_base_male$colQ[counter_smoothing1] <- 
      smoothing_age_pattern_of_final_year_in_base_male$colQ[counter_smoothing1-1]*
      smoothing_age_pattern_of_final_year_in_base_male$colP[counter_smoothing1]
  }
  
  
  smoothing_age_pattern_of_final_year_in_base_male$colR=smoothing_age_pattern_of_final_year_in_base_male[,1]
  
  for (counter_smoothing1 in (nrow(smoothing_age_pattern_of_final_year_in_base_male)-1):1){
    smoothing_age_pattern_of_final_year_in_base_male$colR[counter_smoothing1] <- 
      smoothing_age_pattern_of_final_year_in_base_male$colR[counter_smoothing1+1]/
      smoothing_age_pattern_of_final_year_in_base_male$colP[counter_smoothing1+1]
  }
  
  
  
  smoothing_age_pattern_of_final_year_in_base_male$colS=0
  #for ages 0 to 14 S is the same as col 1
  smoothing_age_pattern_of_final_year_in_base_male$colS=smoothing_age_pattern_of_final_year_in_base_male[,1]
  
  #for ages 110 to 17
  max_age_ExS<-max(smoothing_age_pattern_of_final_year_in_base_male$Age)
  for (counter1 in (max_age_ExS+1):18){
    smoothing_age_pattern_of_final_year_in_base_male$colS[counter1] <- 
      smoothing_age_pattern_of_final_year_in_base_male$colR[counter1]*(smoothing_age_pattern_of_final_year_in_base_male$Age[counter1]/max_age_ExS)+
      smoothing_age_pattern_of_final_year_in_base_male$colQ[counter1]*((max_age_ExS-smoothing_age_pattern_of_final_year_in_base_male$Age[counter1])/max_age_ExS)
  }
  
  #for ages 16 and 15 (rows 17 and 16)
  count=17
  smoothing_age_pattern_of_final_year_in_base_male$colS[count] <- smoothing_age_pattern_of_final_year_in_base_male$colS[count-2]+
    2/3*(smoothing_age_pattern_of_final_year_in_base_male$colS[count+1]-smoothing_age_pattern_of_final_year_in_base_male$colS[count-2])
  
  count=16
  smoothing_age_pattern_of_final_year_in_base_male$colS[count] <- smoothing_age_pattern_of_final_year_in_base_male$colS[count-1]+
    1/3*(smoothing_age_pattern_of_final_year_in_base_male$colS[count+2]-smoothing_age_pattern_of_final_year_in_base_male$colS[count-1])
  
  ###
  
  
  ########## Next section ##########
  #Trending in of long-run smooth slopes
  
  
  
  #This is the fraction of the difference between the fitted and smoothed slope values
  #which must be added to the fitted slope value to trend in the smoothed slope values
  #over the required number of years
  
  
  #Female
  #we initiate it using an array used for projection just because we need the same column names
  fraction_difference_fitted_smoothed_female <- projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female[1,]
  fraction_difference_fitted_smoothed_female[1,]<-0
  
  
  #note that the projections will include the last year of the base period
  
  header_years<-as.numeric(colnames(fraction_difference_fitted_smoothed_female))
  fraction_difference_fitted_smoothed_female[1,] <- (header_years-max(base_period_for_proj))/trend_in_over_this_many_years
  fraction_difference_fitted_smoothed_female[fraction_difference_fitted_smoothed_female>1]<-1
  
  #initiate slope array
  Slope_female_line_717 <- projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_female
  
  Slope_female_line_717[,1] <- best_year_slope_intercept_female$best_slope
 
  for (count_year in 2:ncol(Slope_female_line_717)){
    for (count_age in 1:nrow(Slope_female_line_717)){
      Slope_female_line_717[count_age,count_year] <- Slope_female_line_717[count_age,1] + 
        fraction_difference_fitted_smoothed_female[1,count_year]*(
          best_year_slope_intercept_female$long_run_slope[count_age]-
            best_year_slope_intercept_female$best_slope[count_age]
        )
    }
  }
  

  #####
  #we initiate it using an array used for projection just because we need the same column names
  fraction_difference_fitted_smoothed_male <- projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male[1,]
  fraction_difference_fitted_smoothed_male[1,]<-0
  
 
  
  header_years<-as.numeric(colnames(fraction_difference_fitted_smoothed_male))
  fraction_difference_fitted_smoothed_male[1,] <- (header_years-max(base_period_for_proj))/trend_in_over_this_many_years
  fraction_difference_fitted_smoothed_male[fraction_difference_fitted_smoothed_male>1]<-1
  
  #initiate slope array
  Slope_male_line_717 <- projection_of_ln_ASDR_using_fitted_model_without_any_adjustment_male
  
  Slope_male_line_717[,1] <- best_year_slope_intercept_male$best_slope
  
  for (count_year in 2:ncol(Slope_male_line_717)){
    for (count_age in 1:nrow(Slope_male_line_717)){
      Slope_male_line_717[count_age,count_year] <- Slope_male_line_717[count_age,1] + 
        fraction_difference_fitted_smoothed_male[1,count_year]*(
          best_year_slope_intercept_male$long_run_slope[count_age]-
            best_year_slope_intercept_male$best_slope[count_age]
        )
    }
  }
  #####
  
  
  #Spline knots to the slope
  #####

  spline_knots_ages<-user_defined_values_list$spline_values[,1]
  spline_knots_smoothing_window<-user_defined_values_list$spline_values[,2]
  
  spline_knots_array_female <- spline_knot(spline_knots_ages,
                                           spline_knots_smoothing_window,
                                           Slope_female_line_717,
                                           2:ncol(Slope_female_line_717))
  
  #An "Age" column will be appended to the end. We want to remove it
  spline_knots_array_female<-spline_knots_array_female[,1:ncol(Slope_female_line_717)]    
  
  #Female
  #Apply spline to age pattern of slope values
  #so lets initate the entire array to that first
  Smooth_slope_female_line_855 <- Slope_female_line_717
  #apply cubic spline to get the other ages
  
  for (count1 in 2:ncol(Slope_female_line_717)){
    Smooth_slope_female_line_855[,count1]=cubicspline(spline_knots_ages,
                                                      as.numeric(spline_knots_array_female[,count1]),
                                                      0:max_age_ExS_after_smoothing)
  }
  
  
  #male
  #Apply spline to age pattern of slope values
  #so lets initate the entire array to that first
  spline_knots_array_male <- spline_knot(spline_knots_ages,
                                         spline_knots_smoothing_window,
                                         Slope_male_line_717,
                                         2:ncol(Slope_male_line_717))
  
  #An "Age" column will be appended to the end. We want to remove it
  spline_knots_array_male<-spline_knots_array_male[,1:ncol(Slope_male_line_717)]    
  
  
  Smooth_slope_male_line_855 <- Slope_male_line_717
  #apply cubic spline to get the other ages
  
  for (count1 in 2:ncol(Slope_male_line_717)){
    Smooth_slope_male_line_855[,count1]=cubicspline(spline_knots_ages,
                                                    as.numeric(spline_knots_array_male[,count1]),
                                                    0:110)
    
  }
  
  #####
  
  #Projection of ln(ASDRs) using fitted model with adjusted slope
  #line 967
  
  #Females
  #####

  #initialising 
  ln_smoothest_ASDR_female_line_968<-Smooth_slope_female_line_855
  ln_smoothest_ASDR_female_line_968[,1]<-
    log(smoothing_age_pattern_of_final_year_in_base_female$colS)
  
  for (counter1 in 2:ncol(ln_smoothest_ASDR_female_line_968)){
    ln_smoothest_ASDR_female_line_968[,counter1]<-
      ln_smoothest_ASDR_female_line_968[,counter1-1]+Smooth_slope_female_line_855[,counter1]
    
  }
  
  ln_smoothest_ASDR_female_line_968[nrow(ln_smoothest_ASDR_female_line_968),]<-
    ln_smoothest_ASDR_females[nrow(ln_smoothest_ASDR_females),ncol(ln_smoothest_ASDR_females)] #assume value of highest age stays constant
  
  
  #males
  #####

  #initialising 
  ln_smoothest_ASDR_male_line_968<-Smooth_slope_male_line_855
  ln_smoothest_ASDR_male_line_968[,1]<-
    log(smoothing_age_pattern_of_final_year_in_base_male$colS)
  
  for (counter1 in 2:ncol(ln_smoothest_ASDR_male_line_968)){
    ln_smoothest_ASDR_male_line_968[,counter1]<-
      ln_smoothest_ASDR_male_line_968[,counter1-1]+Smooth_slope_male_line_855[,counter1]
    
  }
  
  ln_smoothest_ASDR_male_line_968[nrow(ln_smoothest_ASDR_male_line_968),]<-
    ln_smoothest_ASDR_males[nrow(ln_smoothest_ASDR_males),ncol(ln_smoothest_ASDR_males)]
  
  #####
  
  #Projection B: ASDRs from log-linear extrapolation with adjusted slope
  
  ##### Females
  Project_B_females_line_1081 <- exp(ln_smoothest_ASDR_female_line_968)
  
  ##### Males
  Project_B_males_line_1081 <- exp(ln_smoothest_ASDR_male_line_968)
  
  #####
  
  #####
  
  #Projection c: 
  
  #####Females
  #Initiate as Projection B
  Project_C_female_line_1196 <- Project_B_females_line_1081
  # Project_C_female_line_1196[,1] <- Females_Smoothed_ASDRs_smoothed_over_age_and_time[,max(data_cols_for_projection)]
  
  Project_C_female_line_1196[,1] <- smoothest_ASDR_females[,max(data_cols_for_projection)]
  
  
  for (count_year in 2:ncol(Project_C_female_line_1196)){
    for (count_age in 1:nrow(Project_C_female_line_1196)){
      Project_C_female_line_1196[count_age,count_year]<-
        Project_B_females_line_1081[count_age,count_year]*
        (Project_C_female_line_1196[count_age,1]/
           Project_B_females_line_1081[count_age,1]
        )
    }
  }
  
  
  #####males
  #Initiate as Projection B
  Project_C_male_line_1196 <- Project_B_males_line_1081
  Project_C_male_line_1196[,1] <- smoothest_ASDR_males[,max(data_cols_for_projection)]
  
  for (count_year in 2:ncol(Project_C_male_line_1196)){
    for (count_age in 1:nrow(Project_C_male_line_1196)){
      Project_C_male_line_1196[count_age,count_year]<-
        Project_B_males_line_1081[count_age,count_year]*
        (Project_C_male_line_1196[count_age,1]/
           Project_B_males_line_1081[count_age,1]
        )
    }
  }
  
  
  
  #####
  #Projection D
  
  
  #initiate as first row of projection C
  
  #####Females
  smoothest_female_line_1310 <- Project_C_female_line_1196[1,]

  
  for (count1 in 1:ncol(smoothest_female_line_1310)){
    smoothest_female_line_1310[count1]<-
      (as.numeric(colnames(smoothest_female_line_1310)[count1])-
         min(as.numeric(colnames(smoothest_female_line_1310))))/trended_in_over_this_many_years
  }
  
  smoothest_female_line_1310[smoothest_female_line_1310>1]<-1
  
  #initiate D as C
  Project_D_females_line_1312 <- Project_C_female_line_1196 #first col is the same
  

  for (count_year in 2:ncol(Project_D_females_line_1312)){
    for (count_age in 1:nrow(Project_D_females_line_1312)){
      Project_D_females_line_1312[count_age,count_year]<-
        smoothest_female_line_1310[count_year]*
        Project_B_females_line_1081[count_age,count_year]+
        (1-smoothest_female_line_1310[count_year])*
        Project_C_female_line_1196[count_age,count_year]
    }
  }
  
  #####males
  smoothest_male_line_1310 <- Project_C_male_line_1196[1,]

  
  for (count1 in 1:ncol(smoothest_male_line_1310)){
    smoothest_male_line_1310[count1]<-
      (as.numeric(colnames(smoothest_male_line_1310)[count1])-
         min(as.numeric(colnames(smoothest_male_line_1310))))/trended_in_over_this_many_years
  }
  
  smoothest_male_line_1310[smoothest_male_line_1310>1]<-1
  
  #initiate D as C
  Project_D_males_line_1312 <- Project_C_male_line_1196 #first col is the same
  

  for (count_year in 2:ncol(Project_D_males_line_1312)){
    for (count_age in 1:nrow(Project_D_males_line_1312)){
      Project_D_males_line_1312[count_age,count_year]<-
        smoothest_male_line_1310[count_year]*
        Project_B_males_line_1081[count_age,count_year]+
        (1-smoothest_male_line_1310[count_year])*
        Project_C_male_line_1196[count_age,count_year]
    }
  }
  
  #Correct so that the ASDR for any particular year/age is greater or equal for males than females
  for (count_through_rows in 1:nrow(Project_D_males_line_1312)){
    for (count_through_cols in 2:ncol(Project_D_males_line_1312)){
      if (Project_D_males_line_1312[count_through_rows,count_through_cols] < Project_D_females_line_1312[count_through_rows,count_through_cols])
      {
        Project_D_males_line_1312[count_through_rows,count_through_cols] <- Project_D_females_line_1312[count_through_rows,count_through_cols]
      }
    }
  }
  
  
  #we put in the age and sex details so that we can feed the projected ASDRs into the lifetable function
  # projection_ASDRs_males<-cbind(Males_Smoothed_ASDRs_smoothed_over_age_and_time[,1:(data_cols_for_projection[1]-1)],Project_D_males_line_1312)
  
  projection_ASDRs_males<-cbind(smoothest_ASDR_males[,1:(data_cols_for_projection[1]-1)],Project_D_males_line_1312)
  
  
  
  projection_ASDRs_males$Variable<-"projected ASDRs Males"
  
  
  projection_ASDRs_females<-cbind(smoothest_ASDR_females[,1:(data_cols_for_projection[1]-1)],Project_D_females_line_1312)
  
  projection_ASDRs_females$Variable<-"projected ASDRs females"
  
  data_cols_in_projections<-data_cols_for_projection[1]:ncol(projection_ASDRs_females)
  
  #Life Table calculation
  out_lt_female<-life_table_function_ES(projection_ASDRs_females,data_cols_in_projections)
  
  out_lt_male<-life_table_function_ES(projection_ASDRs_males,data_cols_in_projections)
  
  combined_df_females <- do.call(rbind, lapply(out_lt_female, function(x) rbind(x, rep("", ncol(x)))))
  combined_df_males <- do.call(rbind, lapply(out_lt_male, function(x) rbind(x, rep("", ncol(x)))))
  
  if (user_defined_values_list$output_max_age < user_defined_values_list$forecast_max_age){
    projection_ASDRs_males<-change_age_output(projection_ASDRs_males,4:ncol(projection_ASDRs_males), user_defined_values_list$output_max_age)
    projection_ASDRs_females<-change_age_output(projection_ASDRs_females,4:ncol(projection_ASDRs_females), user_defined_values_list$output_max_age)
  }
  
  
  #let's check if the user wants the jump off year to be included in the forecast output
  
  if (user_defined_values_list$forecast_max_age==1){
    #include, do nothing
  }else{
    #don't include jump off year in the output
    col_index_jumpoff<-which(colnames(projection_ASDRs_females)==as.character(user_defined_values_list$jump_off))
    projection_ASDRs_females <- projection_ASDRs_females[,-col_index_jumpoff]
    projection_ASDRs_males <- projection_ASDRs_males[,-col_index_jumpoff]
  }
  
  #Let's write the projections to an excel workbook if the user has provided a filename
  
  if (!is.null(write_excel_projections)){
    
    wb <- createWorkbook()
    
    addWorksheet(wb, "Project Females")
    writeData(wb, "Project Females", projection_ASDRs_females)
    
    addWorksheet(wb, "Project Males")
    writeData(wb, "Project Males", projection_ASDRs_males)
    saveWorkbook(wb, write_excel_projections,overwrite = TRUE)
  }
  
  return(list(ProjectD_Females=projection_ASDRs_females,
              ProjectD_Males=projection_ASDRs_males
  ))
}


create_forecast_object<-function(
    base_pop=NULL,
    base_deaths=NULL,
    death_rates=NULL,
    first_data_col,
    jump_off_year,
    project_to_year,
    output_file_path = "output_file.xlsx",
    male_smoothing_type=2, 
    female_smoothing_type=2, 
    forecast_max_age_ExS=110,
    output_max_age_ExS=105, 
    set_max_values="YES", 
    female_max_value=0.8, 
    male_max_value=0.9,
    min_length_base_data=20, 
    trend_in_long_run_slopes=20, 
    trend_in_for_projection = 50,
    include_jump_off_year = 0,
    spline_values_1=NULL, 
    heavy_spline_values_1=NULL, 
    spline_values_2=NULL) {
  
  
  user_defined_values_list=list()
  
  
  if (is.null(death_rates) && (is.null(base_deaths)||is.null(base_pop))){
    stop("you need to provide age-specific death rates or base deaths and base population. You haven't provided either")
  }
  
  if (is.null(death_rates)){
    user_defined_values_list$deaths1<-base_deaths
    user_defined_values_list$populations1<-base_pop
    user_defined_values_list$death_rates1<- NULL
  }else{
    user_defined_values_list$deaths1<-NULL
    user_defined_values_list$populations1<-NULL
    user_defined_values_list$death_rates1<- death_rates
  }
  
  
  #create default values where new spline values are not provided
  if (is.null(spline_values_1)){
    spline_values_1 <- data.frame(
      X2 = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110),
      X3 = c(1, 3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,   3,   1,  1)
    )
  }
  
  if (is.null(heavy_spline_values_1)){
    heavy_spline_values_1 <- data.frame(
      x2= c(0,   5,  10,  20,  40,  60,  80,  90, 100, 110),
      x3= c(1,   5,  19,  19,  19,  19,   9,   9,   9,  0)
    )
  }
  
  if (is.null(spline_values_2)){
    spline_values_2 <- data.frame(
      x2 = c(0,   1,   3,   6,   9,  12,  15,  18,  21,  25,  30,  35,  40,  45,  50,  55,  60,  65,  70,  75,  80,  85,  90,  95, 100, 110),
      x3 = c(1,   1,   1,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   3,   3,   3,   3,   3,   3,   3,   3,  -1)
    )
  }
  
  
  user_defined_values_list$output_file_path <- output_file_path
  user_defined_values_list$first_data_col <- first_data_col
  user_defined_values_list$jump_off <- jump_off_year
  user_defined_values_list$project_to <- project_to_year
  user_defined_values_list$male_smoothing_type <- male_smoothing_type
  user_defined_values_list$female_smoothing_type <- female_smoothing_type
  user_defined_values_list$forecast_max_age <- forecast_max_age_ExS
  user_defined_values_list$output_max_age <- output_max_age_ExS
  user_defined_values_list$set_max_values <- set_max_values
  user_defined_values_list$female_max_value <- female_max_value
  user_defined_values_list$male_max_value <- male_max_value
  user_defined_values_list$min_length_base_data <- min_length_base_data
  user_defined_values_list$trend_in_long_run_slopes <- trend_in_long_run_slopes
  user_defined_values_list$trend_in_for_projection <- trend_in_for_projection
  user_defined_values_list$spline_values <- spline_values_1
  user_defined_values_list$heavy_spline_values <- heavy_spline_values_1
  user_defined_values_list$spline_values2 <- spline_values_2
  
  user_defined_values_list$include_jump_off_year<-include_jump_off_year
  
  return(user_defined_values_list)
}

# Example code
# # 
# datacol1 <- 4
# jump_off_year <- 1990
# project_to_this_year <- 2020
# max_age_ExS = 105
# country_label1<-"AUS"
# 
# #data from HMD
# 
# Country_Data <- hmd.mx(
#   country = country_label1,
#   username = "your user name",
#   password = "your password"
# )
# 
# #as data is sparse we combine upper ages.
# Country_Data <- extract.ages(
#   Country_Data,
#   0:max_age_ExS,
#   combine.upper = TRUE
# )
# first_year1<-min(Country_Data$year)
# year1=1990
# year2=2020
# #let's subset the hmd data that is in the base period
# HMD_data_BASE <- extract.years(
#   Country_Data,
#   first_year1:year1
# )
# 
# 
# #lets take the male and female death rates, and for each
# # 1 create an "Age" column
# # 2 create a "Sex" column
# # Creates a Variable column
# #
# #put data into the correct structure for the Extrapolative smoothing code
# death_rates_males<-as.data.frame(HMD_data_BASE$rate$male)
# death_rates_females<-as.data.frame(HMD_data_BASE$rate$female)
# 
# descriptive_cols <- death_rates_males[,1:3]
# colnames(descriptive_cols) <- c("Variable", "Sex", "Age")
# descriptive_cols$Age <- 0:(nrow(death_rates_males)-1)
# descriptive_cols$Variable<-"death rates"
# 
# death_rates_males <- cbind(descriptive_cols,death_rates_males)
# death_rates_males$Sex <- "Males"
# 
# death_rates_females <- cbind(descriptive_cols,death_rates_females)
# death_rates_females$Sex <- "Females"
# 
# death_rates_combined <- rbind(death_rates_females,death_rates_males)
# rownames(death_rates_combined) <- 1:nrow(death_rates_combined)
# 
# forecast_object <-create_forecast_object(
#   base_pop=NULL,
#   base_deaths=NULL,
#   death_rates=death_rates_combined,
#   4,
#   year1,
#   year2)
# 
# HMD_projections <- create_projections(forecast_object,
#                                       is_evaluation = 1)
