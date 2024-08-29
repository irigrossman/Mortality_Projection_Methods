# Set working directory
setwd('C:/programming/Mortality Packages/Life_table_code')

# Load required libraries
library(StMoMo)
library(demography)
library(openxlsx)
library(ggplot2)

# Data parameters
datacol1 <- 4

jump_off_year <- 2020
project_to_this_year <- 2050
max_age <- 105
country_label1 <- "AUS"
projection_years<-(jump_off_year+1):project_to_this_year
first_year_base_period<-1950

# Load HMD data
Country_Data <- readRDS(file = "AUS_HMD_data.rds")

Country_Data <- extract.ages(
  Country_Data,
  0:max_age,
  combine.upper = TRUE
)

#subset data to required base period years
#let's subset the hmd data that is in the base period
HMD_data_BASE <- extract.years(
  Country_Data,
  first_year_base_period:jump_off_year
)

# Define Lee-Carter model
LC <- lc()

# Prepare data for StMoMo
CountryData_male <- StMoMoData(HMD_data_BASE, series = "male")
CountryData_female <- StMoMoData(HMD_data_BASE, series = "female")

# Fit the Lee-Carter model
LCfit_male <- fit(LC, data = CountryData_male)
LCfit_female <- fit(LC, data = CountryData_female)

# Forecast the rates
forecast_horizon <- project_to_this_year - jump_off_year
LCfor_male <- forecast(LCfit_male, h = forecast_horizon)
LCfor_female <- forecast(LCfit_female, h = forecast_horizon)

male_projections = LCfor_male$rates
female_projections = LCfor_female$rates

life_table_function<-function(projections,ages,file_name=NULL){
  data_cols_original<-1:ncol(projections)
  ASDRs<-cbind(as.data.frame(ages),as.data.frame(ages),as.data.frame(ages),as.data.frame(projections))
  colnames(ASDRs)[1]<-"Variable"
  colnames(ASDRs)[2]<-"Sex"
  colnames(ASDRs)[3]<-"Age"
  data_cols<-data_cols_original+3 #to account for the additional columns with variables
  start_data_col<-min(data_cols)
  nmx <- as.data.frame(ASDRs)
  #step 1 create lifetable using the demography package
  demogdata_lt <- demogdata(
    data = ASDRs[,data_cols],
    ages = ASDRs$Age,
    pop = ASDRs[,data_cols], #input not used for lifetables.
    years = 1:length(data_cols),
    type = "mortality",
    label = "label",
    name = "name"
  )
  
  
  
  max_age<-max(ages)
  
  lt1 <- lifetable(demogdata_lt,max.age=max_age)
  
  info_cols_to_add<-ASDRs[,1:(min(data_cols)-1)]
  
  colnames(lt1$mx)<-colnames(projections)
  mx = cbind(info_cols_to_add,lt1$mx)
  mx$Variable<-"mx"
  
  colnames(lt1$qx)<-colnames(projections)
  qx = cbind(info_cols_to_add,lt1$qx)
  qx$Variable<-"qx"
  
  colnames(lt1$lx)<-colnames(projections)
  lx = cbind(info_cols_to_add,lt1$lx)
  lx$Variable<-"lx"
  
  colnames(lt1$dx)<-colnames(projections)
  dx = cbind(info_cols_to_add,lt1$dx)
  dx$Variable<-"dx"
  
  colnames(lt1$Lx)<-colnames(projections)
  Lx = cbind(info_cols_to_add,lt1$Lx)
  Lx$Variable<-"Lx"
  
  colnames(lt1$Tx)<-colnames(projections)
  Tx = cbind(info_cols_to_add,lt1$Tx)
  Tx$Variable<-"Tx"
  
  colnames(lt1$ex)<-colnames(projections)
  ex = cbind(info_cols_to_add,lt1$ex)
  ex$Variable<-"ex"
  
  colnames(lt1$rx)<-colnames(projections)
  rx = cbind(info_cols_to_add,lt1$rx)
  rx$Variable<-"rx"
  
  
  #if a filename has been provided let's write to excel
  if (!is.null(file_name)){
    
    
    data_list1 <-list(nmx = mx, nqx = qx, ndx = dx, nlx = lx, Lx = Lx, Tx=Tx, ex = ex, rx=rx)
    combined_full_life_table <- do.call(rbind, lapply(data_list1, function(x) rbind(x, rep("", ncol(x)))))
    
    # Convert specified columns to numeric
    combined_full_life_table[, data_cols] <- sapply(combined_full_life_table[, data_cols], as.numeric)
    
    
    # Create workbook and add sheets with dataframes
    wb <- createWorkbook()
    addWorksheet(wb, "life table")
    writeData(wb, "life table", combined_full_life_table)
    
    
    saveWorkbook(wb, file_name, overwrite = TRUE)
    
  }
  
  
  
  return(list(nmx = mx, nqx = qx, ndx = dx, nlx = lx, Lx = Lx, Tx=Tx, ex = ex, rx=rx))
  
}



# Generate life tables and save to Excel
ages <- CountryData_male$ages
male_lt_lists<-life_table_function(male_projections, ages)
ages <- CountryData_female$ages
female_lt_lists<-life_table_function(female_projections, ages)


# male_lt and female_lt are lists. Lets combine lists and convert data to numeric, 
# so that we can then save the lifetables
combine_life_table <- function(lt_list, sex1,years1) {
  combined <- do.call(rbind, lapply(lt_list, function(x) rbind(x, rep("", ncol(x)))))
  data_cols <- 4:ncol(combined)  # Assuming the first 3 columns are not numeric (Variable, Sex, Age)
  combined[, data_cols] <- sapply(combined[, data_cols], as.numeric)
  combined$Sex<-sex1
  colnames(combined)[4:(3+length(years1))]<-years1
  return(combined)
}

male_lt <- combine_life_table(male_lt_lists,'male',projection_years)
female_lt <- combine_life_table(female_lt_lists,'female',projection_years)

# Save to Excel workbook
wb <- createWorkbook()
addWorksheet(wb, "male_lt")
addWorksheet(wb, "female_lt")
addWorksheet(wb, "male_proj")
addWorksheet(wb, "female_proj")
writeData(wb, "male_lt", male_lt)
writeData(wb, "female_lt", female_lt)
writeData(wb, "male_proj", LCfor_male$rates)
writeData(wb, "female_proj", LCfor_female$rates)
saveWorkbook(wb, "life_tables_and_projections.xlsx", overwrite = TRUE)

# Function to plot combined life expectancy (ex) for males and females and save as JPG
plot_and_save_combined_life_expectancy <- function(male_lt_data, female_lt_data, chosen_year,base_period) {
  # Find the correct column index for the chosen year
  male_year_index <- which(colnames(male_lt_data) == as.character(chosen_year))
  female_year_index <- which(colnames(female_lt_data) == as.character(chosen_year))
  
  # Extract the data for the chosen year
  plot_data_male <- data.frame(Age = male_lt_data$Age, ex = male_lt_data[, male_year_index], Sex = "Male")
  plot_data_female <- data.frame(Age = female_lt_data$Age, ex = female_lt_data[, female_year_index], Sex = "Female")
  
  # Combine male and female data into one data frame
  plot_data <- rbind(plot_data_male, plot_data_female)
  
  # Plot using ggplot2
  plot <- ggplot(plot_data, aes(x = Age, y = ex, color = Sex)) +
    geom_line(size = 1) +
    labs(
      title = paste("Projected Life Expectancy (ex) for Year", chosen_year, "\n Base Period Data:", min(base_period),"to",max(base_period)),
      x = "Age",
      y = "Life Expectancy (ex)"
    ) +
    scale_color_manual(values = c("Male" = "blue", "Female" = "lightblue")) +
    scale_y_continuous(breaks = seq(0, 120, by = 10)) +
    theme_minimal()
  
  # Save the plot as a JPG file
  filename <- paste0("Combined_Life_Expectancy_", chosen_year, ".jpg")
  ggsave(filename, plot, width = 8, height = 6)
  
  # Return the plot object for display if needed
  return(plot)
}

# Chosen year to plot
chosen_year <- project_to_this_year

# Plot and save the combined life expectancy for males and females
plot_combined_ex <- plot_and_save_combined_life_expectancy(male_lt_lists$ex, female_lt_lists$ex, chosen_year,first_year_base_period:jump_off_year)
print(plot_combined_ex)
