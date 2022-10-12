'''
Google Capstone Project: Bellabeat
Author: Sandra Owhor
Date: 19th August, 2022
Email: owhorsandra@gmail.com

'''

# Upload CSV files to R and Load common libraries
library(tidyverse)

# Load CSV files and create dataframes
# Daily activity dataframe
daily_activity <- read.csv("FitBit_data/dailyActivity_merged.csv")

# Daily Calories dataframe
daily_calories <- read.csv("FitBit_data/dailyCalories_merged.csv")

# Sleep day dataframe
sleep_day <- read.csv("FitBit_data/sleepDay_merged.csv")

# Weight log Info dataframe
weight_log <- read.csv("FitBit_data/weightLogInfo_merged.csv")

# Exploring dataframes and identifying columns, also using
# the str() to see the structure of the dataframes and 
# decide which to manipulate

head(daily_activity)
colnames(daily_activity)
str(daily_activity)

head(daily_calories)
colnames(daily_calories)
str(daily_calories)

head(sleep_day)
colnames(sleep_day)
str(sleep_day)

head(weight_log)
colnames(weight_log)
str(weight_log)


# Data Cleaning

# Load libraries
library(lubridate)

# Correcting faulty data types and confirming changed data types relevant 
# in the 'Id' and "Date' columns...these are relevant for this analysis

daily_activity$New_Id <- as.character(daily_activity$Id)
daily_activity$New_date <- mdy(daily_activity$ActivityDate)
str(daily_activity)

daily_calories$New_Id <- as.character(daily_calories$Id)
daily_calories$New_date <- mdy(daily_calories$ActivityDay)
str(daily_calories)

sleep_day$New_Id <- as.character(sleep_day$Id)
sleep_day$New_date <- mdy_hms(sleep_day$SleepDay)
str(sleep_day)

weight_log$New_Id <- as.character(weight_log$Id)
weight_log$New_date <- mdy_hms(weight_log$Date)
str(weight_log)

# Removing columns that are not so relevant for this analyis
# and creating new dataframes...removing missing values if any

new_daily_act <- daily_activity %>%
                 select(-Id, -ActivityDate, -TrackerDistance, -
                          LoggedActivitiesDistance, -
                          ModeratelyActiveDistance, -
                          FairlyActiveMinutes, -Calories) %>%
                 drop_na(TotalSteps,
                         TotalDistance,
                         VeryActiveDistance,
                         LightActiveDistance,
                         SedentaryActiveDistance,
                         VeryActiveMinutes,
                         LightlyActiveMinutes,
                         SedentaryMinutes,
                         New_Id, New_date)
head(new_daily_act)

new_calories <- daily_calories %>%
                select(-Id, -ActivityDay) %>%
                drop_na(Calories, New_Id, New_date)
head(new_calories)
                
new_sleep_day <- sleep_day %>%
                 select(-Id, -SleepDay) %>%
                 drop_na(TotalSleepRecords,
                         TotalMinutesAsleep, 
                         TotalTimeInBed,
                         New_Id, New_date)
head(new_sleep_day)

new_weight_log <- weight_log %>% 
                  select(-Id, -Date, -WeightPounds, -
                           Fat, -BMI, -IsManualReport, -
                           LogId) %>% 
                  drop_na(WeightKg, New_Id, New_date)
head(new_weight_log)
                  
# Identifying unique participants in each dataframe
n_distinct(new_daily_act$New_Id)
n_distinct(new_sleep_day$New_Id)
n_distinct(new_calories$New_Id)
n_distinct(new_weight_log$New_Id)

# Quick summary statstics of some of the data frames
new_daily_act %>%
  select(TotalSteps, TotalDistance,
         SedentaryActiveDistance) %>%
  summary()

new_calories %>% 
  select(Calories) %>%
  summary()

new_sleep_day %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

new_weight_log %>%
  select(WeightKg) %>%
  summary()

# Plotting some explorations to see how participants used the fitness app

# Loading libraries
library(ggplot2)

# Finding relationships in each dataframe

ggplot(data = new_daily_act) +
   geom_point(mapping = aes(x = SedentaryMinutes, y = TotalSteps,
                            color = New_date))

ggplot(data = new_daily_act) +
   geom_point(mapping = aes(x = VeryActiveMinutes,
                            y = TotalDistance,
                            color = n_distinct(New_Id)))

# Combining dataframes
# Reposition 'New_Id' column in each dataframe to the first postion
# just for simplicity and readability
New_daily_act <- new_daily_act %>%
                dplyr::select("New_Id", everything())

New_calories <- new_calories %>%
                dplyr::select("New_Id", everything())

New_sleep_day <- new_sleep_day %>%
                 dplyr::select("New_Id", everything())

New_weight_log <- new_weight_log %>%
                  dplyr::select("New_Id", everything())

FitBit_data_all <- bind_rows( New_daily_act, New_calories, 
                             New_sleep_day, New_weight_log, 
                             )

# See how many people used the app features
n_distinct(FitBit_data_all$New_Id)

# Replacing 'NA' with '0' in the combined dataframe

FitBit_data_all[is.na(FitBit_data_all)] = 0

# Saving and naming the new 'FitBit_data_all' dataframe in
# a different working directory as a CSV file format
# Export file without rows by setting the 'row.names=FALSE'

write.csv(FitBit_data_all, file = 
        "C:/Users/USER/Desktop/FitBit_Fitness_Tracker_Data/New_FitBit_data.csv", 
            row.names = F)

# File exported as CSV file format to tableau for further analysis
# and visualization





