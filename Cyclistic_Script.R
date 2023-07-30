### Cyclistic 12 Month Analysis ###
### July 2022 through June 2023 ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study).

#========================================================
# Step 0: Install the packages we need and load libraries
#========================================================
# tidyverse for data import and cleanup
# lubridate for date and time functions
# ggplot for visualizations
# dplyr for data manipulation

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot")
install.packages("dplyr")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

getwd() #Display the working directory
setwd("D:/OneDrive/Documents/Capstone/CSV") #Set working directory to location of CSV data files on your machine
getwd() #Make sure that working directory is set correctly

#===============================
# STEP 1: Upload Data to RStudio
#===============================
# Upload datasets for the most recent 12 months of data (csv files)
m07_2022 <- read_csv("202207-divvy-tripdata.csv")
m08_2022 <- read_csv("202208-divvy-tripdata.csv")
m09_2022 <- read_csv("202209-divvy-tripdata.csv")
m10_2022 <- read_csv("202210-divvy-tripdata.csv")
m11_2022 <- read_csv("202211-divvy-tripdata.csv")
m12_2022 <- read_csv("202212-divvy-tripdata.csv")
m01_2023 <- read_csv("202301-divvy-tripdata.csv")
m02_2023 <- read_csv("202302-divvy-tripdata.csv")
m03_2023 <- read_csv("202303-divvy-tripdata.csv")
m04_2023 <- read_csv("202304-divvy-tripdata.csv")
m05_2023 <- read_csv("202305-divvy-tripdata.csv")
m06_2023 <- read_csv("202306-divvy-tripdata.csv")

#====================================================
# STEP 2: Examine Data and Combine Into a Single File
#====================================================
# Make sure the column names are the same in every file
# Column names should be identical in all files (so that we can join them into one file)
# Column names do not need to be in the same order in every file

colnames(m07_2022)
colnames(m08_2022)
colnames(m09_2022)
colnames(m10_2022)
colnames(m11_2022)
colnames(m12_2022)
colnames(m01_2023)
colnames(m02_2023)
colnames(m03_2023)
colnames(m04_2023)
colnames(m05_2023)
colnames(m06_2023)

# The column names are consistent in all 12 files
# None need to be renamed (love it!)

# Review dataframes for inconsistencies
str(m07_2022)
str(m08_2022)
str(m09_2022)
str(m10_2022)
str(m11_2022)
str(m12_2022)
str(m01_2023)
str(m02_2023)
str(m03_2023)
str(m04_2023)
str(m05_2023)
str(m06_2023)

# Combine files for 12 individual months into a single file
all_trips <- bind_rows(m07_2022, m08_2022, m09_2022, m10_2022, m11_2022, m12_2022, m01_2023, m02_2023, m03_2023, m04_2023, m05_2023, m06_2023)

# Remove latitude and longitude
# We don't need them
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$hour <- format(as.POSIXct(all_trips$started_at), "%H")

# Add a "ride_length" calculation to all_trips (in minutes)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = c("mins"))

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#create column for different seasons: Spring, Summer, Fall, Winter
all_trips <- all_trips %>% mutate(season = 
                                    case_when(month == "03" ~ "Spring",
                                              month == "04" ~ "Spring",
                                              month == "05" ~ "Spring",
                                              month == "06"  ~ "Summer",
                                              month == "07"  ~ "Summer",
                                              month == "08"  ~ "Summer",
                                              month == "09" ~ "Fall",
                                              month == "10" ~ "Fall",
                                              month == "11" ~ "Fall",
                                              month == "12" ~ "Winter",
                                              month == "01" ~ "Winter",
                                              month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
all_trips <- all_trips %>% mutate(time_of_day = 
                                 case_when(hour == "0" ~ "Night",
                                           hour == "1" ~ "Night",
                                           hour == "2" ~ "Night",
                                           hour == "3" ~ "Night",
                                           hour == "4" ~ "Night",
                                           hour == "5" ~ "Night",
                                           hour == "6" ~ "Morning",
                                           hour == "7" ~ "Morning",
                                           hour == "8" ~ "Morning",
                                           hour == "9" ~ "Morning",
                                           hour == "10" ~ "Morning",
                                           hour == "11" ~ "Morning",
                                           hour == "12" ~ "Afternoon",
                                           hour == "13" ~ "Afternoon",
                                           hour == "14" ~ "Afternoon",
                                           hour == "15" ~ "Afternoon",
                                           hour == "16" ~ "Afternoon",
                                           hour == "17" ~ "Afternoon",
                                           hour == "18" ~ "Evening",
                                           hour == "19" ~ "Evening",
                                           hour == "20" ~ "Evening",
                                           hour == "21" ~ "Evening",
                                           hour == "22" ~ "Evening",
                                           hour == "23" ~ "Evening")
)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
# Delete any trips less than 10 minutes or more than one day
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<10 | all_trips$ride_length>1440),]
# Remove NA values from ride length and create new version of dataframe (v3)
all_trips_v3 = na.omit(all_trips_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (in minutes)
mean(all_trips_v3$ride_length) #straight average (total ride length / rides)
median(all_trips_v3$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v3$ride_length) #longest ride
min(all_trips_v3$ride_length) #shortest ride

# Find and remove any duplicate rows
distinct(all_trips_v3)

# Take a moment to inspect the data we have now
str(all_trips_v3)
summary(all_trips_v3$ride_length)

# Compare members and casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)

# Get average ride time per day for members and casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

# Put days of the week in order
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Get average ride time per day for members and casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Create visualizations to verify that we have what we want before saving output and creating final visualizations in Tableau, etc.

# Visualize the number of rides by rider type
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
  ggsave(file="D:/OneDrive/Documents/Capstone/Rides by Weekday.png")

# Visualize average ride duration
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
  ggsave(file="D:/OneDrive/Documents/Capstone/Ride Duration by Weekday.png")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file to use with Excel, Tableau, etc.
# Read more about exporting data here: https://datatofish.com/export-dataframe-to-csv-in-r/
write.csv(all_trips_v3, file='D:/OneDrive/Documents/Capstone/CSV/all_trips_v3.csv')

# Next steps
# Create visualizations and dashboard in Tableau
# Write internal report of process and preliminary results
# Create final report (presentation) for client