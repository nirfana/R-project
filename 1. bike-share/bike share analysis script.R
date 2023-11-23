### Cyclistic_Exercise_Full_Year_Analysis ###

# This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).
# It’s originally based on the case study
# "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman
# (found here: https://artscience.blog/home/divvy-dataviz-case-study).
# We will be using the Divvy dataset for the case study.
# The purpose of this script is to consolidate downloaded Divvy data into a single dataframe
# and then conduct simple analysis to help answer the key question:
# “In what ways do members and casual riders use Divvy bikes differently?”

# importing necesary packages
library(tidyverse)
library(ggplot2)
library(lubridate)

# 1. Import The Data
setwd("~/Documents/GitHub/R-project")

q1_2019 <- read_csv("./data/Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("./data/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("./data/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("./data/Divvy_Trips_2019_Q4.csv")

# 2. Combining data into a single file 
# i have to see the column names of each dataset to match the data 
colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)


# aparently there is a inconsistency of the coulumn names especially in the q2 2019 data
# renaming the columns to be consistance, the column names will be renamed to be same as the q1 2020 data
q1_2019 <- rename(q1_2019,
                  ride_id = trip_id,
                  bike_id = bikeid,
                  started_at = start_time,  
                  ended_at = end_time,
                  start_station_name = from_station_name, 
                  start_station_id = from_station_id,
                  end_station_name = to_station_name, 
                  end_station_id = to_station_id,
                  member_casual = usertype)

q2_2019 <- rename(q2_2019,
                  ride_id = "01 - Rental Details Rental ID",
                  bike_id = "01 - Rental Details Bike ID", 
                  started_at = "01 - Rental Details Local Start Time",  
                  ended_at = "01 - Rental Details Local End Time",  
                  start_station_name = "03 - Rental Start Station Name", 
                  start_station_id = "03 - Rental Start Station ID",
                  end_station_name = "02 - Rental End Station Name", 
                  end_station_id = "02 - Rental End Station ID",
                  member_casual = "User Type")

q3_2019 <- rename(q3_2019,
                  ride_id = trip_id,
                  bike_id = bikeid, 
                  started_at = start_time,  
                  ended_at = end_time,  
                  start_station_name = from_station_name, 
                  start_station_id = from_station_id, 
                  end_station_name = to_station_name, 
                  end_station_id = to_station_id, 
                  member_casual = usertype)

q4_2019 <- rename(q4_2019,
                  ride_id = trip_id,
                  bike_id = bikeid, 
                  started_at = start_time,  
                  ended_at = end_time,  
                  start_station_name = from_station_name, 
                  start_station_id = from_station_id, 
                  end_station_name = to_station_name, 
                  end_station_id = to_station_id, 
                  member_casual = usertype)


# Inspect the dataframes and look for inconguencies
str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id),
                   bike_id = as.character(bike_id)) 

q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id),
                   bike_id = as.character(bike_id)) 

q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id),
                   bike_id = as.character(bike_id)) 

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id),
                   bike_id = as.character(bike_id)) 

# combine into one big data frame
all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019)

# The birthyear will be removed from the dataset. 
# These fields are no longer relevant to the current analysis, 
# beacause bike-sharing is a relatively new technology. 
# so it is not likely that there would be a significant difference in usage patterns between different age groups.
all_trips <- all_trips %>%  
  select(c(ride_id, 
           started_at, 
           ended_at, 
           bike_id, 
           start_station_name, 
           start_station_id, 
           end_station_name, 
           end_station_id, 
           member_casual))

# 3. Clean Up and Prepare data for analysis 
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
# now the datasets only having 9 columns 

# seeing the dimension of the dataset
dim(all_trips) 

head(all_trips)  # See the first 6 rows of data frame. 
tail(all_trips) # See the last 6 rows of data frame.

str(all_trips)  #See list of columns and data types (numeric, character, etc)

summary(all_trips)  #Statistical summary of data. Mainly for numerics


# The data can only be aggregated at the ride-level, which is too granular. 
# We will want to add some additional columns of data -- such as day, month, year -- 
# that provide additional opportunities to aggregate the data.
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# We will want to add a calculated field for length of ride 
# We will add "ride_length" to the entire dataframe for consistency.
# the results will be in seconds
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")

# Since in this project we call use as "member" and "casual"
# we will renaming the value inside casual_member column
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "Member"
                                ,"Customer" = "Casual"))

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


summary(all_trips$ride_length)
# There are some rides where tripduration shows up as negative, 
# including several hundred rides where the company took bikes out of circulation for Quality Control reasons. 
# We will want to delete these rides.

# We will create a new version of the dataframe (v2) since data is being removed
all_trips_clean <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
# the code means that the new dataset will not contain rows with ride length less than 0 or negative value 
# also without rows where the start station of bike ride is HQ QR

# Compare members and casual users
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = mean)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = median)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = max)
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual, FUN = min)

# rearrange days so it will be in the right order
all_trips_clean$day_of_week <- ordered(all_trips_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_clean$ride_length ~ all_trips_clean$member_casual + all_trips_clean$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 	# calculates the average duration
  arrange(member_casual, weekday)	# sorts the results

# Let's visualize the number of rides by rider type
all_trips_clean %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(total_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = total_rides, fill = member_casual)) + 
  geom_text(
    aes(label = scales::comma(total_rides)),
    position = position_dodge(width = 0.9),  
    vjust = -0.5,  
    size = 1.8) +       
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(
    values = c("Casual" = "#f1a340", "Member" = "#998ec3"),
    breaks = c("Casual", "Member"),  # order of legend values
    labels = c("Casual Riders", "Annual Members")  # new legend value name
  ) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Casual Rider Vs. Annual Member",
    subtitle = "Ridership data comparison by member type on week days ",
    x = "Day of the Week",  # Change x-axis label
    y = "Total Rides",     # Change y-axis label
    fill = "Member Type")   # Change legend title

# Let's create a visualization for average duration
all_trips_clean %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(total_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  scale_fill_manual(
    values = c("Casual" = "#f1a340", "Member" = "#998ec3"),
    breaks = c("Casual", "Member"),  # order of legend values
    labels = c("Casual Riders", "Annual Members")  # new legend value name
  ) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Casual Rider Vs. Annual Member",
    subtitle = "Ridership data comparison by member type and average ride duration on week days ",
    x = "Day of the Week",  # Change x-axis label
    y = "Average Duration (Min)",     # Change y-axis label
    fill = "Member Type")   # Change legend title


