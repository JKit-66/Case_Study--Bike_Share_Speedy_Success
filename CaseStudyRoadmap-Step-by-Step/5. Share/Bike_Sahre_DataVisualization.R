install.packages("lubridate")
install.packages("chron")

library(lubridate)
library(hms)
library(chron)

bike_shre <- read.csv("C:/Users/J Kit/Documents/GitHub/Case_Study--Bike_Share_Speedy_Success/csv File/Final .csv File/Final_Divvy_Trips_2020_Q1.csv")


View(bike_shre)

is_numeric <- is.numeric(bike_shre$end_station_id)
print(is_numeric)

#bike_shre$ride_length_seconds <- as.numeric(hms::hms(bike_shre$ride_length))
mean_ride_length <- mean(times(bike_shre$ride_length))
print(mean_ride_length)

bike_shre %>%
  arrange(desc(ride_length_DHMS))


ggplot(data=bike_shre) + 
  geom_bar(mapping=aes(x=day_of_week))


get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

mode_value <- get_mode(bike_shre$month_of_ride)
print(mode_value)

library(dplyr)

bike_shre$ride_length_sec <- as.numeric(as.POSIXct(bike_shre$ride_length, format="%H:%M:%S")) - as.numeric(as.POSIXct("00:00:00", format="%H:%M:%S"))
average_seconds <- mean(bike_shre$ride_length_sec, na.rm = TRUE)
print(average_seconds)



average_ride_length <- bike_shre %>%
  group_by(day_of_week) %>%
  summarize(average_ride_length = mean(ride_length_sec, na.rm = TRUE))

print(average_ride_length)


ride_counts <- bike_shre %>%
  group_by(day_of_week) %>%
  summarise(num_rides = n())  

print(ride_counts)


ggplot(data=ride_counts) + 
  geom_bar(mapping=aes(x=day_of_week, y=num_rides, fill = day_of_week), stat = "identity") +
  labs(title="Total Number of Rides", subtitle = "Data for each day in a Week for First Quarter of Year, 1 = Sunday and 7 = Saturday",
       caption="Data collected from Cyclistic") #+ annotate("text", x=220, y=3500, label="The Gentoos are the Largest", color="purple", fontface="bold", size=4, angle=25)


average_ride_length <- bike_shre %>%
  group_by(day_of_week) %>%
  summarize(average_ride_length = mean(ride_length_sec, na.rm = TRUE))

View(average_ride_length)

ggplot(data=average_ride_length) + 
  geom_bar(mapping=aes(x=day_of_week, y=average_ride_length, fill = day_of_week), stat = "identity") +
  labs(title="Average Duration of Each Ride (s)", subtitle = "Data for each day in a Week for First Quarter of Year, 1 = Sunday and 7 = Saturday",
       caption="Data collected from Cyclistic") #+ annotate("text", x=220, y=3500, label="The Gentoos are the Largest", color="purple", fontface="bold", size=4, angle=25)


average_member_ride <- bike_shre %>%
  group_by(member_casual) %>%
  summarize(average_ride_length = mean(ride_length_sec, na.rm = TRUE))

View(average_member_ride)

ggplot(data=average_member_ride) + 
  geom_bar(mapping=aes(x=member_casual, y=average_ride_length, fill = member_casual), stat = "identity") +
  labs(title="Average Duration of Each Ride (s)", subtitle = "Data for different customers, casual = day-pass or single-pass rider and member = annual subscriber",
       caption="Data collected from Cyclistic") #+ annotate("text", x=220, y=3500, label="The Gentoos are the Largest", color="purple", fontface="bold", size=4, angle=25)


member_ride_counts <- bike_shre %>%
  group_by(member_casual) %>%
  summarise(num_rides = n())

View(member_ride_counts)

ggplot(data=member_ride_counts) + 
  geom_bar(mapping=aes(x=member_casual, y=num_rides, fill = member_casual), stat = "identity") +
  labs(title="Total Number of Rides", subtitle = "Data for different customers, casual = day-pass or single-pass rider and member = annual subscriber",
       caption="Data collected from Cyclistic") #+ annotate("text", x=220, y=3500, label="The Gentoos are the Largest", color="purple", fontface="bold", size=4, angle=25)

unique(bike_shre$Starting_Periods)
bike_shre$Starting_Periods <- trimws(bike_shre$Starting_Periods)
bike_shre$Starting_Periods <- case_when(
  bike_shre$Starting_Periods == "Morning" ~ "Morning",
  bike_shre$Starting_Periods == "Afternoon" ~ "Afternoon",
  bike_shre$Starting_Periods == "Evening" ~ "Evening",
  bike_shre$Starting_Periods == "Night" ~ "Night",
  TRUE ~ "Unknown" # Handle any unexpected or unidentified periods
)

periods_ride_counts <- bike_shre %>%
  group_by(Starting_Periods, member_casual) %>%
  summarise(num_rides = n())

filtered_periods_ride_counts <- periods_ride_counts %>%
  filter(Starting_Periods != "Unknown")

View(filtered_periods_ride_counts)

ggplot(data=filtered_periods_ride_counts) + 
  geom_bar(mapping=aes(x=Starting_Periods, y=num_rides, fill= member_casual), stat = "identity") +
  labs(title="Total Number of Rides", subtitle = "Data for different periods in a day",
       caption="Data collected from Cyclistic") 


periods_ride_length_avrg <- bike_shre %>%
  group_by(Starting_Periods,member_casual) %>%
  summarise(average_ride_length = mean(ride_length_sec, na.rm = TRUE))

filtered_periods_ride_length_avrg <- periods_ride_length_avrg %>%
  filter(Starting_Periods != "Unknown")

ggplot(data=filtered_periods_ride_length_avrg) + 
  geom_bar(mapping=aes(x=Starting_Periods, y=average_ride_length, fill= member_casual), stat = "identity") +
  labs(title="Average Duration of Each Ride", subtitle = "Data for different periods in a day",
       caption="Data collected from Cyclistic") #+ annotate("text", x=220, y=3500, label="The Gentoos are the Largest", color="purple", fontface="bold", size=4, angle=25)


