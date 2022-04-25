library(tidyverse)
library(ggplot2)
library(GGally)

filename <- "data/passenger_satisfaction.csv"
df <- read_csv(filename)

df <- read_csv(filename, col_types = cols(gender = col_factor(), 
                                          customer_type = col_factor(), 
                                          travel_type = col_factor(),
                                          class = col_factor(),
                                          travel_type = col_factor(),
                                          satisfaction = col_factor(),
                                          wifi_service = col_factor(),
                                          departure_arrival_time_convenient = col_factor(),
                                          online_booking_ease = col_factor(),
                                          gate_location = col_factor(),
                                          food_drink = col_factor(),
                                          online_boarding = col_factor(),
                                          seat_comfort = col_factor(),
                                          inflight_entertainment = col_factor(),
                                          onboard_service = col_factor(),
                                          legroom_service = col_factor(),
                                          baggage_handling = col_factor(),
                                          checkin_service = col_factor(),
                                          inflight_service = col_factor(),
                                          cleanliness = col_factor(),
                                          departure_delay = col_factor(),
                                          arrival_delay = col_factor()
                                          ))

df <- read_csv(filename, col_types = cols(gender = col_factor(), 
                                          customer_type = col_factor(), 
                                          travel_type = col_factor(),
                                          class = col_factor(),
                                          travel_type = col_factor(),
                                          satisfaction = col_factor()
))


summary(df)
str(df)

# gender chart
ggplot(df, aes(x=gender, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Gender", y = "Proportion")

# customer type chart
ggplot(df, aes(x=customer_type, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Customer Types", y = "Proportion")

# travel type chart
ggplot(df, aes(x=travel_type, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Travel Type", y = "Proportion")

# class chart
ggplot(df, aes(x=class, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Class", y = "Proportion")

# class chart
ggplot(df, aes(x=flight_distance)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Flight Distance", y = "Proportion")

# satisfaction chart
ggplot(df, aes(x=satisfaction, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Overall Satisfaction", y = "Proportion")

# wifi service chart
ggplot(df, aes(x=wifi_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Wifi Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Wifi Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# Departure/Arrival time chart
ggplot(df, aes(x=departure_arrival_time_convenient, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Departure/Arrival Time Convenience", y = "Proportion") +
  scale_x_continuous("Departure/Arrival Time Convenience", breaks = c(0, 1, 2, 3, 4, 5))

# online booking chart
ggplot(df, aes(x=online_booking_ease, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Online Booking Ease", y = "Proportion") +
  scale_x_continuous("Online Booking Ease", breaks = c(0, 1, 2, 3, 4, 5))

# gate location chart
ggplot(df, aes(x=gate_location, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Gate Location Satisfaction", y = "Proportion") +
  scale_x_continuous("Gate Location Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# food drink chart
ggplot(df, aes(x=food_drink, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Food & Drink Satisfaction", y = "Proportion") +
  scale_x_continuous("Food & Drink Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# online boarding chart
ggplot(df, aes(x=online_boarding, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Online Boarding Satisfaction", y = "Proportion") +
  scale_x_continuous("Online Boarding Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# seat comfort chart
ggplot(df, aes(x=seat_comfort, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Seat Comfort Satisfaction", y = "Proportion") +
  scale_x_continuous("Seat Comfort Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# inflight entertainment chart
ggplot(df, aes(x=inflight_entertainment, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Inflight Entertainment Satisfaction", y = "Proportion") +
  scale_x_continuous("Inflight Entertainment Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# onboard service chart
ggplot(df, aes(x=onboard_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Onboard Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Onboard Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# legroom_service chart
ggplot(df, aes(x=legroom_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Legroom Satisfaction", y = "Proportion") +
  scale_x_continuous("Legroom Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# baggage handling chart
ggplot(df, aes(x=baggage_handling, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Baggage Handling Satisfaction", y = "Proportion") +
  scale_x_continuous("Baggage Handling Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# checkin chart
ggplot(df, aes(x=checkin_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Checkin Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Checkin Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# inflight service chart
ggplot(df, aes(x=inflight_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Inflight Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Inflight Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# cleanliness chart
ggplot(df, aes(x=cleanliness, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Cleanliness Satisfaction", y = "Proportion") +
  scale_x_continuous("Cleanliness Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# departure delay chart
ggplot(df, aes(x=departure_delay, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Departure Delay Satisfaction", y = "Proportion") +
  scale_x_continuous("Departure Delay Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# arrival delay chart
ggplot(df, aes(x=arrival_delay, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Arrival Delay Satisfaction", y = "Proportion") +
  scale_x_continuous("Arrival Delay Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

ggpairs(df)

sd(df$flight_distance)
sd(df$departure_delay)

arrival_delay <- drop_na(df)

sd(arrival_delay$arrival_delay)
