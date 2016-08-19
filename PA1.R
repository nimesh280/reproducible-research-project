library(knitr)
opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(ggplot2)
# load the data
data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character",
                                                                          "integer"))
data$date <- ymd(data$date)
str(data)
# Calculate the total number of steps taken per day
steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
# histogram of the total number of steps taken each day
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of Steps taken per day", x = "Steps per day", y = "Frequency")
# mean and median of the total number of steps taken per day
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
# To get the result
mean_steps
median_steps


# Calculate the average number of steps taken in each 5-minute interval per day using dplyr and group by interval
interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
#  time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "green")
# To find the interval containing the maximum number of steps?
interval[which.max(interval$steps),]


# total missing values
sum(is.na(data$steps))
# Create a new dataset as the original and use tapply for filling in the missing values with the average number of steps per 5-minute interval
data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
# Check for no missing values:
sum(is.na(data_full$steps))
# Calculate the number of steps taken in each 5-minute interval per day using dplyr and group by interval
steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
# Histogram for step per day including missing values
ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
# Calculate mean and median
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
# find the result for mean and median
mean_steps_full
median_steps_full


# Use dplyr and mutate to create a new column, weektype, and apply whether the day is weekend or weekday
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)
# Calculate the average steps in the 5-minute interval and use ggplot for making the time series of the 5-minute interval for weekday and weekend, and compare the average steps
interval_full <- data_full %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
