
library(tidyverse); library(xtable)



steps_mean <- mean(Activity$steps , na.rm = TRUE)
steps_median <- median(Activity$steps , na.rm = TRUE)
c(steps_mean,steps_median)
total_steps <- sum(Activity$steps , na.rm = TRUE)

hist(Activity$steps, xlab = "Steps", ylab = "Frequency" , 
     main = "Histogram of Steps", col = "blue")
ggplot(data = Activity)+
  geom_histogram(aes(x = steps)) 

interval_mean <- Activity |>
  group_by(interval) |>
  summarise(Mean = round(mean(steps,na.rm = TRUE),digits = 2))
View(interval_mean)

plot(interval_mean$interval,interval_mean$Mean, type = "l")
max(interval_mean$Mean)
which.max(interval_mean$Mean)
interval_mean[which.max(interval_mean$Mean),]

# Reading File
Activity <- data.frame(read.csv(file = "./activity.csv"))

# q 1-3
daily_total <- Activity |>
                group_by(date) |>
                summarise(Total_Steps <- sum(steps, na.rm = TRUE))

colnames(daily_total) <- c("Date", "Sum")

ggplot(data = daily_total)+
  geom_histogram(aes(x = Sum))

total_mean <- mean(daily_total$Sum) ; total_median <- median(daily_total$Sum)

total_mean ; total_median

# steps by time intervals
interval_mean <- Activity |>
  group_by(interval) |>
  summarise(Mean = round(mean(steps,na.rm = TRUE),digits = 2))

# time series plot of time intervals and steps
ggplot(data = interval_mean)+
  geom_line(aes(x = interval, y = Mean))

# interval with maximum steps
interval_mean[which.max(interval_mean$Mean),]$interval


#number of NAs in the original data
nrow(Activity[Activity$steps == "NA",])

# replacing NAs with time interval mean
Activity_NA <- Activity
Activity_NA$Mean_Steps <- rep(interval_mean$Mean, 61)
Activity_NA[is.na(Activity_NA$steps),]$steps <- 
  Activity_NA[is.na(Activity_NA$steps),]$Mean_Steps

daily_total_NA <- Activity_NA |>
  group_by(date) |>
  summarise(Total_Steps <- sum(steps, na.rm = TRUE))

colnames(daily_total_NA) <- c("Date", "Sum")

ggplot(data = daily_total_NA)+
  geom_histogram(aes(x = Sum))

total_mean_NA <- mean(daily_total$Sum)
total_median_NA <- median(daily_total$Sum)
c(total_mean , total_mean_NA, total_median, total_median_NA)

?as.Date()
Activity_NA$New_Date <- as.Date(Activity_NA$date,
                             tryFormats =c("%Y-%m-%D" , "%Y/%m/%D"))

Activity$Day <- weekdays(Activity$New_Date)
Activity$Type_of_Day <- "NA"

#Weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
#Weekends <- c("Saturday", "Sunday")

Activity[Activity$Day == "Saturday",]$Type_of_Day <- "Weekends"
Activity[Activity$Day == "Sunday",]$Type_of_Day <- "Weekends"
Activity[Activity$Type_of_Day != "Weekends",]$Type_of_Day <- "Weekdays"
Activity$Type_of_Day <- as.factor(Activity$Type_of_Day)


Weekends_data <-  Activity[Activity$Type_of_Day == "Weekends",] %>%
            group_by(interval) %>%
            summarise(Mean_Type_of_day = round(mean(steps,na.rm = TRUE),
                                               digits = 2))
            
Weekdays_data <-  Activity[Activity$Type_of_Day == "Weekdays",] %>%
  group_by(interval) %>%
  summarise(Mean_Type_of_day = round(mean(steps,na.rm = TRUE),
                                     digits = 2))

Weekends_datafram <- data.frame(Weekdays_data$interval, 
        Weekdays_data$Mean_Type_of_day , Weekends_data$Mean_Type_of_day)
colnames(Weekends_datafram) <- c("Interval", "Weekdays", 
                                 "Weekends")

ggplot(data = Weekends_datafram , mapping = aes(x = Interval , y = Weekdays)) +
  geom_line(aes(x = Interval ,  y = Weekdays) , col = "blue") + 
  geom_line(aes(x = Interval , y = Weekends)) + 
  
  ylab("Steps") + xlab("5 Min Intervals")




