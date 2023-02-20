
library(tidyverse); library(xtable)
Activity <- data.frame(read.csv(file = "./activity.csv"))
summary(Activity)
# good for RMarkDown
xtable(summary(Activity))
hist(Activity$steps)

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

nrow(Activity[Activity$steps == "NA",])
for (i in 1:nrow(Activity)) {
  if (Activity$steps[i] == "NA"){
    
  }
  
}





# q 1-3
daily_total <- Activity |>
                group_by(date) |>
                summarise(Total_Steps <- sum(steps, na.rm = TRUE)
                          , Mean_Steps <- round(mean(steps, na.rm = TRUE),2)
                          , Median_Steps <- median(steps , na.rm = TRUE))

view(daily_total)
# q4
plot(1:nrow(daily_total) , 
     daily_total$`Mean_Steps <- round(mean(steps, na.rm = TRUE), 2)`, 
     type = "l") 

# q5
interval_mean <- Activity |>
  group_by(interval) |>
  summarise(Mean = round(mean(steps,na.rm = TRUE),digits = 2))

# tackling the NA problem
Activity$Mean_Steps <- rep(interval_mean$Mean, 61)
Activity[is.na(Activity$steps),]$steps <- 
  Activity[is.na(Activity$steps),]$Mean_Steps

Activity <- Activity[,-4]

for (i in 1:nrow(Activity)) {
  if(Activity$steps[i] == "NA"){
    Activity$steps[i] <- Activity$Mean_Steps[i]
  }
  
}
#number of NAs in the original data
nrow(Activity[Activity$steps == "NA",])
Activity$New_Date <- as.Date(Activity$date,
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




