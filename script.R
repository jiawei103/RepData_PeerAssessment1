data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE)

#sum of steps per day
sum_by_day <- tapply(data$step, data$date, sum, na.rm = TRUE)
#hist(sum_by_day, main = "Total Steps per Day", xlab = "Total Steps", ylab = "Days")
print(mean(sum_by_day))
print(median(sum_by_day))

#average per interval across all the days
average_by_interval <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
times <- unique(data$interval)
times <- formatC(times, width = 4, format = "d", flag = "0")
times <- strptime(times, "%H%M")

library(ggplot2)
data2 <- data.frame(interval = times, average = average_by_interval)
#g <- ggplot(data2, aes(interval, average)) + geom_line() + scale_x_datetime(date_labels = "%H:%M")
#print(g)

library(lubridate)
maximum <- max(data2$average)
location <- grep(maximum, data2$average)
h <- hour(data2[location, 1])
m <- minute(data2[location, 1])
print(paste("Maximum average number of steps at", h, ":", m))

#Imputing missing values
#1
sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))

#2-3 Using mean of each interval to replace NA
data_imputed <- data
for(i in 1:dim(data_imputed)[1]) {
    if(is.na(data_imputed$steps[i])) {
        data_imputed$steps[i] <- data2[as.character(data_imputed$interval[i]),2]
    }
}

#4
sum_by_day2 <- tapply(data_imputed$step, data_imputed$date, sum)
#hist(sum_by_day2, main = "Total Steps per Day", xlab = "Total Steps", ylab = "Days")
print(mean(sum_by_day2))
print(median(sum_by_day2))

#Weekday vs Weekend
data_imputed$dayofweek <- weekdays(strptime(data_imputed$date, "%Y-%m-%d"))
wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
wend <- c("Saturday", "Sunday")
for(i in 1:dim(data_imputed)[1]) {
    if(data_imputed$dayofweek[i] %in% wdays) {
        data_imputed$dayofweek[i] <- 1
    } else {
        data_imputed$dayofweek[i] <- 2
    }
}
data_imputed$dayofweek <- factor(data_imputed$dayofweek, levels = c(1, 2), labels = c("Weekday", "Weekend"))


#Comparing plots
library(dplyr)
data3 <- as.tbl(data_imputed)
data3 <- group_by(data3, dayofweek, interval)
data3 <- summarise(data3, mean = mean(steps))

library(lattice)
xyplot(mean ~ interval | dayofweek, data = data3, type = "l", xlab = "Interval", ylab = "Number of Steps", layout = c(1,2))





