## Load Data
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)

## What is mean total number of steps taken per day?
steps_per_day <- aggregate(x=list(steps=activity$steps), by=list(date=activity$date), 
                           FUN=sum, na.rm= T)

hist(steps_per_day$steps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)

mean(steps_per_day$steps)
median(steps_per_day$steps)

steps_per_interval <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), 
                           FUN=sum, na.rm= T)

## What is the average daily activity pattern?
library(ggplot2)
ggplot(steps_per_interval, aes(x=interval, y=steps))+ geom_line()

steps_per_interval[which(steps_per_interval$steps==max(steps_per_interval$steps)),]


## Imputing missing values
missingvals <- sum(is.na(activity$steps))

newdata<- activity
meansteps_per_interval <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm = T) 
newdata$steps<- replace(newdata$steps, list = (is.na(newdata$steps)), meansteps_per_interval$steps)

newsteps_per_day <- aggregate(x=list(steps=newdata$steps), by=list(date=newdata$date), 
                           FUN=sum, na.rm= T)

hist(newsteps_per_day$steps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)

mean(newsteps_per_day$steps)
median(newsteps_per_day$steps)

## Are there differences in activity patterns between weekdays and weekends?
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
newdata$weekday <- factor((weekdays(newdata$date) %in% weekdays1), 
                          levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

newsteps_per_interval <- aggregate(steps ~ interval + weekday, data = newdata, FUN = mean )

ggplot(newsteps_per_interval, aes(x=interval , y=steps)) +geom_line() + 
        facet_grid(weekday ~ .) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval")