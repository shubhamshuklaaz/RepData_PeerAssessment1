---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

The dates are stored as characters. Hence I converted them using as.Date function

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

```r
steps_per_day <- aggregate(x=list(steps=activity$steps), by=list(date=activity$date), FUN=sum, na.rm= T)

hist(steps_per_day$steps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)
```

![](PA_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(steps_per_day$steps)
```

```
## [1] 9354.23
```

```r
median(steps_per_day$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
library(ggplot2)
steps_per_interval <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),  FUN=sum, na.rm= T)
ggplot(steps_per_interval, aes(x=interval, y=steps))+ geom_line()
```

![](PA_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
steps_per_interval[which(steps_per_interval$steps==max(steps_per_interval$steps)),]
```

```
##     interval steps
## 104      835 10927
```

## Imputing missing values

```r
missingvals <- sum(is.na(activity$steps))

newdata<- activity
meansteps_per_interval <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm = T) 
newdata$steps<- replace(newdata$steps, list = (is.na(newdata$steps)), meansteps_per_interval$steps)

newsteps_per_day <- aggregate(x=list(steps=newdata$steps), by=list(date=newdata$date), FUN=sum, na.rm= T)

hist(newsteps_per_day$steps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)
```

![](PA_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(newsteps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(newsteps_per_day$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
newdata$weekday <- factor((weekdays(newdata$date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

newsteps_per_interval <- aggregate(steps ~ interval + weekday, data = newdata, FUN = mean )

ggplot(newsteps_per_interval, aes(x=interval , y=steps)) +geom_line() + facet_grid(weekday ~ .) + xlab("Interval") + ylab("Mean of Steps") +
ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![](PA_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
                          
                          
