# Reproducible Research: Peer Assessment 1


```r
## Loading and preprocessing the data
library ("ggplot2")
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
library(chron)
activity <- read.csv("activity.csv")


## What is mean total number of steps taken per day?
## 1. Histogram of steps taken each day
stepsPerDay <- aggregate(steps~date, activity, sum, na.rm=TRUE)
g <- ggplot(stepsPerDay, aes(x=steps)) + geom_histogram(binwidth = 1000) +
     ggtitle("Steps taken each day")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)

```r
## 2. Mean and median steps per day
stepsPerDayMean <- mean(stepsPerDay$steps)
stepsPerDayMedian <- median(stepsPerDay$steps)
paste("Mean steps per day:", stepsPerDayMean)
```

```
## [1] "Mean steps per day: 10766.1886792453"
```

```r
paste("Median steps per day:", stepsPerDayMedian)
```

```
## [1] "Median steps per day: 10765"
```

```r
## What is the average daily activity pattern?
## 1. Time series plot of the average number of steps taken
stepsByInterval <- aggregate(steps~interval, activity, mean, na.rm=TRUE)
g <- ggplot(stepsByInterval, aes(x=interval, y=steps)) + geom_line() + 
     ggtitle("Average number of steps by Interval")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)

```r
## 2. Which 5-minute interval, on average across all the days in the 
## dataset, contains the maximum number of steps?
maxInterval <- stepsByInterval[which.max(stepsByInterval$steps),]
paste("5-minute interval on average with max steps: ", maxInterval)
```

```
## [1] "5-minute interval on average with max steps:  835"             
## [2] "5-minute interval on average with max steps:  206.169811320755"
```

```r
## Imputing missing values
## 1. Calculate and report the total number of missing values in 
## the dataset (i.e. the total number of rows with NAs)
missingVal <- sum(is.na(activity$steps))

## 2. Strategy to fill in the NA with mean 
## We use the mean for that 5-minute interval for missing values
activityNA <- activity[is.na(activity$steps),]
activity2 <- activity[!is.na(activity$steps),]
numRows <- nrow(activityNA)
for (i in 1:numRows) {
  activityNA[i,]$steps = 
    stepsByInterval[stepsByInterval$interval == activityNA[i,]$interval,]$steps
}

## 3. Merge the two datasets
activityNew = rbind(activityNA, activity2)

## 4. Histogram of the total number of steps taken each day after missing 
## values are imputed
stepsPerDayNew <- aggregate(steps~date, activityNew, sum, na.rm=TRUE)
g <- ggplot(stepsPerDayNew, aes(x=steps)) + geom_histogram(binwidth = 1000) +
     ggtitle("Steps taken each day after missing values are imputed")
print (g)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)

```r
## Mean and median steps per day with imputed values
stepsPerDayMeanImp <- mean(stepsPerDayNew$steps)
stepsPerDayMedianImp <- median(stepsPerDayNew$steps)
paste("Mean steps per day - Imputed:", stepsPerDayMeanImp)
```

```
## [1] "Mean steps per day - Imputed: 10766.1886792453"
```

```r
paste("Median steps per day - Imputed:", stepsPerDayMedianImp)
```

```
## [1] "Median steps per day - Imputed: 10766.1886792453"
```

```r
## Are there differences in activity patterns between weekdays and weekends?
## 1. Activity by weekend/weekday
activityNew$day <- weekdays(as.Date(activityNew$date,'%Y-%m-%d'))
activityNew$type <- ifelse(is.weekend(activityNew$date), "Weekend", "Weekday")
## 2. Time series plot
stepsbyWeekday <- aggregate(steps~type+interval, activityNew, mean, na.rm=TRUE)
g <- ggplot(stepsbyWeekday, aes(x=interval, y=steps)) +
      geom_line() + 
      facet_grid(type~.)  +
      ggtitle("Average steps per 5-minute interval across weekdays and weekends")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)
