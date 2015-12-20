# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Show any code that is needed to:

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data <- read.csv('activity.csv')
data$date = as.POSIXct(data$date)
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day


```r
steps.per.day <- aggregate(data$steps, list(date = data$date), sum)[,2]
hist(steps.per.day, main='Histogram of total number of steps taken each day', xlab='Steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(steps.per.day, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(steps.per.day, na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean.spi <- aggregate(data$steps, list(interval = data$interval), mean, na.rm=T)
plot(mean.spi, type='l', xlab='Interval', ylab='Average no. steps taken per interval', main='Average daily activity pattern')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mean.spi[mean.spi$x == max(mean.spi$x),]$interval
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(data[is.na(data$steps), 1])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.  

    The chosen strategy replaces missing steps values with the average number of steps for that interval, averaged across all days.  The algorithm is as follows:  
    _For each row with a missing steps value:_  
     a. _get the interval number_  
     b. _calculate the mean number of steps across all days for that interval number_  
     c. _assign the result to the steps value of this row_  

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data2 <- data
for (idx in which(is.na(data2$steps))) {
    interval <- data2$interval[idx]
    avg.for.interval <- mean(data2[data2$interval==interval,]$steps, na.rm=T)
    data2$steps[idx] <- avg.for.interval
}
head(data2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day

```r
steps.per.day2 <- aggregate(data2$steps, list(date = data$date), sum)[,2]
hist(steps.per.day2, main='Histogram of total number of steps taken each day', xlab='Steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

5. Calculate and report the mean and median total number of steps taken per day.


```r
mean(steps.per.day2)
```

```
## [1] 10766.19
```

```r
median(steps.per.day2)
```

```
## [1] 10766.19
```

6. Do these values differ from the estimates from the first part of the assignment?

    _Yes, the median has changed slightly to reflect the fact that many rows now have the interval average number of steps._

    _The mean hasn't changed at all.  This is likely due to the fact that the mean value was used to fill missing values._

7. What is the impact of imputing missing data on the estimates of the total daily number of steps?

    _The proportion of days with steps in the range of 10000 to 15000 has increased relative to the others as a result of the inputed data being included.  This reflects the fact that the mean values added where probably in this range._

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data2$day <- ifelse(grepl('Saturday|Sunday', weekdays(data2$date)), 'weekend', 'weekday')
data2$day <- as.factor(data2$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
idx.wend <- which(data2$day == 'weekend')
mean.spi.wend <- aggregate(data2$steps[idx.wend], list(interval = data2$interval[idx.wend]), mean, na.rm=F)
mean.spi.wday <- aggregate(data2$steps[-idx.wend], list(interval = data2$interval[-idx.wend]), mean, na.rm=F)

par(mfrow=c(2,1), mar = c(4, 4, 2, 2))
plot(mean.spi.wend, type='l', main='Weekend', xlab='Interval', ylab='Number of steps', ylim=c(0, 200))
plot(mean.spi.wday, type='l', main='Weekday', xlab='Interval', ylab='Number of steps', ylim=c(0, 250))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
