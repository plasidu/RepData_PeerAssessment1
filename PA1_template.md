---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv('activity.csv')
data$date <- as.Date(data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
Histogram showing frequencies of sums of steps taken per day

```r
hist(with(data, tapply(steps, date, FUN=sum)))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
Mean of sums of steps taken per day 

```r
mean(with(data, tapply(steps, date, FUN=sum)), na.rm=T)
```

```
## [1] 10766.19
```
Median of sums of steps taken per day

```r
median(with(data, tapply(steps, date, FUN=sum)), na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
meanSteps <- with(data, tapply(steps, interval, FUN=mean, na.rm=T))
plot(meanSteps, type='l')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
meanSteps[meanSteps==max(meanSteps)]
```

```
##      835 
## 206.1698
```


## Imputing missing values
The amount of missing vaules:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
Create new dataset with missing values filled with mean for a given interval:

```r
datb <- data
datb <- within(datb, steps <- ifelse(!is.na(steps), steps, meanSteps))
```
## What is mean total number of steps taken per day?
Histogram showing frequencies of sums of steps taken per day after substituting missing values:

```r
hist(with(datb, tapply(steps, date, FUN=sum)))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
Mean of sums of steps taken per day 

```r
mean(with(datb, tapply(steps, date, FUN=sum)))
```

```
## [1] 10766.19
```
Median of sums of steps taken per day

```r
median(with(datb, tapply(steps, date, FUN=sum)))
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
Function asking whether it is weekday or not:

```r
isWeekday <- function(date) {
	if(weekdays(date)=="Saturday"|weekdays(date)=="Sunday") {
	form <- 'weekend'}
	else {form <- 'weekday'}
	return(form)
}
```
Add a column 'weekpart' using isWeekday:

```r
weekpart <- as.character(lapply(datb$date, isWeekday))
datb <- cbind(datb, weekpart=weekpart)
head(datb)
```

```
##       steps       date interval weekpart
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```
Split marked data:

```r
s <- split(datb, datb$weekpart)
```
Plot:

```r
par(mfrow = c(2, 1))
plot(with(s$weekday, tapply(steps, interval, FUN=mean)), main="Weekdays", type="l")
plot(with(s$weekend, tapply(steps, interval, FUN=mean)), main="Weekends", type="l")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
