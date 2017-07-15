# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data

```r
#Load the data
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)

activity <- read.csv('activity.csv')

#Process the data
#Convert date variable to Date format
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
#Calculate the total number of steps taken per day
numberperday <- aggregate(activity$steps, list(activity$date), sum)

# Make a histogram of the total number of steps taken each day
par(mar=c(5,3,3,2))
hist(numberperday$x, xlab="Number of Steps per day", main="Frequency of Number of Steps a day", col="yellow", labels=TRUE, ylim=c(2,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate and report the mean and median of the total number of steps taken per day
meansteps <- mean(numberperday$x, na.rm=TRUE)
mediansteps <- median(numberperday$x, na.rm=TRUE)
```
The mean number steps per day is 1.0766189\times 10^{4}.  
The median number steps per day is 10765.

## What is the average daily activity pattern?


```r
# Get average number of steps per interval
numberperinterval <- aggregate(activity$steps, list(activity$interval), mean, na.rm=TRUE)

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
par(mar=c(5,6,3,2))
plot(numberperinterval$Group.1, numberperinterval$x, type = "l", xlab="5-Minute Interval Identifier", ylab="Average Number of Steps\n(Across All Days)", xlim=range(numberperinterval$Group.1), main="Number of Steps per 5-Minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Get the time interval with the maximum number of steps
maxsteps <- numberperinterval$Group.1[which.max(numberperinterval$x)]
```
  
    
The interval number which contains the maximum number of steps (on average) is 835. 

## Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NAs <- subset(activity, is.na(steps)==TRUE)
NumNAs <- dim(NAs)[1]
```

The total number of missing values in the dataset is 2304. 


```r
# Merge activity and interval means data frames so that the new data frame contains means for each inteval 
activityimp <- arrange(merge(activity, numberperinterval, by.x = "interval", by.y = "Group.1"), date, interval)

# Get interval means for missing step values 
means <- subset(activityimp, is.na(activityimp$steps), select="x")

# Replace missing step values with interval means
activityimp$steps <- replace(activityimp$steps, is.na(activityimp$steps), means$x)

#Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day
numberperdayimp <- aggregate(activityimp$steps, list(activityimp$date), sum)

hist(numberperdayimp$x, xlab="Number of Steps per day", main="Frequency of Number of Steps a day (with imputed steps)", col="yellow", labels=TRUE, ylim=c(2,40))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
meanstepsimp <- mean(numberperdayimp$x)

medianstepsimp <- median(numberperdayimp$x)

activityimp$date <- as.Date(activityimp$date, format="%Y-%m-%d")
```
The strategy used to impute missing values was to replace missing values with the mean number of steps for the interval in which the value was missing.

The mean number steps per day with missing values imputed is 1.0766189\times 10^{4}.  

The median number steps per day with missing values imputed is 1.0766189\times 10^{4}.

The result is that the days which previously contained NAs now contain the mean and medians with the mean steps per interval imputed.

## Are there differences in activity patterns between weekdays and weekends?

```r
# Create function to map day of week to weekday/weekend
weekdayorend <- function(day) {
    if (day=="Sunday" | day=="Saturday") {
        "weekend"
    } else if (is.na(day)==FALSE) {
        "weekday"
    } else {
        NA
    }
}

#Create new variable with day of week
activityimp <- mutate(activityimp, dayofweek=weekdays(activityimp$date))

#Determine if day of week is weekend or weekday
weekday <- sapply(activityimp$dayofweek, weekdayorend)
activityimp <- mutate(activityimp, dayofweek=weekday)

#Aggregate by Interval and weekday/weekend
activityimpavg <- aggregate(activityimp$steps, by=list(activityimp$interval, activityimp$dayofweek), FUN=mean)

xyplot(x ~ Group.1 | Group.2, data=activityimpavg, type="l", layout=c(1,2), xlab="5-Minute Interval Idenfier", ylab="Number of Steps (average)", main="Average Number of Steps per interval on Weekend vs. Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
