# Reproducible Research Project 1



## Loading and preprocessing the data

###1. Unzip data to obtain the csv file.

```r
unzip("repdata_data_activity.zip")
```

###2.Load the data and show summary of the data

```r
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
###3. Convert date to POSIXct class using lubridate package and convert interval to hour:minute format

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.5
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity$date <- ymd(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
##What is mean total number of steps taken per day?
### 1. Calculate the total number of steps taken per day

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
total_day <- activity %>% group_by(date) %>%summarise(total_steps=sum(steps,na.rm=TRUE)) %>% print
```

```
## Source: local data frame [61 x 2]
## 
##          date total_steps
##        (date)       (int)
## 1  2012-10-01           0
## 2  2012-10-02         126
## 3  2012-10-03       11352
## 4  2012-10-04       12116
## 5  2012-10-05       13294
## 6  2012-10-06       15420
## 7  2012-10-07       11015
## 8  2012-10-08           0
## 9  2012-10-09       12811
## 10 2012-10-10        9900
## ..        ...         ...
```
### 2. Make a histogram of the total number of steps taken each day

```r
sub_day <- subset(total_day, total_steps > 0)
hist(total_day$total_steps,breaks=40, main = "Histogram of Total Steps per Day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###3. Calculate and report the mean and median total number of steps taken per day

```r
mean_steps <- mean(total_day$total_steps,na.rm=TRUE)
median_steps <- median(total_day$total_steps,na.rm=TRUE)
print(round(mean_steps))
```

```
## [1] 9354
```

```r
print(round(median_steps))
```

```
## [1] 10395
```
##What is the average daily activity pattern?
###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
daily_patterns <- activity %>% group_by(interval) %>% summarise(average=mean(steps,na.rm=TRUE))
plot(x = daily_patterns$interval,y = daily_patterns$average,type = "l",
     main = "Average steps for given intervals across all days",
     xlab = "intervals",
     ylab = "Average steps for given intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
print(daily_patterns$interval[which.max(daily_patterns$average)])
```

```
## [1] 835
```
##Imputing missing values
###1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
na_number <- sum(is.na(activity$steps))
print(na_number)
```

```
## [1] 2304
```
###2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
steps_noNAs <- vector()
for (i in 1:nrow(activity))
{
  if (is.na(activity$steps[i]) == TRUE)
  {
    steps_noNAs[i]<-filter(daily_patterns,interval==activity[i,"interval"]) %>% select(average)
  }
  else
  {
    steps_noNAs[i]<-activity$steps[i]
  }
}
```
###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(dplyr)
new_activity<-mutate(activity,steps_no_NAs=as.numeric(steps_noNAs))
str(new_activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date        : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval    : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps_no_NAs: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```
###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_day_noNAs <- new_activity %>% group_by(date) %>% summarise(total_steps=sum(steps_no_NAs))
hist(total_day_noNAs$total_steps,breaks=40, main = "Histogram of Total Steps per Day after inputation", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
print(mean(total_day$total_steps))
```

```
## [1] 9354.23
```

```r
print(median(total_day$total_steps))
```

```
## [1] 10395
```

```r
print(mean(total_day_noNAs$total_steps))
```

```
## [1] 10766.19
```

```r
print(median(total_day_noNAs$total_steps))
```

```
## [1] 10766.19
```
Compared to the data before imputation, both the mean and median values of imputed data increase.
##Are there differences in activity patterns between weekdays and weekends?
###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
new_activity$day = as.factor(ifelse(is.element(weekdays(new_activity$date),weekdays), "Weekday", "Weekend"))
str(new_activity)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date        : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval    : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps_no_NAs: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ day         : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
summary(new_activity$day)
```

```
## Weekday Weekend 
##   12960    4608
```
###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
daily_patterns_new <- new_activity  %>% group_by(interval,day) %>% summarise(average=mean(steps_no_NAs))
library(lattice)
xyplot(daily_patterns_new$average ~ daily_patterns_new$interval|daily_patterns_new$day, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


