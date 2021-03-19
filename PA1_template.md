---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---



## Loading and preprocessing the data

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./activity.zip")
unzip("./activity.zip")
d <- read.csv("activity.csv", na.strings = "NA")
names(d)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(d)
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

```r
# convert date variable to Date type
d$date <- as.Date(d$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day.

```r
d_omit_na <- na.omit(d)
steps_per_day <- with(d_omit_na, aggregate(steps, by = list(date), sum))
```

2. Make a histogram of the total number of steps taken each day.

```r
hist(steps_per_day$x, xlab = "Total number of steps", main = "Histogram of the total number of steps taken each day")
```

![](figures/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_per_day$x)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$x)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_avg <- aggregate(d$steps, by = list(d$interval), mean, na.rm = T)
plot(steps_avg$Group.1, steps_avg$x, type = "l", col = "steelblue", xlab = "5-minute interval", ylab = "Average number of steps")
```

![](figures/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_max <- which.max(steps_avg$x)
steps_avg$Group.1[steps_max]
```

```
## [1] 835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(d$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# use the mean for that 5-minute interval to fill the missing value
fill_missing_value <- function(interval){
     steps_avg[which(steps_avg[1] == interval), 2]
}
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
d_filled <- within(d, steps[is.na(steps)] <- fill_missing_value(interval))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_filled_per_day <- with(d_filled, aggregate(steps, by = list(date), sum))
hist(steps_filled_per_day$x, xlab = "Total number of steps", main = "The total number of steps taken each day after imputing missing data")
```

![](figures/unnamed-chunk-10-1.png)<!-- -->

```r
mean <- mean(steps_filled_per_day$x)
median <- median(steps_filled_per_day$x)
```

After imputing missing data, the mean for the total number of steps taken per day maintains its original value, which is still 1.0766189\times 10^{4}. However, the median of it has changed to 1.0766189\times 10^{4}. The total daily number of steps has been increased.


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
d_filled$weekend <- weekdays(d_filled$date) %in% c("Saturday", "Sunday")
d_filled$weekend <- factor(d_filled$weekend, c(FALSE, TRUE), labels = c("weekday", "weekend"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
steps_filled_avg <- with(d_filled, aggregate(steps, by = list(interval, weekend), mean))
names(steps_filled_avg) <- c("interval", "weekend", "avg")

library(ggplot2)
g<- ggplot(data = steps_filled_avg, aes(x = interval, y = avg, fill = weekend))
g + geom_line(color = "steelblue") + 
        facet_wrap(weekend~., nrow = 2) + 
        ylab("Number of steps") +
        theme(strip.background = element_rect(fill="salmon"),
              strip.text.x = element_text(size = 15, colour = "gray85"))
```

![](figures/unnamed-chunk-12-1.png)<!-- -->






