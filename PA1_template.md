# PA1_template
Stephen Maione  
Thursday, July 17, 2014  


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

```r
raw_data <- read.csv('activity.csv')
```

2. Process/transform the data (if necessary) into a format suitable for your analysis



## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

```r
library(lattice)

daily_steps <- split(raw_data, raw_data$date)
daily_totals <- sapply(daily_steps, function(x) sum(x[, 'steps'], na.rm=TRUE))

bins <- seq(0, 22500, 2500)
histogram(daily_totals, type='count', col='gray',
          xlab="Steps", main="Steps Taken Each Day",
          scales=list(x=list(at=bins, labels=bins)),
          breaks=bins)
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
raw_mean <- mean(daily_totals, na.rm=TRUE)
raw_median <- median(daily_totals, na.rm=TRUE)
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval_steps <- split(raw_data, raw_data$interval)
interval_totals <- sapply(interval_steps, function(x) sum(x[, 'steps'],
                                                          na.rm=TRUE))
num_days <- length(levels(raw_data$date))
steps_per_interval <- interval_totals / num_days

intervals <- as.numeric(names(steps_per_interval))
xyplot(steps_per_interval ~ intervals, type='l',
       xlab='Intervals', ylab='Average Steps',
       main="Average Steps Per 5-Minute Daily Interval")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_value <- max(steps_per_interval)
max_interval <- intervals[steps_per_interval == max_value]
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing_locs <- !complete.cases(raw_data$steps)
num_missing <- sum(missing_locs)
num_missing
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I decided to fill missing values with the interval average.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fill_vals <- steps_per_interval[as.character(raw_data[missing_locs,
                                                      'interval'])]
complete_data <- raw_data
complete_data[missing_locs, 'steps'] <- fill_vals
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
library(lattice)

complete_daily_steps <- split(complete_data, complete_data$date)
complete_daily_totals <- sapply(complete_daily_steps,
                                function(x) sum(x[, 'steps'], na.rm=TRUE))

bins <- seq(0, 22500, 2500)
histogram(complete_daily_totals, type='count', col='gray',
          xlab="Steps", main="Steps Taken Each Day",
          scales=list(x=list(at=bins, labels=bins)),
          breaks=bins)
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 

```r
complete_mean <- mean(complete_daily_totals, na.rm=TRUE)
complete_median <- median(complete_daily_totals, na.rm=TRUE)

mean_diff <- complete_mean - raw_mean
mean_diff
```

```
## [1] 1227
```

```r
median_diff <- complete_median - raw_median
median_diff
```

```
## [1] 0
```

```r
raw_discrepancy <- raw_mean - raw_median
raw_discrepancy
```

```
## [1] -1041
```

```r
complete_discrepancy <- complete_mean - complete_median
complete_discrepancy
```

```
## [1] 186
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


