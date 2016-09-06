#"Markdown Project for Coursera Reproducible Research"
### Peng Li
======================================================================================================================================



## 1. Loading and preprocessing the data

```r
fileurl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl,destfile = "Dataset.zip")
activity <- read.csv(unz("Dataset.zip", "activity.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
```

## 2. What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset


#### Find the sum of steps group by each day 

```r
require(dplyr)
library(dplyr)
activity_by_date <- activity %>%
  group_by(date) %>% 
  summarize_each(funs(sum))  
activity_by_date$date <- strptime(activity_by_date$date, "%Y-%m-%d")
activity_by_date$date <- as.Date(activity_by_date$date)
activity_by_date$steps <- as.numeric(activity_by_date$steps)
```


#### Plot the histogram of the total number of steps taken each day

```r
with(data = activity_by_date, hist(steps, breaks = 50, xlab = "Number of Steps per day", main = "Histogram of Total number of Steps per day"))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)


#### Calculate and report the mean and median of the total number of steps taken per day

```r
mean_step <- mean(activity_by_date$steps, na.rm = TRUE)
median_step <- median(activity_by_date$steps, na.rm = TRUE)
median_date <- activity_by_date$date[which(activity_by_date$steps==median_step)]
print(mean_step)
```

```
## [1] 10766.19
```

```r
print(median_step)
```

```
## [1] 10765
```
The mean of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>.                                                               
The median of the total number of steps taken per day is 1.0765 &times; 10<sup>4</sup>, occured at 2012-11-12.

## 3. What is the average daily activity pattern?

#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activity_by_interval <- activity[!is.na(activity$steps),] %>%
  group_by(interval) %>% 
  summarize(Average_Steps=mean(steps)) 
with(data = activity_by_interval, plot(x=interval, y=Average_Steps, type = "l", xlab = "5-minute interval", col = "red", 
ylab = "average number of steps taken", main = "Time Series Plot of average number of steps taken over 5-min Interval"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps <- max(activity_by_interval$Average_Steps)
max_interval <- activity_by_interval$interval[which(activity_by_interval$Average_Steps==max_steps)]
```
The 5 minute interval at number 835 interval, on average across all the days in the dataset, contains the maximum number of steps of 206.1698113


## 4. Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
table(is.na(activity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
table(is.na(activity$date))
```

```
## 
## FALSE 
## 17568
```

```r
table(is.na(activity$interval))
```

```
## 
## FALSE 
## 17568
```

```r
num_missing_value <- sum(is.na(activity$steps))
```
The total number of missing values in the dataset is about 2304

#### Devise a strategy for filling in all of the missing values in the dataset, and Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
# Create a new data set called activity2 and fill the NAs with the mean for the same 5-minute interval
activity2 <- activity
activity2$steps <- as.numeric(activity2$steps)
activity2$interval <- as.numeric(activity2$interval)
activity_by_interval$interval <- as.numeric(activity_by_interval$interval)
ind <- which(is.na(activity2$steps))
for (i in ind) {
activity2$steps[i] <- activity_by_interval[match(activity2$interval[i],activity_by_interval$interval),2]
}
activity2$steps <- as.numeric(activity2$steps)
activity2$date <- strptime(activity2$date, "%Y-%m-%d")
activity2$date <- as.Date(activity2$date)
```
Double check the number of missing values of the rows in activity 2

```r
table(is.na(activity2$steps))
```

```
## 
## FALSE 
## 17568
```
There is no "NA" in activity2

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

Repeat the same procedures of Question 2

```r
activity_by_date2 <- activity2 %>%
  group_by(date) %>% 
  summarize_each(funs(sum))  
activity_by_date2$date <- strptime(activity_by_date2$date, "%Y-%m-%d")
activity_by_date2$date <- as.Date(activity_by_date2$date)
activity_by_date2$steps <- as.numeric(activity_by_date2$steps)
```

Plot the histogram of the total number of steps taken each day

```r
with(data = activity_by_date2, hist(steps, breaks = 50, xlab = "Number of Steps per day", main = "Histogram of Total number of Steps per day"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

Calculate and report the mean and median of the total number of steps taken per day

```r
mean_step2 <- mean(activity_by_date2$steps)
median_step2 <- median(activity_by_date2$steps)
median_date2 <- activity_by_date2$date[which(activity_by_date2$steps==median_step2)]
print(mean_step2)
```

```
## [1] 10766.19
```

```r
print(median_step2)
```

```
## [1] 10766.19
```

The mean of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>.                                                               
The median of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>, occured at 2012-10-01, 2012-10-08, 2012-11-01, 2012-11-04, 2012-11-09, 2012-11-10, 2012-11-14, 2012-11-30.

### Question: Do these values differ from the estimates from the first part of the assignment? 
    No, the mean and median of the total number steps taken per day don't differ than the estimates from the first part
### What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# lOOK at the summary of two data sets of daily steps 
summary(activity_by_date$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

```r
summary(activity_by_date2$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

  Since the values we replace NA with are the average steps of given intervals, so it does not impact the mean of total daily steps that much, as well as not impact the min and max. But the 1st Quantile, median, and 3rd Quantile do change due to the removing of NAs

## 5. Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
days <- weekdays(activity2$date) 
weekday <- days
indi <- days=="Saturday" | days=="Sunday"
weekday[indi]<-"Weekend day"
weekday[!indi]<-"Weekday"
weekday <- as.factor(weekday)
activity2$day <- weekday
```


##### Group the data by intervals


```r
activity_by_interval2 <- activity2 %>%
  group_by(interval, day) %>% 
  summarize(Average_Steps=mean(steps)) 
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
library(lattice)
xyplot(Average_Steps~interval | day, data = activity_by_interval2,
      col = "blue",
      type = "l",
      xlab = "5-minute interval",
      ylab = "Average number of steps taken",
      main = "Time series plot of Average number of steps vs.5-minute interval",
      layout = c(1,2))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)
