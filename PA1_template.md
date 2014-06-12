Loading and preprocessing the data
===================================

## Show any code that is needed to

* Load the data (i.e. read.csv())

* Process/transform the data (if necessary) into a format suitable for your analysis


```r
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )
data<-read.csv(unz("./activity.zip","activity.csv"),colClasses=c("numeric","myDate","numeric"))
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day


```r
dataStepsPerDay<-tapply(data$steps,data$date,sum,na.rm = TRUE)
hist(dataStepsPerDay)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

* Calculate and report the mean and median total number of steps taken per day


```r
mean(dataStepsPerDay,na.rm=TRUE)
```

```
## [1] 9354
```

```r
median(dataStepsPerDay,na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    

```r
aux<-aggregate(steps ~ interval, data=data, FUN = mean,na.rm = TRUE)
colnames(aux)[2]="mean_steps"
plot(aux$interval,aux$mean_steps,type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
aux[max(aux$mean_steps),]$interval
```

```
## [1] 1705
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
   sum(as.numeric(is.na(data$steps)))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I am going to substitute NA's for the mean of that particular intervale

 *Create a new dataset that is equal to the original dataset but with the missing data filled in.
    

```r
    new_data<-merge(data,aux,by.x="interval",by.y="interval")

    new_data <- within(new_data, steps[is.na(steps)] <- (mean_steps[is.na(steps)]))
    new_data$mean_steps=NULL
```
   
* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
new_dataStepsPerDay<-tapply(new_data$steps,data$date,sum,na.rm = TRUE)
hist(new_dataStepsPerDay)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(new_dataStepsPerDay,na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(new_dataStepsPerDay,na.rm=TRUE)
```

```
## [1] 10352
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
    

```r
aux_w <- weekdays(new_data$date)
aux_w[!(aux_w %in% c("Saturday","Sunday"))] = "weekday"
aux_w[aux_w %in% c("Saturday","Sunday")] = "weekend"
new_data$weekdayType <- aux_w
```
    
* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
    

```r
library("ggplot2")
last_aux<-aggregate(steps ~ weekdayType+interval, data=new_data, mean)
ggplot(last_aux, aes(interval,steps)) + facet_wrap(~weekdayType, ncol=1) + geom_line() + aes(color = factor(weekdayType) ) + scale_color_hue(label = "weekdayType")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
