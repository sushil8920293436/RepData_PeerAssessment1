---
title: 'Reproducible Research: Peer Assessment 1'
author: "Mano"
date: "07/03/2020"
output: html_document
 
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
####  1.Load the data
```{r}
activity <- read.csv("activity.csv")
str(activity)
summary(activity)
head(activity)
```
####  2.Process/transform the data (if necessary) into a format suitable for your analysis
```{r}
##For the following tasks, we will need to remove missing values. So we create a second version of the data without missing values.
act.complete <- na.omit(activity)
```

## What is mean total number of steps taken per day?
#### 1.Calculate the total number of steps taken per day
```{r}
library(dplyr)
library(ggplot2)
act.day <- group_by(act.complete, date)
act.day <- summarize(act.day, steps=sum(steps))
summary(act.day)
```

#### 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```{r}
qplot(steps, data=act.day,main="Histogram of Total Steps by day")
```

#### 3.Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(act.day$steps)
median(act.day$steps)
```


## What is the average daily activity pattern?

####  1.Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
act.int <- group_by(act.complete, interval)
act.int <- summarize(act.int, steps=mean(steps))
ggplot(act.int, aes(interval, steps)) + geom_line()
```

####  2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
act.int[act.int$steps==max(act.int$steps),]
```


## Imputing missing values

####  1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
nrow(activity)-nrow(act.complete)
```
####  2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
names(act.int)[2] <- "mean.steps"
act.impute <- merge(activity, act.int)
```

####  3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
act.impute$steps[is.na(act.impute$steps)] <- act.impute$mean.steps[is.na(act.impute$steps)]
```
####  4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
act.day.imp <- group_by(act.impute, date)
act.day.imp <- summarize(act.day.imp, steps=sum(steps))
qplot(steps, data=act.day.imp,main = "Total Daily Steps")
mean(act.day.imp$steps)
median(act.day.imp$steps)
```


## Are there differences in activity patterns between weekdays and weekends?
####  1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
act.impute$dayofweek <- weekdays(as.Date(act.impute$date))
act.impute$weekend <-as.factor(act.impute$dayofweek=="Saturday"|act.impute$dayofweek=="Sunday")
levels(act.impute$weekend) <- c("Weekday", "Weekend")
```

####  2.Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r}
act.weekday <- act.impute[act.impute$weekend=="Weekday",]
act.weekend <- act.impute[act.impute$weekend=="Weekend",]

act.int.weekday <- group_by(act.weekday, interval)
act.int.weekday <- summarize(act.int.weekday, steps=mean(steps))
act.int.weekday$weekend <- "Weekday"
act.int.weekend <- group_by(act.weekend, interval)
act.int.weekend <- summarize(act.int.weekend, steps=mean(steps))
act.int.weekend$weekend <- "Weekend"

act.int <- rbind(act.int.weekday, act.int.weekend)
act.int$weekend <- as.factor(act.int$weekend)
ggplot(act.int, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)+ ggtitle("Comparison of Average Number of Steps in Each Interval")

```
