---
title: "Reproducible_assignment"
---
Load the data
```{r}
activity = read.csv("activity.csv")
```
---omit the na values
```{r}
activity_filter = na.omit(activity)
```
---calcualte number of missing values in the data
```{r}
length(which(is.na(activity$steps)))

```
## [1] 2304
---Using datatable to transform data 
```{r}
ACFIL = data.table(activity_filter)
temp = ACFIL[,list(steps=sum(steps),interval=sum(interval)),by="date"]
d <- transform(temp,mean = steps / interval)
head(d)

```
---Plot the Step
```{r,echo=FALSE}
png("step.png",height=480,width=480)
hist(d$steps,col="red",xlab="Steps",ylab="total_stepstaken",main = "Steps_DATA")
dev.off()

```

----Calculate mean and Median
```{r}
library(stats)
mean(d$steps)
median(d$steps)

```

## [1] 10766
## [1] 10765
----embed plots 

```{r, echo=FALSE}
png("time-series.png",height=480,width=480)
steps_interval <- aggregate(steps ~interval,data = activity_filter ,mean)
plot(steps ~ interval, data = steps_interval, type = "l")
dev.off()
```

```{r}
steps_interval[which.max(stepsInterval$steps), ]$interval

```
## 835

----write a funtion for filling missing values.Calucte mean steps for each intervalto fill the missing anf then fille the missing value with mean
using thaat funtion

```{r}
intervalfun <- function(interval) {
  steps_interval[steps_interval$interval == interval, ]$steps
}
```

----Duplicate dataset Creation and then using for loop to fill it

```{r}
activityComplete <- activity  # Make a new dataset with the original data
count = 0  # Count the number of data filled in
for (i in 1:nrow(activityComplete)) {
  if (is.na(activityComplete[i, ]$steps)) {
    activityComplete[i, ]$steps <- intervalfun(activityComplete[i, ]$interval)
    count = count + 1
  }
}
```

```
----Total 2304 missing values were filled

----plot the histogram
```{r,echo=FALSE}
png("missing_value_filled.png",height=540,width=540)
total_step_missing = aggregate(steps~date,activityComplete,sum)
hist(total_step_missing$steps,col = "red",xlab="Steps",ylab="total_stepstaken",main = "Steps_After filling_DATA")
dev.off()

```
----Calculate Mean and Median
```{r}
mean(total_step_missing$steps)
median(total_step_missing$steps)
```
1076.19
1076.19
Minor changes were observerd

----Changes in the Pattern
```{r,echo=FALSE}
activityComplete$day = ifelse(as.POSIXlt(as.Date(activityComplete$date))$wday%%6 ==
0, "weekend", "weekday")
stepsInterval_miss = aggregate(steps ~ interval + day, activityComplete,mean)
```

----Plot the Changes
```{r}
library(lattice)
xyplot(steps ~interval | factor(day),data = stepsInterval_miss,aspect = 1/2,type = "l")
png("step_pattern.png",height=480,width=480)
xyplot(steps ~interval | factor(day),data = stepsInterval_miss,aspect = 1/2,type = "l")
dev.off()
```
