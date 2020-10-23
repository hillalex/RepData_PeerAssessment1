---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
# Register an inline hook for formatting numbers
knitr::knit_hooks$set(inline = function(x) {
  if (is.numeric(x)) {
    sprintf("%.0f", x)
  }
  else {
    x
  }
})

library(tidyverse)
library(lubridate)
options(dplyr.summarise.inform = FALSE)
options(lubridate.week.start = 1)
options(digits = 10)
```

## Loading and preprocessing the data

The source data is included in this repo, in a zip file. To reproduce this analysis first inflate the data by running `./inflateData.sh`. This will result in the file `activity.csv` being created in the current directory.

We first read the data into R, and create some helper functions.


```r
data <- read_csv("activity.csv")
data_without_na <- data %>% filter(!is.na(steps))

get_total_per_day <- function(data) {
  data %>%
    group_by(date) %>%
    summarise(sum(steps)) %>%
    rename(total = `sum(steps)`)
}

draw_hist <- function(data) {
  qplot(data$total,
        geom = "histogram",
        binwidth = 895,
        xlab = "Total daily steps",
        ylab = "Frequency",
        fill = "salmon",
        alpha = 0.5) + guides(fill = FALSE, alpha = FALSE)
}
```

## What is the mean total number of steps taken per day?

The following figure shows the distribution of total daily steps:


```r
total_per_day <- get_total_per_day(data_without_na)
draw_hist(total_per_day)
```

![](/home/alex/Documents/dev/RepData_PeerAssessment1/PA1_template_files/figure-html/mean-1.png)<!-- -->

```r
mean <- mean(total_per_day$total)
median <- median(total_per_day$total)
```

The mean number of steps taken in a day is 10766 (to the nearest whole number).
The median number of steps taken in a day is 10765 (to the nearest whole number).

## What is the average daily activity pattern?

To see the average daily pattern, we group the data by interval and then
take the mean number of steps for each interval, across dates.


```r
average_across_days <- data_without_na %>%
  group_by(interval) %>%
  summarise(mean(steps)) %>%
  rename(mean = `mean(steps)`)

qplot(average_across_days$interval, average_across_days$mean,
      geom = "line",
      xlab = "Interval",
      ylab = "Mean number of steps")
```

![](/home/alex/Documents/dev/RepData_PeerAssessment1/PA1_template_files/figure-html/daily-1.png)<!-- -->

```r
max <- average_across_days %>%
  filter(mean == max(average_across_days$mean))

max_interval <- max$interval
```


The interval with the maximum number of average steps is 835

## Imputing missing values


```r
missing <- data %>%
  filter(is.na(steps))

num_missing <- nrow(missing)
```

There are 2304 missing rows in the data.

For a simple imputation of missing values, we replace each missing number of steps with the mean number of steps for that interval.


```r
imputed <- data %>%
  mutate(steps = replace(steps, is.na(steps), average_across_days$mean))

total_per_day_imputed <- get_total_per_day(imputed)
```

We can now visualise the total daily steps for our imputed data set, as before:


```r
draw_hist(total_per_day_imputed)
```

![](/home/alex/Documents/dev/RepData_PeerAssessment1/PA1_template_files/figure-html/mean-imputed-1.png)<!-- -->

```r
mean_imputed <- mean(total_per_day_imputed$total)
median_imputed <- median(total_per_day_imputed$total)
```

After imputing missing values, the mean number of steps taken in a day is 10766
and the median number of steps taken in a day is 10766. Notice that the mean remains the same as in the original dataset with missing values, and the median has been pulled towards the mean.

To see this more clearly, we can view the two datasets side by side and note that the distribution of frequencies is unchanged, with more weight simply added to the mean.


```r
total_excluding_missing_vals <- total_per_day %>% mutate(type = "excluding missing values")
total_with_imputed_vals <- total_per_day_imputed %>% mutate(type = "imputed missing value")

combined_data <- rbind(total_excluding_missing_vals, total_with_imputed_vals)

qplot(x = total,
      data = combined_data,
      facets = type ~ .,
      geom = "histogram",
      binwidth = 895,
      xlab = "Total daily steps",
      ylab = "Frequency",
      fill = "salmon",
      alpha = 0.5) +
  guides(fill = FALSE, alpha = FALSE) +
  geom_vline(data = combined_data, aes(xintercept = mean(total), color = "mean"), linetype = "dashed") +
  scale_color_manual(name = NULL, values = c(mean = "black"))
```

![](/home/alex/Documents/dev/RepData_PeerAssessment1/PA1_template_files/figure-html/compare-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

To compare weekend and weekday patterns, we plot the mean number of steps in each daily interval, disaggregated by type of day. We can see that on weekdays, more steps are taken earlier in the day, and at the weekends steps are more evenly distributed across the day.


```r
daytype_data <- imputed %>%
  mutate(daytype = factor(wday(date) < 6, labels = c("weekend", "weekday"))) %>%
  group_by(daytype, interval) %>%
  summarise(mean(steps)) %>%
  rename(mean = `mean(steps)`)

qplot(interval, mean,
      data = daytype_data,
      facets = daytype ~ .,
      geom = "line",
      xlab = "Interval",
      ylab = "Mean number of steps")
```

![](/home/alex/Documents/dev/RepData_PeerAssessment1/PA1_template_files/figure-html/weekend-1.png)<!-- -->
