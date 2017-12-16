library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

### Download the data file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "./datafiles.zip")
unzip(zipfile = "./datafiles.zip")

activity <- tbl_df(read.csv("./activity.csv"))
activity <- activity %>% mutate ( date = as.POSIXct(date, format="%Y-%m-%d")
)
activity <- activity %>% mutate (interval_start = formatC(interval, width=4, flag="0" ))
activity <- activity %>% mutate (interval_start = paste (substr(interval_start, 1,2) , ":" , substr(interval_start, 3,4) , sep=""))
##################################################################################
Steps_Per_Day <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)

g <- ggplot(Steps_Per_Day, aes(steps ))

g <- g + geom_histogram(binwidth =2500 , col="black", fill="dark blue") + 
    xlab("Steps per Day")  + 
    ylab("Frequency") +
    ggtitle("Total Number of Steps Taken Each Day")
print(g)

mean_steps <- as.numeric(round(mean(Steps_Per_Day$steps), digits=2))
median_steps <- round(median(Steps_Per_Day$steps), digits=2)

##################################################################################
Steps_By_Interval <- aggregate(steps ~ interval_start, activity, mean, na.rm=TRUE)
v <- c(1:289)
breaks <- (v%%12) == 1
breakpoints <- Steps_By_Interval$interval_start[breaks]
g <- ggplot(Steps_By_Interval, aes(x=interval_start, y=steps , group=1))
g <- g + geom_line(col="darkblue") + 
    xlab("Time")  + 
    scale_x_discrete(breaks = breakpoints ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
    ylab("Average Number of Steps") +
    ggtitle("Average Number of Steps in 5 Min Interval")
print(g)

Interval_with_max_steps <- Steps_By_Interval[which.max(Steps_By_Interval$steps),]$interval_start
##################################################################################

Total_missing_rows <- sum(is.na(activity$steps))

imputed_steps <-  activity %>% 
    mutate ( steps = ifelse (is.na(steps), Steps_By_Interval$steps[match(activity$interval, Steps_By_Interval$interval)] , steps ) )


imp_Steps_Per_Day <- aggregate(steps ~ date, imputed_steps, sum, na.rm=TRUE)

g <- ggplot(imp_Steps_Per_Day, aes(steps ))

g <- g + geom_histogram(binwidth =2500 , col="black", fill="green") + 
    xlab("Steps per Day")  + 
    ylab("Frequency") +
    ggtitle("Post Null Row Impution - Total Number of Steps Taken Each Day")
print(g)

imp_mean_steps <- round(mean(imp_Steps_Per_Day$steps), digits=2)
imp_median_steps <- round(median(imp_Steps_Per_Day$steps), digits=2)

##################################################################################

imputed_steps <- imputed_steps %>% mutate (day = ifelse(weekdays(as.Date(date) )%in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

Steps_By_Interval_day <- aggregate(steps ~ interval_start+day, imputed_steps, mean, na.rm=TRUE)
v <- c(1:289)
breaks <- (v%%12) == 1
breakpoints <- Steps_By_Interval_day$interval_start[breaks]

g <- ggplot(Steps_By_Interval_day, aes(x=interval_start, y=steps , group=1))
g <- g + geom_line()  +
    facet_grid(day ~ .) +
    xlab("Time")  + 
    scale_x_discrete(breaks = breakpoints ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 1)) +
    ylab("Average Number of Steps") +
    ggtitle("Average Number of Steps per Interval")
print(g)

Interval_with_max_steps <- Steps_By_Interval[which.max(Steps_By_Interval$steps),]$interval
