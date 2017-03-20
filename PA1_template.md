## Loading and preprocessing the data
library(ggplot2)
library(plyr)
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')

## Processing the data

activityData$day <-weekdays(as.Date(activityData$date))
activityData$DateTime <- as.POSIXct(activityData$date, format="%Y-%m-%d")
clean <- activityData[!is.na(activityData$steps),]

## Calculate total number of steps taken per day?

sumtable <- aggregate(activityData$steps ~ activityData$date, FUN=sum,)
colnames(sumtable) <- c("Date","Steps")

## Make a histogram of the total number of steps taken each day

hist(sumtable$Steps, breaks =5, xlab ="Steps",main ="Total Steps per Day")

## Histogram to be referred in the attached .pdf file

## Calculate and report the mean and median total number of steps taken per day

as.integer(mean(sumtable$Steps))
as.integer(median(sumtable$Steps))

## What is the average daily activity pattern?

averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 

## Plot to be referred in attached pdf file

mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])

## Imputing missing values
numMissingValues <- length(which(is.na(activityData$steps)))

## My strategy for filling in NAs will be to substitute the missing steps with the average 5 minute interval based on the day of the week.

## Create the average number of steps per weekday and interval

avgtable <- ddply(clean, .(interval,day), summarize, Avg = mean(steps))

## Create dataset with all NAs for substitution

nadata <- activityData[is.na(activityData$steps), ]

## Merge NA data with average weekday interval for substitution

newdata <- merge(nadata, avgtable, by= c("interval", "day"))

## Reorder the new substituded data in the same format as clean data set
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")

##Merge the NA averages and non NA data together
mergeData <- rbind(clean, newdata2)

##Create sum of steps per date to compare with step 1
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")

## Mean of Steps with NA data taken care of
as.integer(mean(sumTable2$Steps))
## Median of Steps with NA data taken care of
as.integer(median(sumTable2$Steps))

## Creating the histogram of total steps per day, categorized by data set to show impact
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )

## Are there differences in activity patterns between weekdays and weekends?
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

library(lattice) 
## Warning message: package ‘lattice’ was built under R version 3.3.2

## Summarize data by interval and type of day
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

##Plot data in a panel plot
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
