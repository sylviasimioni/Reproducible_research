library(readr)
activity <- read_csv("~/coursera/Reproducible Research/activity.csv")
View(activity)
library(ggplot2)
library(dplyr)
Sys.setlocale("LC_TIME", "English")
str(activity)

StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
StepsPerDay

g <- ggplot(StepsPerDay, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="navy", fill="gray")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))

mean(StepsPerDay$Steps, na.rm=TRUE)

median(StepsPerDay$Steps, na.rm=TRUE)

StepsPerTime <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
StepsPerTime$time <- StepsPerTime$interval/100
h <- ggplot(StepsPerTime, aes(time, steps))
h+geom_line(col="brown")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))

ST <- as_tibble (StepsPerTime)
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))

ACT <- as_tibble(activity)
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())

activity$CompleteSteps <- ifelse(is.na(activity$steps), round(StepsPerTime$steps[match(activity$interval, StepsPerTime$interval)],0), activity$steps)
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
head(activityFull, n=10)

StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))

mean(StepsPerDayFull$Steps)
median(StepsPerDayFull$Steps)

activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
activityFull$weekday <- weekdays(activityFull$RealDate)
head(activityFull, n=10)

activityFullselect <- select(activityFull, steps, weekday)
head(activityFullselect)


StepsPerweekday <- aggregate(steps~ weekday,data=activityFull,FUN=mean,na.action=na.omit)
StepsPerTime$time <- StepsPerTime$interval/100
StepsPerweekday

h <- ggplot( data = activityFull, aes(weekday, steps))
h <- h + stat_summary(fun.y = mean, geom = "bar", fill= "brown") + ggtitle("Average Steps per Weekday") 
h        
            
