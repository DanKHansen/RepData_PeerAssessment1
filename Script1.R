#Unpacking the dataset
unzip("activity.zip")

#Loading the dataset
dataset <- read.csv("activity.csv")

#Finding the aggregated number of steps pr. day
ag.steps.pr.day <- aggregate(steps ~ date, dataset, sum)

#Plotting a histogram of aggregated steps per day and the frequency of each observation
#the number of columns set to 21, as the maximum value of aggregated steps per day is approx: 21000: max(ag.steps.pr.day$steps)

#Plotting the histogram using the lattice package
library(lattice)
histogram(ag.steps.pr.day$steps, xlab = "Aggregated number of steps pr. day", ylab = "Frequency of observations", nint = 21)

#Finding the mean and median of aggregated steps per day
ds1mean <- mean(ag.steps.pr.day$steps)
ds1median <- median(ag.steps.pr.day$steps)

#Finding the mean aggregated number of steps per interval
mag.steps.pr.interval <- aggregate(steps ~ interval, dataset, mean)

#Plotting a time series graph of the result
plot(mag.steps.pr.interval$interval, mag.steps.pr.interval$steps, type = "l", ylab = "Avg. number of steps", xlab = "Time intervals", xlim = c(0, 2400), lab = c(12,4,0))

#Which interval contains the highest average number of steps
mag.steps.pr.interval[which.max(mag.steps.pr.interval$steps),]
#it is the 0835 time interval that contains the highest average numer of steps

#How many row in the original dataset contains NA-values
sum(is.na(dataset$steps))
#This could also have been found by the use of the summary() function

#The reason for missing values could be any...
#Forgot to wear the gadget, no batteries, any other kind of issues
#So in order to fill them in several approaches could be selected
#Fill the missing with day-averages, or interval averages or any other kind of approaches
#We could also use the mice package which includes several different ways to do this
#I've decided to use the default "ppm" (predictive mean matching) with default iterations based on the assumption
#that the reason for the missing values a of technical reasons, and that the person has worn
#the gadget throughout the test-periode.

#Loading the mice package
library(mice)

#Imputing the dataset
tempdata <- mice(dataset)
imputedData <- complete(tempdata, 1)

#Finding the aggregated number of steps pr. day in the imputed dataset
ag.steps.pr.day.2 <- aggregate(steps ~ date, imputedData, sum)

#Histogram of the imputed dataset
histogram(ag.steps.pr.day.2$steps, xlab = "Aggregated number of steps pr. day", ylab = "Frequency of observations", nint = 21)

#Finding the mean and median of aggregated steps per day on the imputed dataset
ds2mean <- mean(ag.steps.pr.day.2$steps)
ds2median <- median(ag.steps.pr.day.2$steps)

#As expected the median is the same, the mean is of course different, as we have replaced
#2304 missing values which results in larger data set, and the mean is dependent of not only
#the amount of observations but also the size of each value it self.

#Now lets see if there is a difference in pattern whether it is weekend or weekday

#First we'll create a factor variable of daytypes
daytype <- weekdays(as.Date(imputedData$date))
#Now replace all workingdays to the string "weekday"
daytype <- replace(daytype, daytype %in% list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekday")
#The same for weekends
daytype <- replace(daytype, daytype %in% list("Saturday", "Sunday"), "weekend")
#And glue it together with the imputed dataset
imputedData <- cbind(imputedData, daytype)
#We now have a dataset to analyse

#And now for the timeseries plot
#lets again find the mean of aggregated steps per interval
mag.steps.pr.interval.2 <- aggregate(steps ~ interval + daytype, imputedData, mean)

#Plotting a time series graph of the result
xyplot(mag.steps.pr.interval.2$steps ~ mag.steps.pr.interval.2$interval | daytype, data=mag.steps.pr.interval.2, type = "l", layout=c(1,2), xlab = "Intervals", ylab = "Aggr. number of steps")

#We can clearly see a different pattern during weekends. Weekdays tend to have a peak around
#0800 to 1000, whereas during weekends the number of steps seems to be more spread out 
#all through the day.
