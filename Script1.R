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
histogram(ag.steps.pr.day$steps, data = steps, xlab = "Aggregated number of steps pr. day", ylab = "Frequency of observations", nint = 21)


#Finding the mean and median of aggregated steps per day
mean(ag.steps.pr.day$steps)
median(ag.steps.pr.day$steps)

#Finding the mean aggregated number of steps per interval
mag.steps.pr.interval <- aggregate(steps ~ interval, dataset, mean)

#Plotting a time series graph of the result
plot(mag.steps.pr.interval$interval, mag.steps.pr.interval$steps, type = "l", ylab = "Avg. number of steps", xlab = "Time intervals", xlim = c(0, 2400), lab = c(12,4,0))

#Which interval contains the highest average number of steps
mag.steps.pr.interval[which.max(mag.steps.pr.interval$steps),]


#How many row in the original dataset contains NA-values
sum(is.na(dataset$steps))

#The strategy to fill the NA vales is as follows:
#fill each gap with a correspondant mean value for that specific day, if the day do not have a mean value, 0 will be used
library(mice)

