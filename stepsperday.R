#Loading and pre-processing data
#
#Un-pack the archive, and load it
unzip("activity.zip")
ds <- read.csv("activity.csv")
#pre-processing (tidy the dataset)
#Spreading the values of the interval-variable out in separate columns
#filling the values with the values from the steps-variable
library(tidyr)
library(dplyr)
myds <- spread(ds, interval, steps, fill = 0)

#What is the mean total number of steps taken per day?
#
#We'll use the untydied dataset for this...
#Summarize the steps/day into a list
stepsperday <- aggregate(steps ~ date, ds, sum)
#1. Generate a histogram
hist(stepsperday$steps, breaks = 5)
#2. Calculate the mean and median
#Mean
mspd <- mean(stepsperday$steps)
#Median
medspd <- median(stepsperday$steps)

#What is the average daily activity pattern?
#Now we'll use the tydied dataset
#1. Time-series plot
#the x-axis will be a char-vector of the range from 0000 to 2355 
x <- names(myds[,2:289])
y <- mean(myds[,2])
for(i in c(3:289)) {y <- cbind(y, mean(myds[, i]))}


#2. maximum average