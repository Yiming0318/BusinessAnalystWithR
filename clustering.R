rm(list=ls())
getwd()
setwd("C:/Users/Gazi/Desktop/NYU_Classes/Fall_2020/Teaching/Complex_Systems_Engineering/software")
getwd()

apr14 <- read.csv('uber-raw-data-apr14.csv')
may14 <- read.csv('uber-raw-data-may14.csv')
jun14 <- read.csv('uber-raw-data-jun14.csv')
jul14 <- read.csv('uber-raw-data-jul14.csv')
aug14 <- read.csv('uber-raw-data-aug14.csv')
sep14 <- read.csv('uber-raw-data-sep14.csv')

head(apr14,10)

tail(apr14,10)

install.packages('dplyr')
library(dplyr)

data14 <- dplyr::bind_rows(apr14, may14, jun14, jul14, aug14, sep14)

summary(data14)

# VIM library for using 'aggr'
install.packages('VIM')
library(VIM)

# 'aggr' plots the amount of missing/imputed values in each column
aggr(data14)

install.packages('lubridate')
library(lubridate)

# Separate or mutate the Date/Time columns
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))

#data14$date_time
data14$Month <-  factor(month(data14$Date.Time))  

head(data14, 10)

set.seed(20)
clusters <- kmeans(data14[,2:3], 5)

data14$Borough <- as.factor(clusters$cluster)

# Inspect 'clusters'
str(clusters)

install.packages('ggmap')
library(ggmap)

NYCMap <- get_map("New York", zoom = 10)
ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")
