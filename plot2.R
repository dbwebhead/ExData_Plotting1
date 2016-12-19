## Project 1: Plotting Power Consumption - Plot 2

## Sypnosis:
## The goal here is to examine how household energy usage varies over a 2-day period 
## in February, 2007 by reproducing a series of four plots. 
## This code reproduces plot 2.

## install necessary packages
requiredPackages <- c("sqldf", "utils")
ipak <- function(pkg)
{
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}
ipak(requiredPackages)

# invoke libraries
library(utils)
library(sqldf)

## downnload the source file from Internet
if(!file.exists("household_power_consumption.txt")) {
        temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
        unlink(temp)
}

## read household power consumption data (constraining on first two days of Feb 2007)
power_data <- read.csv.sql("household_power_consumption.txt", 
                           sql = " SELECT * FROM file 
                                   WHERE Date = '1/2/2007' 
                                      OR Date = '2/2/2007'", 
                           sep=';', header=TRUE)

## format date and transform the power variables
power_data$Date <- as.Date(power_data$Date, format = "%d/%m/%Y")
power_df <- power_data[(power_data$Date == "2007-02-01") | (power_data$Date == "2007-02-02"),]
power_df$Global_active_power <- as.numeric(as.character(power_df$Global_active_power))
power_df$Global_reactive_power <- as.numeric(as.character(power_df$Global_reactive_power))
power_df$Voltage <- as.numeric(as.character(power_df$Voltage))
power_df <- transform(power_df, timestamp = as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
power_df$Sub_metering_1 <- as.numeric(as.character(power_df$Sub_metering_1))
power_df$Sub_metering_2 <- as.numeric(as.character(power_df$Sub_metering_2))
power_df$Sub_metering_3 <- as.numeric(as.character(power_df$Sub_metering_3))

## reproduce plot 2
plot2 <- function() {
        plot(power_df$timestamp,power_df$Global_active_power, 
             type = "l",                 ## create line type
             xlab = "",                  ## with no X-axis label
             ylab = "Global Active Power (kilowatts)")
        dev.copy(png, file = "plot2.png", width = 480, height = 480)
        dev.off()
}
plot2()