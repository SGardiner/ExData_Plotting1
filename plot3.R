## Plot3.R

library(dplyr)

## Download the data file
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
## unzip it
download.file(fileURL, destfile="household_power_consumption.zip", method = "curl")
## read it into variable 'household_power'
household_power <- read.csv(unzip("household_power_consumption.zip"), sep = ";")

plot3 <- function() {
        ## Prep the data
        ## filter to only two dates - Feb. 1st and 2nd, 2007
        power <- filter(household_power, Date == "1/2/2007" | Date == "2/2/2007")
        ## Add a date / time column with date class
        power$date <- strptime(paste(power$Date,power$Time), format = "%d/%m/%Y %H:%M:%S")
        ## Add a weekday column
        power$weekday <- weekdays(power$date)
        ## coerce the sub_meter data into a numeric value
        power$Sub_metering_1 <- as.numeric(as.character(power$Sub_metering_1))
        power$Sub_metering_2 <- as.numeric(as.character(power$Sub_metering_2))
        power$Sub_metering_3 <- as.numeric(as.character(power$Sub_metering_3))

        ## Plot the data
        ## Start with plot of the Sub_metering_1 in Black
        plot(power$date, power$Sub_metering_1, main = NULL, type = "l", xlab = " ", 
             ylab = "Energy sub metering", col = "black")
        ## Add Sub_metering_2 data in Red
        lines(power$date, power$Sub_metering_2, type = "l", col = "red")
        ## Add Sub_metering_3 data in Blue
        lines(power$date, power$Sub_metering_3, type = "l", col = "blue")
        ## Add the legend
        legend("topright", legend = c("Sub_metering_1  ","Sub_metering_2  ",
                "Sub_metering_3  "), lty = c(1,1), col = c("black", "red", "blue"))

        ## copy the screen to a png file
        dev.copy(png, file = "plot3.png")
        dev.off()  ## close the png device

}