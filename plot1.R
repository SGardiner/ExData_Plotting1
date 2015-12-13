## Plot1.R 

library(dplyr)

## Download the data file
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
## unzip it
download.file(fileURL, destfile="household_power_consumption.zip", method = "curl")
## read it into variable 'household_power'
household_power <- read.csv(unzip("household_power_consumption.zip"), sep = ";")

plot1 <- function() {
        ## Prep the data
        ## filter to only two dates - Feb. 1st and 2nd, 2007
        power <- filter(household_power, Date == "1/2/2007" | Date == "2/2/2007")
        ## Add a date / time column with date class
        power$date <- strptime(paste(power$Date,power$Time), format = "%d/%m/%Y %H:%M:%S")
        ## Add a weekday column
        power$weekday <- weekdays(power$date)
        ## coerce the power data into a numeric value
        power$Global_active_power <- as.numeric(as.character(power$Global_active_power))

        ## Plot the data - a histogram of global active power
        par(mfrow=c(1,1), bg = "white")
        ## plot the histogram with the right xlabel, color and title
        hist(power$Global_active_power, xlab = "Global Active Power (killowatts)", 
             col = "red", main = "Global Active Power", bg = "white")
        
        ## copy the screen to a png file
        dev.copy(png, file = "plot1.png")
        dev.off()  ## close the png device
}



