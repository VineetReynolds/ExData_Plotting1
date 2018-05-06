library(data.table)
library(dplyr)
library(graphics)

# Function to read household energy usage data over a period of Feb 1st and 2nd
# 2007. Returns a data frame
loadPowerConsumptionData <- function () {
  dataDir <- "./data"
  zipFile <- "./data/household_power_consumption.zip"
  dataFile <- "./data/household_power_consumption.txt"
  # Create a directory named 'data' if not already present.
  # Download and extract the zipfile only once,
  # before reading the file into a data table
  if(!file.exists(dataDir)) {
    dir.create(dataDir)
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(url = fileUrl, destfile = zipFile)
    unzip(zipFile, exdir = dataDir)
  }
  data <- fread(dataFile, colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.strings = "?")
  
  # Select the data required for exploratory analysis
  feb1st2nd <- c(as.Date("2007-02-01", format="%Y-%m-%d"), as.Date("2007-02-02", format="%Y-%m-%d"))
  data <- data %>%
    # Filter the data to only select rows where the date is 2007-02-01 or 2007-02-02
    filter(as.Date(data$Date, format = "%d/%m/%Y") %in% feb1st2nd) %>%
    # add a new column 'DateTime' that stores the Date and Time in a combined form
    mutate(DateTime = as.POSIXct(strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")))
}

# Function to create a time series graph of Energy submetering usage based on
# data read from the household energy consumption datafile, and save it in a
# PNG file named plot3.png
plot3 <- function() {
  # Load data from the dataset file for exploratory analysis
  powerConsumption <- loadPowerConsumptionData()
  
  # Draw the plot on a PNG file named plot3.png in the current working dir,
  # with width and height of 480 px, and a white background.
  png(filename = "plot3.png", width = 480, height = 480, bg = "white")
  
  # Compute the ranges for the date time and submetering values. This is to
  # set the axes correctly when drawing the graph.
  xrange <- with(powerConsumption, range(DateTime))
  yrange <- with(powerConsumption, range(Sub_metering_1, Sub_metering_2, Sub_metering_3))
  
  # Plot a line graph of the Energy submetering vs time. Set the label for 
  # y-axis. To do this, first plot previously computed 'xrange' vs 'yrange'
  # to create the graph with axes and title, but without plotting any lines
  # (using type=n). Then, plot the individual submetering sets in different
  # colours.
  with(powerConsumption, plot(xrange , yrange, type = "n", xlab="", ylab= "Energy sub metering"))
  with(powerConsumption, lines(Sub_metering_1 ~ DateTime, col = "black"))
  with(powerConsumption, lines(Sub_metering_2 ~ DateTime, col = "red"))
  with(powerConsumption, lines(Sub_metering_3 ~ DateTime, col = "blue"))
  
  # Add a legend, setting the labels and colors as appropriate
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue"), lwd = 1)
  
  # Remember to close the device
  dev.off()
}

plot3()
