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

# Function to create a combine 4 plots into a single one - 
# * global active power over time,
# * voltage over time,
# * energy submetering usage over time,
# * global reactive power over time,
# and save it in a PNG file named plot4.png
plot4 <- function() {
  # Load data from the dataset file for exploratory analysis
  powerConsumption <- loadPowerConsumptionData()
  
  # Draw the plot on a PNG file named plot4.png in the current working dir,
  # with width and height of 480 px, and a white background.
  png(filename = "plot4.png", width = 480, height = 480, bg = "white")
  
  # Draw the plots in a 2 x 2 matrix
  par(mfrow = c(2,2))
  
  # Create the plots one after the other into the device
  plotGlobalActivePowerVsTime(powerConsumption)
  plotVoltageVsTime(powerConsumption)
  plotEnergySubmeteringVsTime(powerConsumption)
  plotGlobalReactivePowerVsTime(powerConsumption)
  
  # Remember to close the device
  dev.off()
}

# Function to create a time series graph of Global Active Power based on data
# read from the household energy consumption datafile
plotGlobalActivePowerVsTime <- function(powerConsumption) {
  # Plot a line graph of the Global active power vs time.
  # Set the label for y-axis.
  with(powerConsumption, plot(Global_active_power ~ DateTime, type = "l", ylab= "Global Active Power", xlab=""))
}

# Function to create a time series graph of Voltage based on data read from the
# household energy consumption datafile
plotVoltageVsTime <- function(powerConsumption) {
  # Plot a line graph of the Voltage vs time.
  with(powerConsumption, plot(Voltage ~ DateTime, type = "l", xlab= "datetime", ylab="Voltage"))
}

# Function to create a time series graph of Energy submetering usage based on
# data read from the household energy consumption datafile
plotEnergySubmeteringVsTime <- function(powerConsumption) {
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
  legend("topright",
         lwd = 1,     # Display the lines for subsets
         box.lwd = 0, # Do not draw a box around the legend
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue"))
}

# Function to create a time series graph of Global reactive power based on data
# read from the household energy consumption datafile
plotGlobalReactivePowerVsTime <- function(powerConsumption) {
  # Plot a line graph of the Global reactive power vs time.
  with(powerConsumption, plot(Global_reactive_power ~ DateTime, type = "l", xlab= "datetime", ylab="Global_reactive_power"))
}

plot4()