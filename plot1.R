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

# Function to create a histogram of Global active power based on data read from
# the household energy consumption datafile, and save it in a PNG file named
# plot1.png
plot1 <- function() {
  # Load data from the dataset file for exploratory analysis
  powerConsumption <- loadPowerConsumptionData()
  
  # Draw the plot on a PNG file named plot1.png in the current working dir,
  # with width and height of 480 px, and a white background.
  png(filename = "plot1.png", width = 480, height = 480, bg = "white")
  
  # Plot a histogram of Global active power values, and set the label for x-axis, and the title of the plot.
  with(powerConsumption, hist(Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power"))
  dev.off()
}

plot1()