#################################################################################
#
#  Filename: Plot4.R
#   Version: 1.0.0
#      Date: 2015.09.09
#    Author: Mariusz Musiał
# Rev. Info: 1.0.0 - initial version of the script
#
# This script generates a plot with four subplots from the data of
# Individual household electric power consumption Data Set available from
# UCI Machine Learning repository 
# https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
#
#         Input: household_power_consumption.txt 
#        Output: Plot4.png file
#     Execution: run this script from your current working directory using 
#                source("Plot4.R")
# Prerequisites: 1) this script is located in your current working directory
#                2) Input file is located in the current working directory
#                3) input file is ordered by date and time, ascending
#  Dependencies: dplyr
#
#################################################################################

require(dplyr)

oldlocale <- Sys.getlocale(category = "LC_TIME")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Some pseudo-constants declaration

startDateString <- "1/2/2007"
endDateString <- "3/2/2007"     # one day past the end date to analyze

fname <- "household_power_consumption.txt"
pfname <- "Plot4.png"

# Data reading.
# Only the required time frame is read. To do this, first we calculate how many
# lines are to be skipped, then we calculate how many lines are to be read.
# This code works only if the data in the input file is ordered by date and time, ascending.
# Data reading code is common for all Plotx.R scripts, however it is not delegated
# to a separate function to ease script reading and analysis.
# This code is not very R-ish, maybe I'll find some more compact way to do this in
# the furure.

con <- file(fname, "r")     # Initialize the connection
n <- 0                      # Initialize the counter
line <- readLines(con, 1)

# The by-product of reading the first line is a vector of column names
header <- strsplit(line, ";")[[1]]

cat("Analyzing data file...\n")

while(!grepl(startDateString, line)) {
    n <- n + 1
    if(length(line <- readLines(con, 1)) == 0) # Protection against end of file
        break
}

cat(n, "lines to skip...\n")

skip <- n                   # remember number of lines to skip

# We continue with the line read above, the one which matched the start date
# We re-check it here, so that the counter is correctly incremented and no tweaking
# is necessary at calculating numlines

while(!grepl(endDateString, line)) {
    n <- n + 1
    if(length(line <- readLines(con, 1)) == 0) # Protection against end of file
        break
}

numlines <- n - skip       # calculate number of lines to read in

cat(numlines, "lines to read...\n")

close(con)

# Actual data frame reading, using parameters calculated above.

hpc <- read.table(fname, sep = ";", stringsAsFactors = F, skip = skip, nrows = numlines, col.names = header)

cat("Done reading data.\n")

hpc <- hpc %>% mutate(Date = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %T")) %>% select(-Time)

# Creating the histogram directly do PNG device

png(file = pfname, bg = "transparent")
par(mfrow = c(2,2))
# top left
with(hpc, plot(Date, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power"))
# top right
with(hpc, plot(Date, Voltage, type = "l", xlab = "datetime"))
# bottom left
with(hpc, plot(Date, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering"))
with(hpc, points(Date, Sub_metering_2, type = "l", col = "red"))
with(hpc, points(Date, Sub_metering_3, type = "l", col = "blue"))
legend("topright", c(names(hpc)[6:8]), col = c("black", "red", "blue"), lty = c(1,1,1), bty = "n")
# bottom right
with(hpc, plot(Date, Global_reactive_power, type = "l", xlab = "datetime"))
dev.off()
par(mfrow = c(1,1))


cat("Plot written to file", pfname, ".\n")

Sys.setlocale("LC_TIME", oldlocale)
