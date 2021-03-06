#################################################################################
#
#  Filename: plot1.R
#   Version: 1.0.1
#      Date: 2015.09.12
#    Author: Mariusz Musiał
# Rev. Info: 1.0.0 - initial version of the script
#            1.0.1 - changed filenames to lower case
#
# This script generates a histogram of Global Active Power from the data of
# Individual household electric power consumption Data Set available from
# UCI Machine Learning repository 
# https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
#
#         Input: household_power_consumption.txt 
#        Output: plot1.png file
#     Execution: run this script from your current working directory using 
#                source("plot1.R")
# Prerequisites: 1) this script is located in your current working directory
#                2) Input file is located in the current working directory
#                3) input file is ordered by date and time, ascending
#  Dependencies: None
#
#################################################################################


     # Some pseudo-constants declaration

startDateString <- "1/2/2007"
endDateString <- "3/2/2007"     # one day past the end date to analyze

fname <- "household_power_consumption.txt"
pfname <- "plot1.png"

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

numlines <- n - skip        # calculate number of lines to read in

cat(numlines, "lines to read...\n")

close(con)

     # Actual data frame reading, using parameters calculated above.

hpc <- read.table(fname, sep = ";", stringsAsFactors = F, skip = skip, nrows = numlines, col.names = header)

cat("Done reading data.\n")

     # Creating the histogram directly do PNG device

png(file = pfname, bg = "transparent")
hist(hpc$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
dev.off()

cat("Plot written to file", pfname, ".\n")
