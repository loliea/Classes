library(dplyr)
# Read the data from the text file replacing the "?" with na
dataset_raw <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = TRUE, na.strings = "?")
# Subset from the initial dataset to keep onlu Feb 1 2007 and Feb 2 2007
dataset <- filter(dataset_raw, Date %in% c("1/2/2007", "2/2/2007"))
# Combine Date and Time to make a field datetime format
dataset <- mutate(dataset, Date.Time = paste(Date, Time, sep = "-"))
#Transform the Date, Time and Date.Time from character to POSIXlt
dataset <- transform(dataset, Date = strptime(Date, "%d/%m/%Y" , "Europe/Paris"), 
                     Time = strptime(Time, "%H:%M:%S", "Europe/Paris"),
                     Date.Time = strptime(Date.Time, "%d/%m/%Y-%H:%M:%S", "Europe/Paris"))

#Initialize the interface
dev.off()
dev.new()
#Save the initial layout to be able to reset it if needed
opar = par()

#Run the four graphs:
source("plot1.R")
source("plot2.R")
source("plot3.R")
source("plot4.R")
#reset the layout to original
suppressWarnings(par(opar))
