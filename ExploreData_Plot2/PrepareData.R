# Download the data, unzip it and save it under two datasets NEI and SCC
data.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(data.url, destfile ="NEI_data.zip", method ="curl")
unzip("NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Merge the two datasets to capture the labels of SCC keeping only the first 4 columns of the dataset SCC
NEI.all <- merge(NEI, SCC[,c(1,2,3,4)], by.y = 'SCC')
rm(NEI) # Free memory

