## Q: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Download the data, unzip it and save it under the dataset NEI
data.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(data.url, destfile ="NEI_data.zip", method ="curl")
unzip("NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")

# Load the library dplyr to filter the dataset and create the aggregate
library(dplyr)
NEI.TotalPM2.5 <- summarise(group_by(filter(NEI, year %in% c(1999, 2002, 2005, 2008)), year), Total.PM2.5 = sum(Emissions))

# Plot the data including linear regression to highlight the trend
par(mar=c(5.1, 5.1, 4.1, 2.1)) #set margins - originaly 5.1 4.1 4.1 2.1
plot(NEI.TotalPM2.5, xlab = "Years", ylab = expression("Total "*PM[2.5]* " (in Ton)"), 
     pch = "o", col = "red", 
     main = expression("Total "*PM[2.5]* " in the US for the years 1999, 2002, 2005 and 2008"))
linera.regression <- lm(Total.PM2.5 ~ year, NEI.TotalPM2.5)
abline(linera.regression, lwd = 2, col = "blue")
legend("topright", legend = c(expression("Total "*PM[2.5]), "Linear regression"),
       pch = c("o","_"), col = c("red", "blue"))
# Copy the graph to a PNG device and close it
dev.copy(png, file = "Plot1.png", width = 750, height = 480)
dev.off()
