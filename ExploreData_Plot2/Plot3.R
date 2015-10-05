## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008?
## Use the ggplot2 plotting system to make a plot answer this question.

# Download the data, unzip it and save it under the dataset NEI
data.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(data.url, destfile ="NEI_data.zip", method ="curl")
unzip("NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")

# Load the library dplyr to filter on Baltimore and keep the dates from 1999 t9 2008
library(dplyr)
NEI.Balt.Type.PM2.5 <- summarise(group_by(filter(NEI, year %in% c(1999:2008) & fips == "24510"), type, year), Total.PM2.5 = sum(Emissions))

# ggplot
library(ggplot2)
par(mar=c(5.1, 5.1, 4.1, 2.1)) #set margins - originaly 5.1 4.1 4.1 2.1
qplot(year, Total.PM2.5, data = NEI.Balt.Type.PM2.5, 
      geom = c("point", "smooth"), 
      method = "lm", se = FALSE, 
      facets = . ~ type,
      main = expression(PM[2.5]*" in Baltimore from 1999 to 2008 by type"),
      xlab = "Years",
      ylab = expression(PM[2.5]*" (in Ton))"))
# Copy the graph to a PNG device and close it
dev.copy(png, file = "plot3.png", width = 750, height = 480)
dev.off()