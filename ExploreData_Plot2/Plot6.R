## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?

# Download the data, unzip it and save it under the two datasets NEI and SCC
data.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(data.url, destfile ="NEI_data.zip", method ="curl")
unzip("NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge the following two datasets:
#     - Fitler NEI to keep fips 24510 and 06037 which are Baltimore and LA
#     - Filter SCC to keep only the category on road which represents motor vehicules (14 entries which SCC.Level.Three looks like motor vehicules)
NEI.Balt.LA.Motor.merge <- merge(NEI[NEI$fips %in% c("24510", "06037"), ], 
                              SCC[SCC$Data.Category == "Onroad",], 
                              by.y = 'SCC')

library(dplyr)
library(ggplot2)

# Summarize the data using the variables we will need for the plot
NEI.Balt.LA.Motor <- summarise(group_by(NEI.Balt.LA.Motor.merge, year, fips), Total.PM2.5 = sum(Emissions))
# Change fips to factors with nice labels
NEI.Balt.LA.Motor$fips <- factor(NEI.Balt.LA.Motor$fips, levels = c("24510", "06037"), labels = c("Baltimore", "LA"))

# Plot comparing total PM2.5 for Baltimore and LA
par(mar=c(5.1, 5.1, 4.1, 2.1)) #set margins - originaly 5.1 4.1 4.1 2.1
q1 <- ggplot(NEI.Balt.LA.Motor, aes(year, Total.PM2.5, label = formatC(Total.PM2.5, format = "d", big.mark = ",")))
q2 <- q1 + geom_point(size = 4, alpha = 0.75) + geom_text(vjust = -1, size = 3) + geom_smooth(method = lm, se = FALSE) + facet_grid(.~fips)
# Go over the board to annotate teh graphs with emission variance - used https://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/
q3 <- q2 + geom_text(aes(x = x, y= y, label = labs, group = NULL), 
                data = data.frame(x = c(2004, 2004), y = c(800, 3700), fips = factor(c("Baltimore", "LA"),levels = c("Baltimore", "LA")) , 
                                  labs = c("Decrease of emissions of 258.5 tons", "Increase of emissions of 170.2 tons")), size = 4)
q3 + labs(x = "Years", y = expression("Total "*PM[2.5]* " (in Ton)"), title = expression("Total "*PM[2.5]* " in Baltimore and LA for Motor Vehicules"))
# Copy the graph to a PNG device and close it
dev.copy(png, file = "plot6.png", width = 750, height = 480)
dev.off()

## ---------------------------------------------------------------------------------------------------
## Beyond scope of project ... What has been driving the increase of PM2.5 in LA?
# Second plot breaking down the graph by SCC Level Three to identify in LA the source of polution that is driving the increase of PM2.5
# Create a new aggregate keeping only the fips LA and aggregating by year and SCC Level Three
NEI.LA.Motor.SCC3 <- summarise(group_by(NEI.Balt.LA.Motor.merge[NEI.Balt.LA.Motor.merge$fips == "06037",], year, SCC.Level.Three), Total.PM2.5 = sum(Emissions))
# Plot the aggregate using for facet the variable SCC Level Three
q1.1 <- ggplot(NEI.LA.Motor.SCC3, aes(year, Total.PM2.5)) + geom_point(size = 4, alpha = 0.75)
q2.1 <- q1.1 + geom_smooth(method = lm, se = FALSE) + facet_wrap( ~ SCC.Level.Three, nrow = 3, ncol = 4)
q2.1 + labs(x = "Years", y = expression("Total "*PM[2.5]* " (in Ton)"), title = expression("Total "*PM[2.5]* " in LA for Motor Vehicules by SCC Three"))
# ==> we see that the type of on road motor vehicule that is mainly driving the increase in PM 2.5 is "Heavy Duty Diesel Vehicles (HDDV) Class 8A & 8B" (highest level of polution + sharpest increase)

# Now removing the "Heavy Duty Diesel Vehicles (HDDV) Class 8A & 8B" in LA let's compare again PM2.5 of Baltimore vs. LA for motor vehicules
# Create new aggregate removing the SCC Level Three "Heavy Duty Diesel Vehicles (HDDV) Class 8A & 8B" from LA data points
NEI.Balt.LA.Motor.altered <- summarise(
  group_by(
    NEI.Balt.LA.Motor.merge[!(NEI.Balt.LA.Motor.merge$fips == "06037" & NEI.Balt.LA.Motor.merge$SCC.Level.Three == "Heavy Duty Diesel Vehicles (HDDV) Class 8A & 8B"),],
    year, fips), Total.PM2.5 = sum(Emissions))
NEI.Balt.LA.Motor.altered$fips <- factor(NEI.Balt.LA.Motor.altered$fips, levels = c("24510", "06037"), labels = c("Baltimore", "LA"))
q1.2 <- ggplot(NEI.Balt.LA.Motor.altered, aes(year, Total.PM2.5)) + geom_point(size = 4, alpha = 0.75)
q2.2 <- q1.2 + geom_smooth(method = lm, se = FALSE) + facet_grid(.~fips)
q2.2 + labs(x = "Years", y = expression("Total "*PM[2.5]* " (in Ton)"), title = expression("Total "*PM[2.5]* " in Baltimore and LA for Motor Vehicules"))
# ==> We see that even of PM2.5 in LA is still above Baltimore (which is normal based on the population difference) but LA sharply reduced emission of PM2.5 if we exclude
# "Heavy Duty Diesel Vehicles (HDDV) Class 8A & 8B" from the type of motor vehicules
