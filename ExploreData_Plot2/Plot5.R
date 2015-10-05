## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City

# Download the data, unzip it and save it under the two datasets NEI and SCC
data.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(data.url, destfile ="NEI_data.zip", method ="curl")
unzip("NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge the following two datasets:
#     - Fitler NEI to keep years 1999 to 2008 and fips 24510 which is Baltimore
#     - Filter SCC to keep only the category on road which represents motor vehicules (14 entries which SCC.Level.Three looks like motor vehicules)
NEI.Balt.Motor.merge <- merge(NEI[NEI$year %in% c(1999:2008) & NEI$fips == "24510", ], 
                       SCC[SCC$Data.Category == "Onroad",], 
                       by.y = 'SCC')

# Summarize the dataset keeping year and summing emmissions to get PM2.5 for motor vhicules in Baltimore
library(dplyr)
NEI.Balt.Motor <- summarise(group_by(NEI.Balt.Motor.merge, year), Total.PM2.5 = sum(Emissions))

par(mar=c(5.1, 5.1, 4.1, 2.1)) #set margins - originaly 5.1 4.1 4.1 2.1
plot(NEI.Balt.Motor, xlab = "Years", ylab = expression("Total "*PM[2.5]* " (in Ton)"), pch = "o", col = "red", main = expression("Total "*PM[2.5]* " in Baltimore for Motor Vehicules for the Years 1999 to 2008"))
linera.regression <- lm(Total.PM2.5 ~ year, NEI.Balt.Motor)
abline(linera.regression, lwd = 2, col = "blue")
legend("topright", legend = c(expression("Baltimore Motor Vehicules "*PM[2.5]), "Linear regression"), pch = c("o","_"), col = c("red", "blue"))
# Copy the graph to a PNG device and close it
dev.copy(png, file = "plot5.png", width = 750, height = 480)
dev.off()