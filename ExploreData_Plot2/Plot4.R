## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# Download the data, unzip it and save it under the two datasets NEI and SCC
data.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(data.url, destfile ="NEI_data.zip", method ="curl")
unzip("NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Filter SCC to keep only the rows where short name contains coal and comb
SCC.Comb.Coal <- SCC[grepl("Coal", SCC$Short.Name)&grepl("Comb", SCC$Short.Name),]

# Merge the two datasets to capture the labels of SCC keeping only the first 4 columns of the dataset SCC
NEI.Comb.Coal <- merge(NEI, SCC.Comb.Coal[,c(1,3)], by.y = 'SCC')
# Create total by year
library(dplyr)
NEI.Comb.Coal.Total <- summarise(group_by(NEI.Comb.Coal, year), Total.PM2.5 = sum(Emissions))

# Plot the dataset across the years
par(mar=c(5.1, 5.1, 4.1, 2.1)) #set margins - originaly 5.1 4.1 4.1 2.1
plot(NEI.Comb.Coal.Total$year, NEI.Comb.Coal.Total$Total.PM2.5, xlab = "Years", 
     ylab = expression("Total "*PM[2.5]* " (in Ton)"), 
     pch = "o", col = "red", 
     ylim = c(350000, 650000),
     yaxt = "n",   #To be able to format the numbers pretty don't print the values of the y axis
     main = expression("Total "*PM[2.5]* " in the USA for Coal Combustion Related Sources - Years 1999 to 2008")
     )
axis(2, at = axTicks(2), labels=formatC(axTicks(2), format="d", big.mark=',')) #print pretty the values of y
linera.regression <- lm(Total.PM2.5 ~ year, NEI.Comb.Coal.Total)
abline(linera.regression, lwd = 2, col = "blue")
legend("topright", legend = c(expression("Total "*PM[2.5]), "Linear regression"), pch = c("o","_"), col = c("red", "blue"))
# Copy the graph to a PNG device and close it
dev.copy(png, file = "Plot4.png", width = 800, height = 480)
dev.off()