#Plot3
#Start with initializing the plot for submeter 1
plot(x = dataset$Date.Time, y = dataset$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
#Ad the plot for submeter 2
lines(x = dataset$Date.Time, y = dataset$Sub_metering_2, col = "red")
#Ad the plot for submeter 2
lines(x = dataset$Date.Time, y = dataset$Sub_metering_3, col = "blue")
#Finaly ad the legend
legend(x = "topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, col = c("black", "red", "blue"))