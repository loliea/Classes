#Plot4
par(mfrow=c(2,2))
#Graph in first quadran
plot(x = dataset$Date.Time, y = dataset$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (killowatts)")
#Graph in second quadran
plot(x = dataset$Date.Time, y = dataset$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
#Graph in third quadran
plot(x = dataset$Date.Time, y = dataset$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
lines(x = dataset$Date.Time, y = dataset$Sub_metering_2, col = "red")
lines(x = dataset$Date.Time, y = dataset$Sub_metering_3, col = "blue")
legend(x = "topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, col = c("black", "red", "blue"), bty="n")
#Graph in fourth quadran
plot(x = dataset$Date.Time, y = dataset$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power", ylim = c(0, 0.5))