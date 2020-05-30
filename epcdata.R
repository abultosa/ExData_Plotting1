# Load and clean the data table
epcdata <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", colClasses = c('character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))

# change the date type
epcdata$Date <- as.Date(epcdata$Date, "%d/%m/%Y")

# Filter data according to the date given instruction
epcdata <- subset(epcdata,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

# Removal of incomplete data observation
epcdata <- epcdata[complete.cases(epcdata),]

#Join Date and Time column
dateTime <- paste(epcdata$Date, epcdata$Time)

# Name the vector
dateTime <- setNames(dateTime, "DateTime")

# Remove Date and Time column
epcdata <- epcdata[ ,!(names(epcdata) %in% c("Date","Time"))]

# Add DateTime column
epcdata <- cbind(dateTime, epcdata)

# Format dateTime Column
epcdata$dateTime <- as.POSIXct(dateTime)

# Create the histogram
hist(epcdata$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

#Save file and close device
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

# Create Plot 2
plot(epcdata$Global_active_power~epcdata$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#save plot 2
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

#Create plot 3
with(epcdata, {
    plot(Sub_metering_1~dateTime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# Save plot 3 and close device
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

#Create plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(epcdata, {
    plot(Global_active_power~dateTime, type="l", 
         ylab="Global Active Power (kilowatts)", xlab="")
    plot(Voltage~dateTime, type="l", 
         ylab="Voltage (volt)", xlab="")
    plot(Sub_metering_1~dateTime, type="l", 
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    plot(Global_reactive_power~dateTime, type="l", 
         ylab="Global Rective Power (kilowatts)",xlab="")
})

#Save plot 4 and close device
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()