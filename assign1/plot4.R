plot4 <- function() {
  
  dat <- getdata()
  
  # Convert date and time columns to one date-time string
  datetime <- strptime(paste(dat$Date,dat$Time),format="%d/%m/%Y %H:%M:%S")
  
  # Open PNG device and make a png file for plot below
  png("plot4.png",width=480,height=480)
  
  # Force 2rows x 2cols of plots and set plotting margin
  par(mfrow = c(2, 2), mar = c(4, 5, 2, 2))
  
  # Plot global active power vs. datetime
  plot(datetime,dat$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)")
  
  # Plot voltage vs. datetime
  plot(datetime,dat$Voltage,type="l",xlab="datetime",ylab="Voltage")
  
  # Plot energy submetering vs. datetime
  plot(datetime,dat$Sub_metering_1,type="l", xlab="", ylab="Energy sub metering")
  lines(datetime,dat$Sub_metering_2,col="red")
  lines(datetime,dat$Sub_metering_3,col="blue")
  legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=c(1,1,1),lwd=(rep(2,3)),cex=0.8)
  
  # Plot global reactive power vs. datetime
  plot(datetime,dat$Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power")
  
  # Close png device
  dev.off()  
}