plot3 <- function() {
  
  dat <- getdata()
  
  # Convert date and time columns to one date-time string
  datetime <- strptime(paste(dat$Date,dat$Time),format="%d/%m/%Y %H:%M:%S")
  
  # Open PNG device and make a png file for plot below
  png("plot3.png",width=480,height=480)
  
  # Set margins for plotting
  par(mar=c(3,5,2,2))
  
  # Make line plot with 3 lines for energy sub metering 1,2, and 3
  plot(datetime,dat$Sub_metering_1,type="l", xlab="", ylab="Energy sub metering")
  lines(datetime,dat$Sub_metering_2,col="red")
  lines(datetime,dat$Sub_metering_3,col="blue")
  legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=c(1,1,1),lwd=(rep(2,3)))

  # Close png device
  dev.off()    
}