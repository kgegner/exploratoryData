plot2 <- function() {
  
  dat <- getdata()
  
  # Convert date and time columns to one date-time string
  datetime <- strptime(paste(dat$Date,dat$Time),format="%d/%m/%Y %H:%M:%S")
  
  # Open PNG device and make a png file for plot below
  png("plot2.png",width=480,height=480)
  
  # Set margins for plotting
  par(mar=c(3,5,2,2))
  
  # Make line plot of global active power versus time
  plot(datetime,dat$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)")

  # Close png device
  dev.off()  
}