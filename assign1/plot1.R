plot1 <- function() {
  
  dat <- getdata()
  
  # Set margins for plotting
  par(mar=c(5,5,2,2))
  
  # Open PNG device and make a png file for histogram below
  png("plot1.png",width=480,height=480)
  
  # Make histogram of global active power
  hist(dat$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab=" Frequency")
  
  # Close png device
  dev.off()
}