# plot2 creates scatterplot of annual emissions from all PM-2.5 polluting sources
# in the city of Baltimore, Maryland

plot2 <- function() {
  
  getdata()
  
  # Create subset of NEI data for city of Baltimore, Maryland (fips=24510)
  baltData <- subset(NEI,NEI$fips=="24510")
  
  # Sum Baltimore, MD's emissions from all sources for each year
  baltYrlyEmiss <- aggregate(baltData$Emissions ~ baltData$year, data=baltData, sum)
  names(baltYrlyEmiss) <- c("Year","Emissions")
  
  # Open PNG device and make a png file for plot below
  png("plot2.png",width=480,height=480)
  
  # Set margins for plotting
  par(mar=c(5,5,4,2))
  
  # Create linear regression model of Baltimore, MD yearly emissions
  lmfit <- lm(baltYrlyEmiss$Emissions ~ baltYrlyEmiss$Year)
  
  # Plot Baltimore, MD's total emissions per year, with linear regression model
  plot(baltYrlyEmiss$Year,baltYrlyEmiss$Emissions,
       main="Annual PM-2.5 Emissions (all sources) in Baltimore, Maryland",
       xlab="Year", ylab="Emissions (tons)")
  abline(lmfit)
  
  # Close png device
  dev.off()
  
}