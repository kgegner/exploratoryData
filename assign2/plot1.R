# plot1 - creates scatterplot with annual emissions from all PM-2.5 polluting sources

plot1 <- function() {
 
  getdata()
  
  # Sum emissions from all sources for each year
  yrlyEmissions <- aggregate(NEI$Emissions ~ NEI$year, data=NEI, sum)
  names(yrlyEmissions) <- c("Year","Emissions")
  
  # Open PNG device and make a png file for plot below
  png("plot1.png",width=480,height=480)
  
  # Set margins for plotting
  par(mar=c(5,5,4,2))
  
  # Create linear regression model of yearly emissions
  lmfit <- lm(yrlyEmissions$Emissions ~ yrlyEmissions$Year)
  
  # Plot total emissions per year, with linear regression model
  plot(yrlyEmissions$Year, yrlyEmissions$Emissions,
       main="Annual PM-2.5 Emissions (all sources)",
       xlab="Year", ylab="Emissions (tons)")
  abline(lmfit)
  
  # Close png device
  dev.off()
}