plot2 <- function() {
  
  # Get NEI cached data, will be NULL if NEI data not yet loaded
  NEI <- dataCache$getNEI()
  
  # NEI data hasn't been loaded yet, so load it, and create NEI data frame
  if(is.null(NEI)){
    dataCache$loadNEI()
    NEI <- dataCache$getNEI()
  }
  
  # Create subset of NEI data for city of Baltimore, Maryland (fips=24510)
  balt_dat <- subset(NEI,NEI$fips=="24510")
  
  # Sum Baltimore, MD's emissions from all sources for each year
  balt_yrly_emiss <- aggregate(balt_dat$Emissions ~ balt_dat$year, data=balt_dat,sum)
  names(balt_yrly_emiss) <- c("Year","Emissions")
  
  # Open PNG device and make a png file for plot below
  png("plot2.png",width=480,height=480)
  
  # Set margins for plotting
  par(mar=c(5,5,4,2))
  
  # Create linear regression model of Baltimore, MD yearly emissions
  lmfit <- lm(balt_yrly_emiss$Emissions ~ balt_yrly_emiss$Year)
  
  # Plot Baltimore, MD's total emissions per year, with linear regression model
  plot(balt_yrly_emiss$Year,balt_yrly_emiss$Emissions,
       main="Annual PM-2.5 Emissions (all sources) in Baltimore, Maryland",
       xlab="Year", ylab="Emissions (tons)")
  abline(lmfit)
  
  # Close png device
  dev.off()
  
}