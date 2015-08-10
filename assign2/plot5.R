# plot5 creates line plot of annual motor vehicle emissions in Baltimore, Maryland

plot5 <- function() {
  
  getdata()
  
  # Select only motor vehicle sources
  vehicleData <- filterdata("Mobile - On-Road.*")
  
  # Select motor vehicle emissions data for Baltimore, MD
  baltVehicleData <- filter(vehicleData, str_detect(fips, "24510"))
  
  # Clean data: remove unecessary columns, shorten descriptions, and sum total emissions for Baltimore, MD
  cleanData <- cleandata(baltVehicleData,replaceString1 = "Mobile - On-Road ", replaceString2 = "Vehicles", groupingCol = "EI.Sector")
  
  # Create final data frame with types of motor vehicle source emissions and total emissions for all motor vehicles
  finalData <- totals(cleanData)
  
  # Create line plot of annual emissions from each motor vehicle source and from all motor vehicles sources combined
  g <- ggplot(finalData,aes(year,Total.Emissions))
  g + geom_point(aes(color=EI.Sector)) + geom_line(aes(color=EI.Sector)) + labs(title="Annual Motor Vehicle Emissions in Baltimore, MD", x="Year", y="Emissions (tons)")
  
  # Save above plot
  ggsave("plot5.png")
}
