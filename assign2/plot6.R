# plot6 creates line plot of annual motor vehicle emissions 
# in Los Angeles, California and Baltimore, Maryland

plot6 <- function() {
  
  getdata()
  
  # Select only motor vehicle sources 
  vehicleData <- filterdata("Mobile - On-Road.*")
  
  # Select motor vehicle emissions data for Baltimore, MD 
  baltVehicleData <- filter(vehicleData, str_detect(fips, "24510"))

  # Select motor vehicle emissions data for Los Angeles, CA
  laVehicleData <- filter(vehicleData,str_detect(fips, "06037"))
  
  # Clean data: remove unecessary columns, shorten descriptions, and sum total emissions for Baltimore, MD
  cleanBaltData <- cleandata(baltVehicleData,replaceString1 = "Mobile - On-Road ", replaceString2 = "Vehicles", groupingCol = "EI.Sector") %>%
    group_by(EI.Sector, year, Total.Emissions) %>% 
    summarize(city="Baltimore, MD") %>%
    select(year, EI.Sector, Total.Emissions, city) 
  
  # Clean data: remove unecessary columns, shorten descriptions, and sum total emissions for Los Angeles, CA
  cleanLaData <- cleandata2(laVehicleData,replaceString1 = "Mobile - On-Road ", replaceString2 = "Vehicles", groupingCol = "EI.Sector") %>%
    group_by(EI.Sector, year, Total.Emissions) %>% 
    summarize(city="Los Angeles, CA") %>%
    select(year, EI.Sector, Total.Emissions, city) 
  
  # Join clean data for both cities into one data frame
  joinedData <- rbind(cleanBaltData, cleanLaData)
  names(joinedData) <- c("year","EI.Sector","Total.Emissions","city")
  
  # Create final data frame, to be used for plotting and easy comparison
  finalData <- totals(joinedData,groupBy="city")
  
  # Make line plot of yearly emissions for motor vehicle sources by sector and city 
  g <- ggplot(finalData,aes(year,Total.Emissions))
  # ggplot pieces: plot + plot in panels by city + color dots/lines by EI.Sector + labels + make x axis labels vertical
  g + facet_wrap(~city) +  geom_point(aes(color=EI.Sector)) + geom_line(aes(color=EI.Sector)) + labs(title="Annual Emissions for Motor Vehicle Sources", x="Year", y="Emissions (tons)") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Save above plot
  ggsave("plot6.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}