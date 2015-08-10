# plot4 creates line plot of annual emissions from coal sources

plot4 <- function() {
  
  getdata()
  
  # Filter data to only include coal related sources
  coalData <- filterdata("Coal")
  
  # Shorten EI.Sector descriptions, sum emmisions for each coal source, and get rid of extra columns
  cleanData <- cleandata(filteredData=coalData,replaceString1 = "Fuel Comb - ", replaceString2 = "- Coal", groupingCol = "EI.Sector", emissionsDivisor=1000)
  
  # Create final data frame with types of coal source emissions and total emissions for all coal sources
  finalData <- totals(cleanData)
  
  # Create line plot of annual emissions for coal sources by sector
  g <- ggplot(finalData,aes(year,Total.Emissions))
  g + geom_point(aes(color=EI.Sector)) + geom_point(aes(color=EI.Sector)) + geom_line(aes(color=EI.Sector)) + labs(title="Annual PM-2.5 Emissions for Coal Sources", x="Year", y="Emissions (thousands of tons)")
  
  # Save above plot
  ggsave("plot4.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}
