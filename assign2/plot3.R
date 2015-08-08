# plot3 creates line plot of annual emissions from 4 different source types
# in the city of Baltimore, Maryland

plot3 <- function() {
  
  getdata()
  
  # Create subset of NEI data for city of Baltimore, Maryland (fips=24510)
  baltData <- subset(NEI,NEI$fips=="24510")
  
  # Sort and modify balt_data for plotting
  cleanData <- baltData %>%
    # 1. Specify columns used for grouping
    group_by(type, year) %>% 
    # 2. Sum all emissions for each year and type
    summarize(Total.Emissions = sum(Emissions)) %>%
    # 3. Select columns to include in final data frame
    select(year, type, Total.Emissions) 
  
  # Plot emissions per year and type of source
  g <- ggplot(cleanData,aes(year,Total.Emissions))
  g + geom_point(aes(color=type)) + geom_line(aes(color=type)) + labs(title="Annual PM-2.5 Emissions per Source Type in Baltimore,MD", x="Year", y="Emissions (tons)") 
  
  # Save the above plot
  ggsave("plot3.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}