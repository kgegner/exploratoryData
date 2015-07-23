plot3 <- function() {
  
  # Get NEI cached data, will be NULL if NEI data not yet loaded
  NEI <- dataCache$getNEI()
  
  # NEI data hasn't been loaded yet, so load it, and assign to NEI data frame
  if(is.null(NEI)){
    dataCache$loadNEI()
    NEI <- dataCache$getNEI()
  }
  
  # Create subset of NEI data for city of Baltimore, Maryland (fips=24510)
  balt_data <- subset(NEI,NEI$fips=="24510")
  
  # Sort and modify balt_data for plotting
  clean_data <- balt_data %>%
    # 1. Specify columns used for grouping
    group_by(type, year) %>% 
    # 2. Sum all emissions for each year and type
    summarize(Total.Emissions = sum(Emissions)) %>%
    # 3. Select columns to include in final data frame
    select(year, type, Total.Emissions) 
  
  # Plot emissions per year and type of source
  g <- ggplot(clean_data,aes(year,Total.Emissions))
  g + geom_point(color="navy") + facet_wrap(~type) + geom_smooth(color="olivedrab",method="lm",se=FALSE) + labs(title="Annual PM-2.5 Emissions per Source Type in Baltimore,MD", x="Year", y="Emissions (tons)") 
  
  # Save the above plot
  ggsave("plot3.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}