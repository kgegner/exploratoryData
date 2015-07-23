plot5 <- function() {

  # Get NEI cached data, will be NULL if NEI data not yet loaded
  NEI <- dataCache$getNEI()
  
  # NEI data hasn't been loaded yet, so load it, and assign to NEI data frame
  if(is.null(NEI)){
    dataCache$loadNEI()
    NEI <- dataCache$getNEI()
  }
  
  # Get SCC cached data, will be NULL if SCC data not yet loaded
  SCC <<- dataCache$getSCC()
  
  # SCC data hasn't been loaded yet, so load it, and assign to SCC data frame
  if(is.null(SCC)){
    dataCache$loadSCC()
    SCC <<- dataCache$getSCC()
  }
  
  # Select only motor vehicle sources from SCC data
  SCC_vehicles <- filter(SCC, str_detect(EI.Sector, "Mobile - On-Road.*"))
  
  # Save classification codes for all vehicle sources listed in EI.Sector
  SCC_vehicle_vals <- as.character(unique(SCC_vehicles$SCC))
  
  # Join NEI and SCC into one data frame for all motor vehicle sources
  veh_data <- inner_join(NEI, SCC_vehicles, by="SCC") %>% 
    filter(SCC %in% SCC_vehicle_vals)
  
  # Select vehicle source data for Baltimore, MD 
  balt_veh_data <- filter(veh_data, str_detect(fips, "24510"))
  
  # Sort and modify balt_veh_data for plotting
  cleandata <- balt_veh_data %>% 
    # 1. Clean up EI.Sector descriptions
    mutate(EI.Sector = str_replace_all(EI.Sector, "Mobile - On-Road ", "")) %>%
    # 2. Specify columns used for grouping
    group_by(EI.Sector, year) %>% 
    # 3. Sum all emissions for each year and sector
    summarize(Total.Emissions = sum(Emissions)) %>%
    # 4. Select columns to include in final data frame
    select(year, EI.Sector, Total.Emissions) 
  
  # Plot yearly emissions for motor vehicle sources by sector
  g <- ggplot(cleandata,aes(year,Total.Emissions))
  g + geom_point(color="navy") + facet_wrap(~EI.Sector) + geom_smooth(color="olivedrab",method="lm",se=FALSE) + labs(title="Annual Emissions for Motor Vehicle Sources", x="Year", y="Emissions (tons)")
  
  # Save above plot
  ggsave("plot5.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}