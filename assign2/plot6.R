plot6 <- function() {
  
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
  
  # Select vehicle source data for Los Angeles, CA
  la_veh_data <- filter(veh_data, str_detect(fips, "06037"))
  
  # Sort and modify balt_veh_data for plotting
  clean_balt_data <- balt_veh_data %>% 
    # 1. Clean up EI.Sector descriptions
    mutate(EI.Sector = str_replace_all(EI.Sector, "Mobile - On-Road ", "")) %>%
    mutate(EI.Sector = str_replace_all(EI.Sector, "Vehicles", "")) %>%
    # 2. Specify columns used for grouping
    group_by(EI.Sector, year) %>% 
    # 3. Sum all emissions for each year and sector and add city column
    summarize(Total.Emissions = sum(Emissions),city="Baltimore, MD") %>%
    # 4. Select columns to include in final data frame
    select(year, EI.Sector, Total.Emissions,city) 
  
  # Sort and modify la_veh_data for plotting
  clean_la_data <- la_veh_data %>% 
    # 1. Clean up EI.Sector descriptions
    mutate(EI.Sector = str_replace_all(EI.Sector, "Mobile - On-Road ", "")) %>%
    mutate(EI.Sector = str_replace_all(EI.Sector, "Vehicles", "")) %>%
    # 2. Specify columns used for grouping
    group_by(EI.Sector, year) %>% 
    # 3. Sum all emissions for each year and sector and add city column
    summarize(Total.Emissions = sum(Emissions),city="Los Angeles, CA") %>%
    # 4. Select columns to include in final data frame
    select(year, EI.Sector, Total.Emissions,city) 
  
  # Join clean data for both cities into one data frame
  final_data <- rbind(clean_balt_data, clean_la_data)
  names(final_data) <- c("year","EI.Sector","Total.Emissions","city")
  
  # Plot yearly emissions for motor vehicle sources by sector and city 
  g <- ggplot(final_data,aes(year,Total.Emissions))
  # ggplot pieces: plot + color dots by city + put legend at bottom + plot in panels by sector and let y axis adjust for each city's emissions + labels + make x axis labels vertical
  g + geom_point(aes(color=city)) + theme(legend.position="none") + facet_grid(city~ EI.Sector,scales="free_y") + labs(title="Annual Emissions for Motor Vehicle Sources", x="Year", y="Emissions (tons)") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Save above plot
  ggsave("plot6.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}