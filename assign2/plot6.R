plot6 <- function() {
  
  getdata()
  
  # Select only motor vehicle sources from SCC data
  SCC_vehicles <- filter(SCC, str_detect(EI.Sector, "Mobile - On-Road.*"))
  
  # Save classification codes for all vehicle sources listed in EI.Sector
  SCC_vehicle_vals <- as.character(unique(SCC_vehicles$SCC))
  
  # Join NEI and SCC into one data frame for all motor vehicle sources
  vehData <- inner_join(NEI, SCC_vehicles, by="SCC") %>% 
    filter(SCC %in% SCC_vehicle_vals)
  
  # Select vehicle source data for Baltimore, MD 
  baltVehData <- filter(vehData, str_detect(fips, "24510"))
  
  # Select vehicle source data for Los Angeles, CA
  laVehData <- filter(vehData, str_detect(fips, "06037"))
  
  # Sort and modify balt_veh_data for plotting
  cleanBaltData <- baltVehData %>% 
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
  cleanLaData <- laVehData %>% 
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
  joinedData <- rbind(cleanBaltData, cleanLaData)
  names(joinedData) <- c("year","EI.Sector","Total.Emissions","city")
  
  # Sum emissions for each year across all motor vehicle sources
  vehTotals <- joinedData %>%
    # 1. Specify columns for grouping
    group_by(year,city) %>%
    # 2. Sum emissions from each source type for each year
    summarize(Total.Emissions = sum(Total.Emissions), EI.Sector="Combined") %>%
    # 3. Select columns to include
    select(year,EI.Sector,Total.Emissions,city)
  
  # Add the combined motor vehicle source emissions to final_data 
  finalData <- rbind(joinedData,vehTotals)
  finalData <- with(finalData, finalData[order(year,EI.Sector,city),])
  
  # Make line plot of yearly emissions for motor vehicle sources by sector and city 
  g <- ggplot(finalData,aes(year,Total.Emissions))
  # ggplot pieces: plot + plot in panels by city + color dots/lines by EI.Sector + labels + make x axis labels vertical
  g + facet_wrap(~city) +  geom_point(aes(color=EI.Sector)) + geom_line(aes(color=EI.Sector)) + labs(title="Annual PM-2.5 Emissions for Motor Vehicle Sources", x="Year", y="Emissions (tons)") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # Save above plot
  ggsave("plot6.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html

  # Not used, but for reference
  # Bar Plot
  # g + facet_wrap(~city) +  geom_bar(aes(fill = EI.Sector), stat = "identity") + labs(title="Annual PM-2.5 Emissions for Motor Vehicle Sources", x="Year", y="Emissions (tons)") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # Grid style plots 
  # ggplot pieces: plot + color dots by city + no legend + plot in panels by sector and let y axis adjust for each city's emissions + labels + make x axis labels vertical
  #g + geom_point(aes(color=city)) + theme(legend.position="none") + facet_grid(city~ EI.Sector,scales="free_y") + labs(title="Annual Emissions for Motor Vehicle Sources", x="Year", y="Emissions (tons)") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}