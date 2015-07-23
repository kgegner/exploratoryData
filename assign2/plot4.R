plot4 <- function() {
  
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
  
  # Select only coal sources from SCC data 
  SCC_coal <- filter(SCC, str_detect(EI.Sector, "Coal"))
 
  # Save classification codes for all coal sources listed in EI.Sector
  SCC_coal_vals <- as.numeric(as.character(unique(SCC_coal$SCC)))
  
  # Join NEI and SCC into one data frame for all coal sources
  coal_data <- inner_join(NEI, SCC_coal, by="SCC") %>% 
    filter(SCC %in% SCC_coal_vals)
  
  # Sort and modify coal_data for plotting
  cleandata <- coal_data %>% 
    # 1. Clean up EI.Sector descriptions
    mutate(EI.Sector = str_replace_all(EI.Sector, "Fuel Comb - ", "")) %>%
    mutate(EI.Sector = str_replace_all(EI.Sector, " - Coal", "")) %>%
    # 2. Specify columns used for grouping
    group_by(EI.Sector, year) %>% 
    # 3. Sum all emissions for each year and sector (Total.Emissions in thousands of tons)
    summarise(Total.Emissions = sum(Emissions)/1000) %>%
    # 4. Select columns to include in final data frame
    select(year, EI.Sector, Total.Emissions) 
  
  # Plot yearly emissions for coal sources by sector
  g <- ggplot(cleandata,aes(year,Total.Emissions))
  g + geom_point(color="navy") + facet_wrap(~EI.Sector) + geom_smooth(color="olivedrab",method="lm",se=FALSE) + labs(title="Annual Emissions for Coal Sources", x="Year", y="Emissions (thousands of tons)")
  
  # Save above plot
  ggsave("plot4.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}