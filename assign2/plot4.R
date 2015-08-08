# plot4 creates a line plot of annual coal emissions throughout the U.S.

plot4 <- function() {
  
  getdata()
  
  # Select only coal sources from SCC data 
  SCC_coal <- filter(SCC, str_detect(EI.Sector, "Coal"))
 
  # Save classification codes for all coal sources listed in EI.Sector
  SCC_coal_vals <- as.numeric(as.character(unique(SCC_coal$SCC)))
  
  # Join NEI and SCC into one data frame for all coal sources
  coalData <- inner_join(NEI, SCC_coal, by="SCC") %>% 
    filter(SCC %in% SCC_coal_vals)
  
  # Sort and modify coal_data for plotting
  cleanData <- coalData %>% 
    # 1. Clean up EI.Sector descriptions
    mutate(EI.Sector = str_replace_all(EI.Sector, "Fuel Comb - ", "")) %>%
    mutate(EI.Sector = str_replace_all(EI.Sector, " - Coal", "")) %>%
    # 2. Specify columns used for grouping
    group_by(EI.Sector, year) %>% 
    # 3. Sum all emissions for each year and sector (Total.Emissions in thousands of tons)
    summarize(Total.Emissions = sum(Emissions)/1000) %>%
    # 4. Select columns to include in final data frame
    select(year, EI.Sector, Total.Emissions) 
  
  # Sum emissions for each year across all coal sources
  coalTotals <- cleanData %>%
    # 1. Specify columns for grouping
    group_by(year) %>%
    # 2. Sum emissions from each source type for each year
    summarize(Total.Emissions = sum(Total.Emissions), EI.Sector="Combined") %>%
    # 3. Select columns to include
    select(year,EI.Sector,Total.Emissions)

  # Join data sets for emissions by coal source and combined emissions for all coal sources
  finalData <- rbind(cleanData,coalTotals)
  finalData <- with(finalData, finalData[order(year,order(-Total.Emissions)),])
  
  # Plot yearly emissions for coal sources by sector
  g <- ggplot(final_data,aes(year,Total.Emissions))
  g + geom_point(aes(color=EI.Sector)) + geom_point(aes(color=EI.Sector)) + geom_line(aes(color=EI.Sector)) + labs(title="Annual PM-2.5 Emissions for Coal Sources", x="Year", y="Emissions (thousands of tons)")
  
  # Save above plot
  ggsave("plot4.png")
  
  # Referenced: http://rstudio-pubs-static.s3.amazonaws.com/24355_fd01489d9a8544bca3d9d186b6088734.html
}