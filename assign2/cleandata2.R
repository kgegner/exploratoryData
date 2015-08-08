# Shortens EI.Sector descriptions and creates new column for total annual 
# emissions of the requested source type. The resulting data frame is ready
# for plotting.

cleandata2 <- function(filteredData,replaceString1,replaceString2,groupingCol,emissionsDivisor=1) {
  
  if (groupingCol == "type") {
    
    cleanData <- filteredData %>%
      
      # 1. Clean up EI.Sector descriptions
      mutate(EI.Sector = str_replace_all(EI.Sector, replaceString1, "")) %>% 
      mutate(EI.Sector = str_replace_all(EI.Sector, replaceString2, "")) %>% 
    
      # 2. Specify columns used for grouping
      group_by(year,type) %>% 
    
      # 3. Sum annual emissions for each requested source type
      summarize(Total.Emissions = sum(Emissions)/emissionsDivisor) %>%
      
      # 4. Select columns to include in final data frame (year + grouping column + Total.Emissions)
      select(year,type,Total.Emissions)
  }
  
  else if (groupingCol== "EI.Sector") {
    
    cleanData <- filteredData %>%
      
      # 1. Clean up EI.Sector descriptions
      mutate(EI.Sector = str_replace_all(EI.Sector, replaceString1, "")) %>% 
      mutate(EI.Sector = str_replace_all(EI.Sector, replaceString2, "")) %>% 
      
      # 2. Specify columns used for grouping
      group_by(year,EI.Sector) %>% 
      
      # 3. Sum annual emissions for each requested source type
      summarize(Total.Emissions = sum(Emissions)/emissionsDivisor) %>%
      
      # 4. Select columns to include in final data frame (year + grouping column + Total.Emissions)
      select(year,EI.Sector,Total.Emissions)
  }
  
  return(cleanData)
}
