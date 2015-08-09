totals <- function(cleanData,groupBy="year") {
  
  if (groupBy =="year") {
    # Sum emissions for each year across all specified sources
    combinedTotals <- cleanData %>%
      # 1. Specify columns for grouping
      group_by(year) %>%
      # 2. Sum emissions from each source type for each year
      summarize(Total.Emissions = sum(Total.Emissions), EI.Sector="Combined") %>%
      # 3. Select columns to include
      select(year,EI.Sector,Total.Emissions)
    
    # Create final data frame by binding clean data with combined source totals 
    finalData <- rbind(cleanData,combinedTotals)
    finalData <- with(finalData, finalData[order(year,EI.Sector),])
  }
  
  else if (groupBy == "city") {
    # Sum emissions for each year across all motor vehicle sources
    combinedTotals <- cleanData %>%
      # 1. Specify columns for grouping
      group_by(year,city) %>%
      # 2. Sum emissions from each source type for each year
      summarize(Total.Emissions = sum(Total.Emissions), EI.Sector="Combined") %>%
      # 3. Select columns to include
      select(year,EI.Sector,Total.Emissions,city)
    
    # Create final data frame by binding clean data with combined source totals 
    finalData <- rbind(cleanData,combinedTotals)
    finalData <- with(finalData, finalData[order(year,EI.Sector,city),])
  }
  
  return(finalData)

}