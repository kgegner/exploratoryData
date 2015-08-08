# Extracts the requested emission sources from SCC data, saves the 
# classification codes pertaining to those sources, and then joins the
# NEI and extracted SCC data frames into one data frame. The final data 
# frame can then be that can be 
# manipulated for plotting.

filterdata <- function(EIsectorDescription) {
  
  # Select requested emission sources (columnDesciption) from SCC data
  SCCsourceSpecific <- filter(SCC, str_detect(EI.Sector, EIsectorDescription))
  
  # Save classification codes for all requested emission sources
  classCodes <- as.character(unique(SCCsourceSpecific$SCC))
  
  # Join NEI and SCC into one data frame for all requested emission sources
  filteredData <- inner_join(NEI, SCCsourceSpecific, by="SCC") %>% 
    filter(SCC %in% classCodes)
  
  return(filteredData)
}