init <- function() {
  
  # Run first thing, sets up workspace and required functions/variables/objects
  
  library(dplyr)
  library(ggplot2)
  library(stringr)

  source("makeDataCache.R")
  source("getdata.R")
  source("filterdata.R")
  source("cleandata1.R")
  source("cleandata2.R")
  source("totals.R")
  source("plot1.R")
  source("plot2.R")
  source("plot3.R")
  source("plot4.R")
  source("plot5.R")
  source("plot6.R")
  
  # Create dataCache object to store NEI and SCC data
  dataCache <<- makeDataCache()
  
}