getdata <- function() {
  
  # Read data for PM2.5 emmissions in years 1999, 2002, 2005, and 2008
  NEI <<- readRDS("../data/exdata-data-NEI_data/summarySCC_PM25.rds")
  
  # Read data for mapping the source classification code to the emissions data
  SCC <<- readRDS("../data/exdata-data-NEI_data/Source_Classification_Code.rds")
}