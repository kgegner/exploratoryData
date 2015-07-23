makeDataCache <- function() {
  
  # Assign NEI and SCC as NULL when new importData object created
  NEI <- NULL
  SCC <- NULL
  
  # Read NEI data and assign to global environment variable
  loadNEI <- function() {
    # Read data for PM2.5 emmissions in years 1999, 2002, 2005, and 2008
    NEI <<- readRDS("../data/exdata-data-NEI_data/summarySCC_PM25.rds")
  }
  
  #Read SCC data and assign to global environment variable
  loadSCC <- function() {
    # Read data for mapping the source classification code to the emissions data
    SCC <<- readRDS("../data/exdata-data-NEI_data/Source_Classification_Code.rds")
  }
  
  # Return NEI data
  getNEI <- function() NEI
  
  # Return SCC data
  getSCC <- function() SCC
  
  # List of possible functions for an importData object
  list(loadNEI=loadNEI, loadSCC=loadSCC, getNEI=getNEI, getSCC=getSCC)
}