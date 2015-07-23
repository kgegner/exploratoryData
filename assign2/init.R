init <- function() {
  # Run first thing, sets up workspace and required variables/objects
  
  # Create dataCache object to store NEI and SCC data
  dataCache <<- makeDataCache()
}