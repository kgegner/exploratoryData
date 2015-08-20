# If data not yet loaded, loads NEI and SCC data.
# If data already loaded, reads NEI and SCC from the data cache.

getdata <- function() {

  # Get NEI cached data, will be NULL if NEI data not yet loaded
  NEI <- dataCache$getNEI()
  
  # NEI data hasn't been loaded yet, so load it, and assign to NEI data frame
  if(is.null(NEI)){
    dataCache$loadNEI()
    NEI <<- dataCache$getNEI()
  }
  
  # Get SCC cached data, will be NULL if SCC data not yet loaded
  SCC <- dataCache$getSCC()
  
  # SCC data hasn't been loaded yet, so load it, and assign to SCC data frame
  if(is.null(SCC)){
    dataCache$loadSCC()
    SCC <<- dataCache$getSCC()
  }
}