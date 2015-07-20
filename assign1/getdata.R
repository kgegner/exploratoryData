getdata <- function() {
  # Read data
  txt_file <- "~/Documents/Programming/R/coursera/exploratorydata/data/household_power_consumption.txt"
  col_classes <- c(rep("character",2), rep("numeric",7))
  pwr_data <- read.table(txt_file, header = TRUE, sep = ";", colClasses = col_classes, na.strings = "?")
  
  # Subset data to include only Feb 1, 2007 through Feb 2, 2007
  dat <- subset(pwr_data, pwr_data$Date == "1/2/2007" | pwr_data$Date == "2/2/2007")
  
  return(dat)
}
