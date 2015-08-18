###############################################
# Functions
###############################################
download_data <- function() 
{
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download_dir <- getwd()
  
  zip_file <- paste(download_dir, "data.csv.bz2", sep = "/")
  #if(!file.exists(zip_file)) { download.file(url, zip_file) }
}



#download_data()
stormData <- read.csv("data.csv")

