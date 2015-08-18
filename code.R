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
#stormData <- read.csv("data.csv")

library(plyr)

# translate all letters to lowercase
stormData$EVTYPE <- tolower(stormData$EVTYPE)
# replace all punct. characters with a space
stormData$EVTYPE <- gsub("[[:blank:][:punct:]+]", " ", stormData$EVTYPE)
length(unique(stormData$EVTYPE))

harm <- ddply(stormData, .(EVTYPE), summarize,
                    killed = sum(FATALITIES),
                    injuried = sum(INJURIES),
                    property = .(PROPDMG),
                    property_exp = tolower(PROPDMGEXP),
                    crop = .(CROPDMG),
                    crop_exp = tolower(CROPDMGEXP))


              

exp_transform <- function(e) {
    # h -> hundred, k -> thousand, m -> million, b -> billion
    if (e %in% c('h', 'H'))
        return(2)
    else if (e %in% c('k', 'K'))
        return(3)
    else if (e %in% c('m', 'M'))
        return(6)
    else if (e %in% c('b', 'B'))
        return(9)
    else if (!is.na(as.numeric(e))) # if a digit
        return(as.numeric(e))
    else if (e %in% c('', '-', '?', '+'))
        return(0)
    else {
        stop("Invalid exponent value.")
    }
}


