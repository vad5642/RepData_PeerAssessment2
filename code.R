

#download_data()
#stormData <- read.csv("dpppppata.csv")

library(plyr)
options(scipen = 999)  # Turn off scientific notations for numbers

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download_dir <- getwd()

zip_file <- paste(download_dir, "data.csv.bz2", sep = "/")
download.file(url, zip_file)
stormData <- read.csv(bzfile("data.csv.bz2"))

# translate all letters to lowercase
stormData$EVTYPE <- tolower(stormData$EVTYPE)
# replace all punct. characters with a space
stormData$EVTYPE <- gsub("[[:blank:][:punct:]+]", " ", stormData$EVTYPE)
length(unique(stormData$EVTYPE))

getExp <- function(e) 
{
    # h -> 10^2, k -> 10^3, m -> 10^6, b -> 10^9
    
    m <- 0
    if (e %in% c('h', 'H'))
        m <- (2)
    else if (e %in% c('k', 'K'))
        m <- (3)
    else if (e %in% c('m', 'M'))
        m <- (6)
    else if (e %in% c('b', 'B'))
        m <- (9)
    else if (!is.na(as.numeric(e))) # if a digit
        m <- (as.numeric(e))
    else if (e %in% c('', '-', '?', '+'))
        m <- (0)
    else 
    {
        stop("Invalid exponent value.")
    }
    return (10**m)
}

stormData$PROPDMG_VAL <- stormData$PROPDMG * sapply(stormData$PROPDMGEXP, getExp)
stormData$CROPDMG_VAL <- stormData$CROPDMG * sapply(stormData$CROPDMGEXP, getExp)

harm <- ddply(stormData, .(EVTYPE), summarize,
                    killed = sum(FATALITIES),
                    injured = sum(INJURIES),
                    property = sum(PROPDMG_VAL),
                    crop = sum(CROPDMG_VAL))


# Find events that caused most death and injury
fatalEvents <- head(data.frame(type=harm$EVTYPE, killed = harm$killed)
                     [order(harm$killed, decreasing = T), ], 10)
fatalEvents

injuryEvents <- head(data.frame(type=harm$EVTYPE, injured = harm$injured)
                      [order(harm$injured, decreasing = T), ], 10)
injuryEvents

propertyDamage <- head(data.frame(type=harm$EVTYPE, property = harm$property)
                       [order(harm$property, decreasing = T), ], 10)
format(propertyDamage, big.mark=",")

cropDamage <- head(data.frame(type=harm$EVTYPE, crop = harm$crop)
                   [order(harm$crop, decreasing = T), ], 10)
format(cropDamage, big.mark=",")

fatalitiesPlot <- qplot(type, data = fatalEvents, weight = killed, geom = "bar") + 
    aes(x=reorder(type, killed)) +
    scale_y_continuous("Number of Fatalities") +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Severe Weather Type") +
    ggtitle("Total Fatalities by Severe Weather\n Events in the U.S.\n from 1950 - 2011")

injuriesPlot <- qplot(type, data = injuryEvents, weight = injured, geom = "bar") +
    aes(x=reorder(type, injured))+
    scale_y_continuous("Number of Injuries") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Severe Weather Type") +
    ggtitle("Total Injuries by Severe Weather\n Events in the U.S.\n from 1950 - 2011")

grid.arrange(fatalitiesPlot, injuriesPlot, ncol = 2)

propertyPlot <- qplot(type, data = propertyDamage, weight = property, geom = "bar") + 
    aes(x=reorder(type, property)) +
    scale_y_continuous("Property Damage, USD") +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Severe Weather Type") +
    ggtitle("Total Property Damage by \n Severe Weather Events in the U.S.\n from 1950 - 2011")

cropPlot <- qplot(type, data = cropDamage, weight = crop, geom = "bar") +
    aes(x=reorder(type, crop))+
    scale_y_continuous("Crop Damage, USD") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Severe Weather Type") +
    ggtitle("Total Crop Damage by \n Severe Weather Events in the U.S.\n from 1950 - 2011")

grid.arrange(propertyPlot, cropPlot, ncol = 2)
