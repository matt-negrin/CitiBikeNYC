library(plyr)
library(dplyr)
library(lubridate)
library(Imap)
library(ggplot2)
library(ggmap)

#altering data types to desired format
cleanBike <- rawBike %>%
	mutate(gender = ifelse(gender == 1, "male", ifelse(gender == 2,"female",NA))) %>%
	mutate(gender = as.factor(gender))

#create trip duration columns for seconds, minutes, and hours 
cleanBike <- cleanBike %>%
	rename(tripSec = tripduration) %>%
	mutate(tripMin = tripSec / 60) %>%
	mutate(tripHr = tripMin / 60)

##creating lookup table for addresses
#generate age column based on provided birth year (no month provided, age is therefore approximate)
cleanBike <- cleanBike %>%
	mutate(birthYear = year(as.Date(cleanBike$birth.year, format = "%Y"))) %>%
	mutate(todayYear = rep(year(Sys.Date()), length(cleanBike$birth.year))) %>%
	mutate(age = todayYear - birthYear)

#the goal here is to minimize number of ggmaps queries (limit 2500/ 24hr)
lookupLoc <- select(cleanBike, start.station.id, end.station.id, start.station.latitude, start.station.longitude)

#removing all duplicate stations from lookup table
lookupLoc <- filter(lookupLoc, !duplicated(start.station.id))

#applying ggmap package 'revgeocode' to generate addresses
address <- is.character(0)
for (i in 1:length(lookupLoc$start.station.latitude)) {
    address[i] <- revgeocode(c(lon = lookupLoc$start.station.longitude[i], lat = lookupLoc$start.station.latitude[i]), messaging=FALSE, override_limit = TRUE)
}   

#adding address to lookup table
lookupLoc <- mutate(lookupLoc, address = address)

#removing unnecessary variables
rm(address, i)



