##CLEANING Citibike and Weather data

#relevant libraries
library(plyr)
library(dplyr)
library(lubridate)
library(Imap)
library(ggplot2)
library(ggmap)



##using address lookup table generated in clean1.r to generate location addresses

#adding start addresses back to cleanBike using lookup table
lookupLocSt <- select(lookupLoc, start.station.id, address)
lookupLocSt <- dplyr::rename(lookupLocSt, startAddress = address)
cleanBike <- join(cleanBike, lookupLocSt, by = c("start.station.id" = "start.station.id"))

rm(lookupLoc, lookupLocSt)

##creating lookup table for distance betweeen two points
#adding in unique trip identifier by merging start station ID and end station ID
cleanBike <- mutate(cleanBike, tripId = paste(start.station.id, "-", end.station.id))

#creating lookup table for distance
lookupDist <- select(cleanBike, start.station.id, end.station.id, start.station.latitude, start.station.longitude, end.station.latitude, end.station.longitude, tripId)
lookupDist <- filter(lookupDist, !duplicated(tripId))

#applying Imap package 'gdist' to lookup distance between two lat/lon points
dist <- is.numeric(0)
for (j in 1:length(lookupDist$start.station.latitude)) {
    dist[j] <- gdist(lookupDist$start.station.longitude[j], lookupDist$start.station.latitude[j], lookupDist$end.station.longitude[j], lookupDist$end.station.latitude[j], units = "miles")
}

#compiling lookup table into concise 2 columns for join function
lookupDist <- lookupDist %>% 
    mutate(tripDist = dist) %>%
    select(tripId, tripDist)

#joining lookup table back into primary data frame (cleanBike)
cleanBike <- join(cleanBike, lookupDist, by = c("tripId" = "tripId"))

rm(lookupDist, dist, j)

##generating starting zip code and ending zip code
#pulling zipcode from addresses produced by ggmaps package
startZip <- substring(substr(cleanBike$startAddress, nchar(cleanBike$startAddress)-10+1, nchar(cleanBike$startAddress)),1,5)

#adding zipcodes into main data frame (cleanBike)
cleanBike <- mutate(cleanBike, startZip = as.factor(startZip))

rm(startZip)

#generate column for representative total trip velocity (miles/hr)
cleanBike <- mutate(cleanBike, tripVel = tripDist / tripMin)

#generate columns for weekday labels
#generate day-hour column as unique daily identifier
cleanBike <- cleanBike %>% 
	mutate(startWday = lubridate::wday(startTime, label=TRUE)) %>%
	mutate(startNday = day(startTime)) %>%
	mutate(monDayHour = as.factor(paste(month(startTime), "-", day(startTime), "-", hour(startTime), sep="")))

##final compiling of relevant data into data frame
finJul <- dplyr::select(cleanBike, bikeid, usertype, gender, age, tripId, start.station.id, end.station.id, start.station.name, end.station.name, startAddress, startZip, startWday, startTime, stopTime, tripSec, tripMin, tripHr, tripDist, tripVel, start.station.latitude, start.station.longitude, end.station.latitude, end.station.longitude, monDayHour)

##renaming to preferable naming strategy
finJul <- dplyr::rename(finJul, startId = start.station.id, endTime = stopTime, bikeId = bikeid, startName = start.station.name, startLat = start.station.latitude, startLon = start.station.longitude, endId = end.station.id, endName = end.station.name, endLat = end.station.latitude, endLon = end.station.longitude, userType = usertype)


### WEATHER ###

#selecting and renaming relevant data from raw data set
cleanWeath <- select(rawClimJul, Year.Local, Month.Local, Day.Local, Hour.Local, Cloud.Cover.Fraction, Dew.Point..C., Humidity.Fraction, Precipitation.One.Hour..mm., Snow.Depth..cm., Temperature..C., Visibility..km., Weather.Code.Most.Severe...Description, Wind.Gust..m.s., Wind.Speed..m.s.)

cleanWeath <- rename(cleanWeath, year = Year.Local, month = Month.Local, day = Day.Local, hour = Hour.Local, cloudCover = Cloud.Cover.Fraction, dew = Dew.Point..C., humidity = Humidity.Fraction, precip = Precipitation.One.Hour..mm., snow = Snow.Depth..cm., tempC = Temperature..C., visib = Visibility..km., weathDesc = Weather.Code.Most.Severe...Description, windSpeed = Wind.Speed..m.s.)

#creating date column out of year, month, day, hour columns
#create day column out of date column
#create temp in fahrenheit out of temp in celsius
#adding in column for date by day (no hour) to use in over time chart
cleanWeath <- cleanWeath %>% 
	mutate(dateTime = paste(year, "-", month, "-", day, " ", hour, ":00:00", sep="")) %>%
	mutate(dateTime = as.POSIXct(dateTime, format="%Y-%m-%d %H:%M:%S")) %>%
	mutate(monDayHour = as.factor(paste(month(dateTime), "-", day(dateTime), "-", hour(dateTime), sep=""))) %>%
	mutate(tempF = (tempC*9)/5 + 32) %>%
	mutate(dateDay = as.Date(dateTime))


#assigning NA inches of rain & snow to 0
cleanWeath$precip <- ifelse(is.na(cleanWeath$precip), 0, cleanWeath$precip)
cleanWeath$snow <- ifelse(is.na(cleanWeath$snow), 0, cleanWeath$snow)

#selecting final set of columns to use
cleanWeath <- select(cleanWeath, dateTime, monDayHour, dateDay, cloudCover, dew, humidity, precip, snow, tempF, visib, weathDesc, windSpeed)

## COMBINING BIKE AND WEATHER DATA INTO ONE FINISHED DATASET ##
finJul <- plyr::join(finJul, cleanWeath, by=c("monDayHour" = "monDayHour"))

##Replacing blank weather descriptions with "Clear"
finJul <- finJul %>%
	mutate(weathDesc = ifelse(weathDesc=="", as.character("clear"), as.character(weathDesc))) %>%
	mutate(weathDesc = as.factor(weathDesc))

#adding in weather indicators
finJul <- finJul %>%
        mutate(weathFlag = ifelse(weathDesc=="clear",0, 
                ifelse(weathDesc=="haze",1,
                ifelse(weathDesc=="mist",2,
                ifelse(weathDesc=="light rain",3,
                ifelse(weathDesc=="rain",4,
                ifelse(weathDesc=="heavy rain",5,0))))))
        )

#Adding in starting neighborhood dissection
nb <- tbl_df(read.csv("citibike_p2/data/neighborhoods.csv"))
nb <- rename(nb, startZip = ZIP, neighborhood = NABE)
nb <- mutate(nb, startZip = as.factor(startZip))
finJul <- join(nb, finJul, by = c("startZip" = "startZip"))

#creating useful neighborhood list and removing New Jersey (it's an error)
nabes <- unique(nb$neighborhood)
nabes <- factor(nabes[nabes != "NEW JERSEY"])
nabes <- as.character(nabes)

#Adding in Hour column
hr <- hour(finJul$dateTime)
finJul <- cbind(finJul, hr)

rm(hr, nb)

#removing unnecessary datasets
rm(rawClimJul, cleanBike, rawBike)

