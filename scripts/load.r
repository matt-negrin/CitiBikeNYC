##LOADING DATA

#set working directory
setwd("/users/mattnegrin/r")

#Load relevant libraries
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinythemes)
library(BH)
library(devtools)
library(shinyapps)

##CITIBIKE
#set July file variables
fileUrl7 <- "http://s3.amazonaws.com/tripdata/201407-citibike-tripdata.zip"
filePath7 <- "./citibike_P2/citiJul.csv"
fileZip7 <- "201407-citibike-tripdata.zip"
fileCsv7 <- "2014-07 - Citi Bike trip data.csv"

#set August file variables
fileUrl8 <- "http://s3.amazonaws.com/tripdata/201408-citibike-tripdata.zip"
filePath8 <- "./citibike_P2/citiAug.csv"
fileZip8 <- "201408-citibike-tripdata.zip"
fileCsv8 <- "2014-08 - Citi Bike trip data.csv"

#set September file variables
fileUrl9 <- "http://s3.amazonaws.com/tripdata/201409-citibike-tripdata.zip"
filePath9 <- "./citibike_P2/citiSep.csv"
fileZip9 <- "201409-citibike-tripdata.zip"
fileCsv9 <- "201409-citibike-tripdata.csv"

#set October file variables
fileUrl10 <- "http://s3.amazonaws.com/tripdata/201410-citibike-tripdata.zip"
filePath10 <- "./citibike_P2/citiOct.csv"
fileZip10 <- "201410-citibike-tripdata.zip"
fileCsv10 <- "201410-citibike-tripdata.csv"

#download zip file from Citibike System Data website
download.file(fileUrl7,fileZip7, mode="wb")
download.file(fileUrl8,fileZip8, mode="wb")
download.file(fileUrl9,fileZip9, mode="wb")
download.file(fileUrl10,fileZip10, mode="wb")

#unzip the data
file7 <- unzip(fileZip7, fileCsv7)
file8 <- unzip(fileZip8, fileCsv8)
file9 <- unzip(fileZip9, fileCsv9)
file10 <- unzip(fileZip10, fileCsv10)

#store citibike data in data in dplyr data frame
rawJul <- tbl_df(read.csv(file7))
rawAug <- tbl_df(read.csv(file8))
rawSep <- tbl_df(read.csv(file9))
rawOct <- tbl_df(read.csv(file10))

rawSep <- rawSep %>%
    mutate(starttime = as.POSIXct(starttime, format="%m/%d/%Y %H:%M:%S")) %>%
    mutate(stoptime = as.POSIXct(stoptime, format="%m/%d/%Y %H:%M:%S")) %>%
    mutate(birth.year = as.factor(birth.year))

rawOct <- rawOct %>%
    mutate(starttime = as.POSIXct(starttime, format="%m/%d/%Y %H:%M:%S")) %>%
    mutate(stoptime = as.POSIXct(stoptime, format="%m/%d/%Y %H:%M:%S")) %>%
    mutate(birth.year = as.factor(birth.year))

rawJul <- rawJul %>%
	mutate(starttime = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S")) %>%
	mutate(stoptime = as.POSIXct(stoptime, format="%Y-%m-%d %H:%M:%S"))

rawAug <- rawAug %>%
	mutate(starttime = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S")) %>%
	mutate(stoptime = as.POSIXct(stoptime, format="%Y-%m-%d %H:%M:%S"))

#combine all raw bike data together
rawBike <- merge(rawJul, rawAug, by=intersect(names(rawJul), names(rawAug)), all=TRUE)
rawBike <- merge(rawBike, rawSep, by=intersect(names(rawBike), names(rawSep)), all=TRUE)
rawBike <- merge(rawBike, rawOct, by=intersect(names(rawBike), names(rawOct)), all=TRUE)

rawBike <- rename(rawBike, startTime = starttime, stopTime = stoptime)

##WEATHER
#loading data from purchased CSV file
filePathW <- "citibike_p2/raw_data/CP_NYC_2014.csv"
rawClim <- tbl_df(read.csv(filePathW))
rawClimJul <- filter(rawClim, Month.Local == 7 | Month.Local == 8 | Month.Local == 9 | Month.Local == 10)

#getting rid of unnecessary files
rm(rawClim, file7, file8, file9, file10, filePathW, filePath7, filePath8, filePath9, filePath10, fileUrl7, fileUrl8, fileUrl9, fileUrl10, fileZip7, fileZip8, fileZip9, fileZip10, fileCsv7, fileCsv8, fileCsv9, fileCsv10, rawAug, rawJul, rawOct, rawSep)




