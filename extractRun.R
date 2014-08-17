## Import requisite libraries, API Keys, and functions
library(jsonlite)
library(RCurl)
source("APIKeys.R")
source("git/Running-Data/pullGeoData.R")
source("git/Running-Data/pullWeatherData.R")

## Save folder location of runs and index individual runs into vector
runFolder <- "C:/Users/Caleb Franzmann/TomTom MySports/LieutenantDan/"
runIndex <- paste(runFolder, list.files(runFolder), sep = "")

runExt <- lapply(runIndex, list.files)
runExt <- sapply(runExt, '[[', 1)
runFiles <- paste(runIndex, "/", runExt, sep = "")

## Extract Run is a function that takes a csv file path as its only argument and extracts all relevant data
extractRun <- function(run) {
    data <- read.table(run, sep = ",", colClasses = "numeric", header = TRUE)
    
    ## Extract row of metrics to put into master data frame
    sLat <- data$lat[1]
    sLong <- data$long[1]
    
    ## TimeDate Procedure
    time <- substr(run, 75, 79)
    date <- substr(run, 56, 65)
    
    ## Pull Geographic and Weather Data from APIs
    geoData <- pullGeoData(sLat, sLong)
    weatherData <- pullWeatherData(date, time, geoData)    
    
    row <- c(date, time, geoData[1], geoData[2], geoData[3], geoData[4], sLat, sLong, max(data$time, na.rm = TRUE), max(data$distance, na.rm = TRUE)*0.000621371, mean(data$speed, na.rm = TRUE), mean(data$heartRate, na.rm = TRUE), max(data$calories, na.rm = TRUE), weatherData[1], weatherData[2])
    
    ## Add row to master data frame
    df <- rbind(df, row)
}

## Build Data Frame
df <- data.frame()
df <- sapply(runFiles, extractRun)
df <- data.frame(matrix(unlist(df), ncol = 15, byrow=T), stringsAsFactors = FALSE) ## UPDATE NCOL # IN FINAL VERSION
colnames(df) <- c("Date", "Time", "City", "State", "Country", "Zip.Code", "Latitude", "Longitude", "Seconds", "Distance(Miles)", "Avg.Speed", "Avg.HR", "Calories", "Temperature", "Humidity")
write.csv(df, file = "runData.csv", row.names = FALSE)