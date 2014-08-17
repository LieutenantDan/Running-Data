## Note that this functioion only works with the appropriate inputs and is designed to work inside the extractRun function

pullWeatherData <- function (date, time, geoData) {
    url <- paste("http://api.wunderground.com/api/", weatherKey, "/history_", gsub("-", "", date), "/q/", state.abb[grep(geoData[2], state.name)], "/", gsub(" ", "_", geoData[1]), ".json", sep = "")
    weatherData <- fromJSON(getURL(url, ssl.verifypeer = FALSE))
    weatherData <- data.frame(weatherData$history$observations)
    
    rTime <- as.numeric(substr(time, 1, 2)) + (as.numeric(substr(time, 4, 5))/60)
    wHour <- as.numeric(weatherData$date$hour)
    wMin <- as.numeric(weatherData$date$min)
    wTime <- wHour + (wMin/60)
    
    ## Save row index of closest weather observation as obs
    obs <- which(abs(wTime - rTime) == min(abs(wTime - rTime)))
    
    temperature <- weatherData[obs, 4]
    humidity <- weatherData[obs, 7]
    return(c(temperature, humidity))
}
## To-Do: Code in optionality for non-U.S. cities (this requires a different html request from the API)