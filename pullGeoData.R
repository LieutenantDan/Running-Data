## Note that this code requires imports from JSONLITE and RCURL as well as an API KEY for Weather Underground

## Define decision function that either uses old data through the geoIndex or looks up new data with reverseGeo()
pullGeoData <- function(lat, long) {

    ## Open saved location data, assign as data.frame to locationSave, and coerce zip code, latitude, and longitude to numeric
    locationSave <- read.csv("locationSave.csv", colClasses = c(replicate(6, "character")))
    locationSave[,4] <- as.numeric(locationSave[,4])
    locationSave[,5] <- as.numeric(locationSave[,5])
    locationSave[,6] <- as.numeric(locationSave[,6])
    
    ## Define approximate equality function to use when deciding whether or not to use old geo data or find new data
    approxEqual <- function(a, b, delta = 0.001) {
        absVal <- abs(a-b)
        return(absVal <= delta)
    }
    
    ## Define reverse geocoding function
    reverseGeo <- function(lat, long) {
        url <- paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=", lat, ",", long, "&key=", geoKey, sep = "")
        geoData <- getURL(url, ssl.verifypeer = FALSE)
        geoData <- fromJSON(geoData)
        geoData <- geoData[[1]]$address_components
        
        ## Define JSON Parsing Function
        
        returnGeo <- function(geoData) {
            names <- geoData[[1]][[1]]
            types <- NULL
            
            for (i in 1:length(geoData[[1]][[3]])) {
                if (length(geoData[[1]][[3]][[i]] > 1)) {
                    typeAdd <- paste(geoData[[1]][[3]][[i]], collapse = "")
                    types <- c(types, typeAdd)
                }
            }
            City <- names[which(types == "localitypolitical")]
            State <- names[which(types == "administrative_area_level_1political")]
            Country <- names[which(types == "countrypolitical")]
            Zip_Code <- names[which(types == "postal_code")]
            geoObject <- c(City, State, Country, Zip_Code, lat, long)
            return <- geoObject
        }
        geoData <- returnGeo(geoData)
        return(geoData)
    }
    
    ## Define function to check if lat/long is in saved locations csv, if not, this function utilizes the reverseGeo function
    decision <- function(lat, long) {
        for (i in 1:nrow(locationSave)) {
            if (approxEqual(lat, locationSave$Latitude[i]) == TRUE & approxEqual(long, locationSave$Longitude[i]) == TRUE) {
                return(as.vector(t(locationSave[i,])))
            }
            else {
                geoReturn <- reverseGeo(lat, long)
                locationSave <- rbind(locationSave, geoReturn)
                write.csv(locationSave, "locationSave.csv", row.names = FALSE)
                return(geoReturn)
            }
        }
    }
    
    ## Check Coordinates for approx equality; if approx equal, use index of saved locations, else use reverseGeo to look up data
    ## And return geographic data
    return(decision(lat, long))
}