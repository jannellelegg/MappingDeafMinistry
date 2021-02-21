# let's tidy things up a bit here - with Lincoln's suggestion, pull out data clean up as separate file.

# defining libraries
library(dbplyr)
library(tidyverse)
library(maps)
library(sf)
library(leaflet)
library(opencage)
Sys.setenv(OPENCAGE_KEY = 'c9df8f45fc114ffd88c2aa421814607f')
opencage_key()

# let's read in all that data again

annualMissions <- read.csv("~/TidierAnnualData.csv", stringsAsFactors = FALSE)
ResDeafSchools <- read.csv("~/ResDeafSchools.csv", stringsAsFactors = FALSE)
ConSites <- read.csv("~/Conventions_ConferenceSitesto1880.csv", stringsAsFactors = FALSE)

## Next, let's try to forward geocode each of them  
# First we should combine the city and state columns, creating a new column named place.
# note for future Jannelle - annualMissions has a lot of extraneous data that can be extracted later 
# For now, let's focus on a smaller subset of parameters


#First up, Mission Data

MissionData <- annualMissions %>%
  select(year, facility, city, state, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency) %>%
  mutate(place = str_c(city, ", ", state))

# removing duplicates

MissionPlaces <- MissionData %>%
  distinct(city, state, place)

# geocoding

MissionGeocode <- map_dfr(MissionPlaces$place, function(x) {
  out <- opencage_forward(x, countrycode = "US", no_annotations = TRUE, limit = 1)
  out$results
})

# Grabbing coordinates

MissionCoordinates <- MissionGeocode %>%
  select(place = query,
         lat = geometry.lat,
         lng = geometry.lng)

# joining coordinates back to the data

MissionsGeocoded <- MissionData %>%
  left_join(MissionCoordinates, by = "place")

# Mexico, NY is not friendly. Reading out csv and adding lat/long by hand
# assigning NJ Lat/Long for Syle 1880

write.csv(MissionsGeocoded, "~/MissionsGeocodedRough.csv")

## Read that data back in

MissionsGeocodedCorrected <- read.csv("~/MissionsGeocodedRoughCorrected.csv", stringsAsFactors = FALSE)

## Schools data

SchoolData <- ResDeafSchools %>%
  select(School, YearFounded, City, State) %>%
  mutate(place = str_c(City, ", ", State))

# removing duplicates

SchoolPlaces <- SchoolData %>%
  distinct(City, State, place)

# geocoding

SchoolsGeocode <- map_dfr(SchoolPlaces$place, function(x) {
  out <- opencage_forward(x, countrycode = "US", no_annotations = TRUE, limit = 1)
  out$results})

# Grabbing coordinates

SchoolsCoordinates <- SchoolsGeocode %>%
  select(place = query,
         lat = geometry.lat,
         lng = geometry.lng)

# joining coordinates back to the data

SchoolsGeocoded <- SchoolData %>%
  left_join(SchoolsCoordinates, by = "place")



#### Conference data

ConData <- ConSites %>%
  select(Organization, Event, year, start.date, end.date, Location, city, state) %>%
  mutate(place = str_c(city, ",", state))

# removing duplicates

ConPlaces <- ConData %>%
  distinct(city, state, place)

# geocoding

ConGeocode <- map_dfr(ConPlaces$place, function(x) {
  out <- opencage_forward(x, countrycode = "US", no_annotations = TRUE, limit = 1)
  out$results})

# Grabbing coordinates

ConCoordinates <- ConGeocode %>%
  select(place = query,
         lat = geometry.lat,
         lng = geometry.lng)

# joining coordinates back to the data

ConGeocoded <- ConData %>%
  left_join(ConCoordinates, by = "place")

#Again, we've got a error due to Belleville, Ontario - writing out a file to add the correct Lat/Long

write.csv(ConGeocoded, "~/ConGeocodedRough.csv")

## Read that data back in

ConGeocodedCorrected <- read.csv("~/ConGeocodedRoughCorrected.csv", stringsAsFactors = FALSE)
