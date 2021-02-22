# let's tidy things up a bit here - with Lincoln's suggestion, pull out data clean up as separate file.

# defining libraries
library(dbplyr)
library(tidyverse)
library(maps)
library(sf)
library(leaflet)
library(opencage)
library(lubridate)
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

# okay so now we've got some cleaned up datasets - need to break them back out into some year groupings. 
# Let's start with schools because there is less of them.

Schools1850 <- SchoolsGeocoded %>%
  select(School, YearFounded, place, lat, lng) %>%
  filter(YearFounded <= 1850)

Schools1855 <- SchoolsGeocoded %>%
  select(School, YearFounded, place, lat, lng) %>%
  filter(YearFounded <= 1855)

Schools1860 <- SchoolsGeocoded %>%
  select(School, YearFounded, place, lat, lng) %>%
  filter(YearFounded <= 1860)

Schools1865 <- SchoolsGeocoded %>%
  select(School, YearFounded, place, lat, lng) %>%
  filter(YearFounded <= 1865)

Schools1870 <- SchoolsGeocoded %>%
  select(School, YearFounded, place, lat, lng) %>%
  filter(YearFounded <= 1870)

Schools1875 <- SchoolsGeocoded %>%
  select(School, YearFounded, place, lat, lng) %>%
  filter(YearFounded <= 1875)

Schools1880 <- SchoolsGeocoded %>%
  select(School, YearFounded, place, lat, lng) %>%
  filter(YearFounded <= 1880)

# those dataframes are donezo. Let's do the same for Conferences
# HA! gotta change the numbers to a range (rather than cumulative)

Con1850 <- ConGeocodedCorrected%>%
  select(Organization, Event, year, start.date, end.date, Location, place, lat, lng) %>%
  filter(between(year, 1850, 1854))

Con1855 <- ConGeocodedCorrected%>%
  select(Organization, Event, year, start.date, end.date, Location, place, lat, lng) %>%
  filter(between(year, 1855, 1859))
   

Con1860 <- ConGeocodedCorrected%>%
  select(Organization, Event, year, start.date, end.date, Location, place, lat, lng) %>%
  filter(between(year, 1860, 1864))

Con1865 <- ConGeocodedCorrected%>%
  select(Organization, Event, year, start.date, end.date, Location, place, lat, lng) %>%
  filter(between(year, 1865, 1869))

Con1870 <- ConGeocodedCorrected%>%
  select(Organization, Event, year, start.date, end.date, Location, place, lat, lng) %>%
  filter(between(year, 1870, 1874))

Con1875 <- ConGeocodedCorrected%>%
  select(Organization, Event, year, start.date, end.date, Location, place, lat, lng) %>%
  filter(between(year, 1875, 1879))

Con1880 <- ConGeocodedCorrected%>%
  select(Organization, Event, year, start.date, end.date, Location, place, lat, lng) %>%
  filter(year == 1880)

# does this make sense? Selecting 1855 gives conferences between 1855-1859
# I think it makes sense.

# oof here we go, now for Missions

Missions1850 <- MissionsGeocodedCorrected %>%
  select(year, facility, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency, place, lat, lng)%>%
  filter(between(year, 1850, 1854))

Missions1855 <- MissionsGeocodedCorrected %>%
  select(year, facility, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency, place, lat, lng)%>%
  filter(between(year, 1855, 1859))

Missions1860 <- MissionsGeocodedCorrected %>%
  select(year, facility, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency, place, lat, lng)%>%
  filter(between(year, 1860, 1864))

Missions1865 <- MissionsGeocodedCorrected %>%
  select(year, facility, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency, place, lat, lng)%>%
  filter(between(year, 1865, 1869))

Missions1870 <- MissionsGeocodedCorrected %>%
  select(year, facility, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency, place, lat, lng)%>%
  filter(between(year, 1870, 1874))

Missions1875 <- MissionsGeocodedCorrected %>%
  select(year, facility, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency, place, lat, lng)%>%
  filter(between(year, 1875, 1879))

Missions1880 <- MissionsGeocodedCorrected %>%
  select(year, facility, facilityClergy, bishop, cmdmWorker, serviceRegularity, serviceFrequency, place, lat, lng)%>%
  filter(year == 1880)
