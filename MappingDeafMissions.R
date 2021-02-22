# okay, let's rewind and reset here. We've got a messy data set with various types of information.
# lets break out maps that do a few different things.
# Map 1: contrasts Missions and railways/conferences/schools
# Map 2: explores changes in servicefrequency
# Map 3: explores workers

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

annualMissions <- read.csv("~/CMDM/TidierAnnualData.csv", stringsAsFactors = FALSE)
ResDeafSchools <- read.csv("~/CMDM/ResDeafSchools.csv", stringsAsFactors = FALSE)
ConSites <- read.csv("~/CMDM/Conventions_ConferenceSitesto1880.csv", stringsAsFactors = FALSE)

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

## huzzah that we've got them. 
#let's map those to make sure they worked.

leaflet(SchoolsGeocoded) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = '#A6786D',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>") )

leaflet(ConGeocodedCorrected) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>") )

leaflet(MissionsGeocodedCorrected) %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity) )

## each of those is mapping correctly. There are a few questions here - one observation in New Jersey is missing lat/long data
## Lets put these things together

leaflet(MissionsGeocodedCorrected) %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity) ) %>%
  addCircleMarkers(data = SchoolsGeocoded,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#A6786D',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"), 
                   group = "Deaf Schools") %>%
  addCircleMarkers(data = ConGeocodedCorrected,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "Conference Sites") %>%
  addLayersControl(
    overlayGroups = c("Deaf Schools", "Conference Sites"),
    options = layersControlOptions(collapsed = FALSE))

## Okay, that gives me a map, with all missions, all schools, all conference sites 
# with the option of toggling schools and conferences on and off
# lets throw the railroad content back on there again.


##rr1840 <- read_sf("~/CMDM/USrailshps/RR1840/RR1840WGS84.shp")
##rr1845 <- read_sf("~/CMDM/USrailshps/RR1845/RR1845WGS84.shp")
rr1850 <- read_sf("~/CMDM/USrailshps/RR1850/RR1850WGS84.shp")
rr1855 <- read_sf("~/CMDM/USrailshps/RR1855/RR1855WGS84.shp")
rr1860 <- read_sf("~/CMDM/USrailshps/RR1861/RR1861WGS84.shp")
rr1865 <- read_sf("~/CMDM/USrailshps/RR1861/RR1861WGS84.shp")
rr1870 <- read_sf("~/CMDM/USrailshps/RR1870/RR1870WGS84.shp")
rr1875 <- read_sf("~/CMDM/USrailshps/RR1870/RR1870WGS84.shp")
rr1880 <- read_sf("~/CMDM/USrailshps/RR1870/RR1870WGS84.shp")

#tested to make sure these all mapped  
# okay let's try and make groups here.

leaflet() %>%
  addTiles() %>%
  addPolylines(data = rr1850, group = "Railways 1850-1854") %>%
  addPolylines(data = rr1855, group = "Railways 1855-1860") %>%
  addPolylines(data = rr1861, group = "Railways 1861-1869") %>%
  addPolylines(data = rr1870, group = "Railways 1870-1880") %>%
  
  addLayersControl(
    baseGroups = c("Railways 1840-1844", "Railways 1845-1849", "Railways 1850-1854", "Railways 1855-1860", "Railways 1861-1869", "Railways 1870-1880"),
    options = layersControlOptions(collapsed = FALSE)
  )

# why won't 1870 work?
## omitting 1870 for now. Let's put things together.


leaflet(MissionsGeocodedCorrected) %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity) ) %>%
  addCircleMarkers(data = SchoolsGeocoded,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#A6786D',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"), 
                   group = "Deaf Schools") %>%
  addCircleMarkers(data = ConGeocodedCorrected,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "Conference Sites") %>%
  addPolylines(data = rr1850, 
               color = "grey",
               weight = 2,
               group = "Railways 1850-1854") %>%
  addPolylines(data = rr1855,
               color = "grey",
               weight = 2,
               group = "Railways 1855-1860") %>%
  addPolylines(data = rr1861, 
               color = "grey",
               weight = 2,
               group = "Railways 1861-1869") %>%
  addPolylines(data = rr, 
               color = "grey",
               weight = 2,
               group = "RR") %>%
  addLayersControl(
    baseGroups = c("Railways 1850-1854", "Railways 1855-1860", "Railways 1861-1869", "RR"),
    overlayGroups = c("Deaf Schools", "Conference Sites"),
    options = layersControlOptions(collapsed = FALSE))

## lost the work below on slider content
## Let's start over with the layers again - using the new dataframes


leaflet() %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addCircleMarkers(data = Missions1850,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity),
                   group = "1850-1854") %>%
  addCircleMarkers(data = Missions1855,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity),
                   group = "1855-1859") %>%
  addCircleMarkers(data = Missions1860,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity),
                   group = "1860-1864") %>%
  addCircleMarkers(data = Missions1865,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity),
                   group = "1865-1869") %>%
  addCircleMarkers(data = Missions1870,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity),
                   group = "1870-1874") %>%
  addCircleMarkers(data = Missions1875,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity),
                   group = "1875-1879") %>%
  addCircleMarkers(data = Missions1880,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#2A3E59',
                   weight = 1,
                   radius = 4,
                   popup = ~paste("<b> Place: </b>", facility, "<br>",
                                  "<b> Location: </b>", place, "<br>",
                                  "<b> Mission Worker: </b>", cmdmWorker, "<br>",
                                  "<b> Rector: </b>", facilityClergy, "<br>",
                                  "<b> Bishop: </b>", bishop, "<br>",
                                  "<b> Service Regularity: </b>", serviceRegularity),
                   group = "1880") %>%
  addCircleMarkers(data = Schools1850,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#8C4F2B',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1850-1854") %>%
  addCircleMarkers(data = Schools1855,
                 lng = ~lng, 
                 lat = ~lat, 
                 color = '#8C4F2B',
                 weight = 1,
                 radius = 5,
                 popup = ~paste("<b> School: </b>", School, "<br>",
                                "<b> Founded: </b>", YearFounded, "<br>",
                                "<b> Location: </b>", place, "<br>"), 
                 group = "1855-1859") %>%
  addCircleMarkers(data = Schools1860,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#8C4F2B',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"), 
                   group = "1860-1864") %>%
  addCircleMarkers(data = Schools1865,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#8C4F2B',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"), 
                   group = "1865-1869") %>%
  addCircleMarkers(data = Schools1870,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#8C4F2B',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"), 
                   group = "1870-1874") %>%
  addCircleMarkers(data = Schools1875,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#8C4F2B',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"), 
                   group = "1875-1879") %>%
  addCircleMarkers(data = Schools1880,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#8C4F2B',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> School: </b>", School, "<br>",
                                  "<b> Founded: </b>", YearFounded, "<br>",
                                  "<b> Location: </b>", place, "<br>"), 
                   group = "1880") %>%
  addCircleMarkers(data = Con1850,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1850-1854") %>%
  addCircleMarkers(data = Con1855,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1855-1860") %>%
  addCircleMarkers(data = Con1860,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1860-1864") %>%
  addCircleMarkers(data = Con1865,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1865-1869") %>%
  addCircleMarkers(data = Con1870,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1870-1874") %>%
  addCircleMarkers(data = Con1875,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1875-1879") %>%
  addCircleMarkers(data = Con1880,
                   lng = ~lng, 
                   lat = ~lat, 
                   color = '#D96262',
                   weight = 1,
                   radius = 5,
                   popup = ~paste("<b> Event: </b>", Event, "<br>",
                                  "<b> Organization: </b>", Organization, "<br>",
                                  "<b> Start Date: </b>", start.date, "<br>",
                                  "<b> End Date: </b>", end.date, "<br>",
                                  "<b> Event Location: </b>", Location, "<br>",
                                  "<b> Location: </b>", place, "<br>"),
                   group = "1880") %>%
  addPolylines(data = rr1850, 
               color = "grey",
               weight = 2,
               group = "1850-1854") %>%
  addPolylines(data = rr1855,
               color = "grey",
               weight = 2,
               group = "1855-1859") %>%
  addPolylines(data = rr1860, 
               color = "grey",
               weight = 2,
               group = "1860-1864") %>%
  addPolylines(data = rr1865, 
               color = "grey",
               weight = 2,
               group = "1865-1869") %>%
 ## addPolylines(data = rr1870, 
   ##            color = "grey",
     ##          weight = 2,
        ##       group = "1870-1875") %>%
  ## addPolylines(data = rr1875, 
     ##          color = "grey",
     ##          weight = 2,
     ##          group = "1875-1879") %>%
##  addPolylines(data = rr1880, 
   ##            color = "grey",
     ##          weight = 2,
     ##          group = "1880") %>%
  addLayersControl(
    overlayGroups = c("1850-1854", "1855-1859", "1860-1864", "1865-1869", "1870-1874", "1875-1879", "1880"),
    options = layersControlOptions(collapsed = FALSE))


## two new problems: 1870 polylines break everything again.
## for some reason con1855 continues to appear.
#1880 looks awkward as layers control.

