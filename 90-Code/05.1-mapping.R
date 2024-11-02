##############################
### mapping practice 5.1
##############################
### install packages of "rgbif" and "maps"
#install.packages("rgbif")
#install.packages("maps")
library(ggplot2)
library(rgbif)
library(dplyr)
library(leaflet)
rm(list=ls())
# dir create
map_dir<- "C://ocean5098//05-mapping_practice"
if (dir.exists(map_dir) == FALSE)
{
  dir.create(map_dir)
}

# Get data for Common House Sparrow
species_data <- occ_search(scientificName = "Isoetes taiwanensis",
                           hasCoordinate=T,
                           basisOfRecord='HUMAN_OBSERVATION',
                           limit = 1000) 
head(species_data$data)

# Create an interactive map
leaflet(cleaned_data) %>%
  addTiles() %>%
  addCircles(lng = ~decimalLongitude, lat = ~decimalLatitude, 
             radius = 50, 
             popup = ~paste(scientificName))

