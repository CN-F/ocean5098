##############################
### mapping practice 5.1
##############################
### install packages of "rgbif" and "maps"
#install.packages("rgbif")
#install.packages("maps")
library(ggplot2)
library(rgbif)
library(dplyr)
rm(list=ls())
# Get data for Common House Sparrow
species_data <- occ_data(scientificName = "Isoetes taiwanensis", 
                         limit = 1000)
# Check the data
head(species_data$data)
# Clean the data
cleaned_data <- species_data$data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

# Create a static map
ggplot(cleaned_data, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point(alpha = 0.5) +
  borders("world") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Distribution of Common House Sparrow (Passer domesticus)")
library(leaflet)

# Create an interactive map
leaflet(cleaned_data) %>%
  addTiles() %>%
  addCircles(lng = ~decimalLongitude, lat = ~decimalLatitude, 
             radius = 50, 
             popup = ~paste(scientificName))

