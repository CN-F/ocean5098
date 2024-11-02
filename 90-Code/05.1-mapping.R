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
leaflet(species_data$data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = ~decimalLongitude, 
    lat = ~decimalLatitude, 
    radius = 100,            # 增加圓圈半徑
    color = "blue",          # 圓圈顏色
    fillOpacity = 0.5,       # 填充透明度
    stroke = TRUE,           # 啟用邊框
    weight = 1,              # 邊框粗細
    popup = ~paste(scientificName)  # 設置彈出內容
  ) %>%
  addScaleBar(position = "bottomright") %>%
  addMeasure() %>%
  # 使用 HTML 代碼直接設置控制標題
  addControl("<strong>Species Distribution Map</strong>", position = "topright", 
             className = "leaflet-control-custom")
