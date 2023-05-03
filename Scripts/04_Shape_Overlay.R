install.packages("rinat")
library(rinat)
library(rgdal)
library(sf)
library(grid)
library(ggmap)
library(terra)
library(tidyterra)

####plotting map####
register_google(key = "AIzaSyAEUCxlN7Nvje2a24PW065bblJsCNH3ekg") 
santa_fe <- geocode("Santa Fe, New Mexico") 
print.data.frame(santa_fe)

nmmap <-ggmap(get_googlemap("New Mexico", zoom = 7, maptype = "roadmap"))
print(nmmap)

nmmap_sat <-ggmap(get_googlemap("New Mexico", zoom = 7, maptype = "terrain"))
print(nmmap_sat)

sfmapsat <-ggmap(get_googlemap("Santa Fe", zoom = 12, maptype = "satellite"))
print(sfmapsat)

sfmap <-ggmap(get_googlemap("Santa Fe", zoom = 12, maptype = "terrain"))
print(sfmap)

####Shapefile Overlay####

aoi_boundary_SF <- st_read(
  "Shape_File_Data/WaterPressureZone.shp")

WPZ <- ggplot() + 
  geom_sf(data = aoi_boundary_SF, size = 3, color = "black", fill = "cyan") + 
  ggtitle("Water Pressure Zones in the city of Santa fe") + 
  coord_sf() 
print(WPZ) 

mapRaster <- rast(s)
aoi_boundary_spatve <- vect(aoi_boundary_SF)
shape_overlay<-ggplot() + geom_spatraster(data=mapRaster) + geom_spatvector(data=aoi_boundary_spatve, fill="red", lab="Zones")
print(shape_overlay)

ggsave("~/Desktop/Stakeholder Driven Analysis/Santa_Fe_Water_Data/Inside_Data")
