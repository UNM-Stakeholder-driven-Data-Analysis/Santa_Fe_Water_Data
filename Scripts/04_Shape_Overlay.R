####libraries####
library(rinat)
library(rgdal)
library(sf)
library(grid)
library(ggmap)
library(terra)
library(tidyterra)
library(colorspace)
library(maptiles)
library("ggmap")

####plotting map of Santa Fe####
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

#Water Pressure Zones Shape Overlay
WPZ <- ggplot() + 
  geom_sf(data = aoi_boundary_SF, size = 3, color = "black", fill = "cyan") + 
  ggtitle("Water Pressure Zones in the city of Santa fe") + 
  coord_sf() 
print(WPZ) 

#Water Pressure Zones Colored
ColorWPZ<- ggplot(aoi_boundary_spatve) +
  geom_spatvector(aes(fill = Zone), color = "white") +
  geom_spatvector_text(aes(x=Shape_Area, y=Shape_Leng, label = Zone), color = "black") +
  labs(y = "Latitude", x = "Longitude")
print(ColorWPZ)

#Shapefile Overlay on the City of Santa Fe 

mapRaster <- rast(s)

aoi_boundary_spatve <- vect(aoi_boundary_SF)
shape_overlay<-ggplot() +
    geom_spatraster(data=mapRaster, aes()) +
    geom_spatvector(data=aoi_boundary_spatve, alpha=0.1) + 
    ggtitle("Shapefile Overlay on the City of Santa Fe")
print(shape_overlay)

ggsave("~/Desktop/Stakeholder Driven Analysis/Santa_Fe_Water_Data/Inside_Data")
