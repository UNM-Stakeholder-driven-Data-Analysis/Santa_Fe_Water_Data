#### read me ####

# the purpose of this script is to practice importing and plotting the City of Santa Fe Water Data
# need to schedule meeting with Janie to be sure that the drop off zones for water delivery system corresponds to cannabis producing 

#### libraries ####
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(erikmisc)
install.packages("MARSS")
library(MARSS)
library(nlme)
library(zoo)
install.packages("ggmap")
library("ggmap")

#### load data ####

dat = read.csv("Outside_Data /SantaFeMonthlyDataExport_050920224.csv" , header = TRUE)
options(scipen = 999)

####Data Exploration####
##examine date format##
head(dat$Month.Name)
tail(dat$Month.Name)
head(dat$Year)
tail(dat$Year)
str(dat)
sum(is.na(dat))
sum(is.na(dat_sub))

####selecting variables####

dat_sub <-
  dat %>% 
  dplyr::select(
Month.Name
    , Year
    , Zone
    , Total.Consumption..gal.
    )

#checking variable dimensions
dim(dat_sub)
str(dat_sub)

#renaming variables
dat_sub <-
  dat_sub %>%
  dplyr::rename(
      Month                               = Month.Name
    , Total_Comsumption_Gal               = Total.Consumption..gal.
   )

#Zone 3 data ONLY
dat_Zone3 = dat_sub %>%
  filter(Zone == "Zone3") %>%
  select(Month, Year, Total_Comsumption_Gal)

#### format date/time ####
strptime

#linelist <- linelist %>% 
  #mutate(onset_date = make_date(year = onset_year, month = onset_month, day = onset_day))

####test date formatting ####
#do not use (yet!)
#as.Date(paste(YYYY, MM, "01", sep("-")))
#as.Date(paste(YYYY, MM, "%b/%Y"))

#tail(dat$Sample_DateTime)
#tail(as.POSIXct(dat$Sa /'mple_DateTime, format= "%m/%d/%y %H:%M", tz="MST"))

#create new date/time col with correct format 
#dat$datetime_MST = as.POSIXct(dat$Sample_DateTime, format= "%m/%d/%y %H:%M", tz="MST")

#### plotting #####

#this code will plot the entire data set by zone (also takes an extremely long time)
#plot(dat_sub)

Zones <- ggplot(data = dat_sub, aes(x=Month, y=Total_Comsumption_Gal))+
  geom_point()+
  stat_summary(
   geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~Zone)
print(Zones)
ggsave("Zones.png", plot = Zones)

#plotting by zone

Zone1 <- ggplot(data = dat_sub, aes(x=Year, y=Total_Comsumption_Gal))+
  geom_point()+
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone1")
print(Zone1)
ggsave("Zone1.png", plot = Zone1)

Zone2 <- ggplot(data = dat_sub, aes(x=Year, y=Total_Comsumption_Gal))+
  geom_point()+
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone2")
print(Zone2)
ggsave("Zone2.png", plot = Zone2)

#plotting zone 3 due to the interesting story shown versus the other water delivery pressure zones

Zone3 <- ggplot(data = dat_sub, aes(x=Year, y=Total_Comsumption_Gal))+
  geom_point()+
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone3")
print(Zone3)
ggsave("Zone3.png", plot = Zone3)

table(dat$Site_Name)

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

####shapefile overlay####

install.packages("rinat")
library(rinat)
library(rgdal)
library(sf)
library(grid)
library(ggmap)

#sfshape <-readOGR("Shape_File_Data/WaterPressureZone.shp")
#print(sfshape)

aoi_boundary_SF <- st_read(
  "Shape_File_Data/WaterPressureZone.shp")

print(sfmap) %>%
  (geom_polygon(data = aoi_boundary_SF, mapping = aes(x=lon, y=lat, group=group)))
    
    
    
#coord_equal() %>%
#coord_map() %>%
#color= "red"))

ggplot() +
  geom_raster(data = aoi_boundary_SF, aes(x = x, y = y, fill = HARV_chmCrop)) +
  geom_sf(data = lines_HARV, color = "black") +
  geom_sf(data = aoi_boundary_HARV, color = "grey20", size = 1) +
  geom_sf(data = point_HARV, pch = 8) +
  ggtitle("NEON Harvard Forest Field Site w/ Canopy Height Model") + 
  coord_sf()




ggplot() +
  geom_sf(data = aoi_boundary_SF, aes(color = "red"), size = 1) +
  scale_color_manual(values = colors) +
  ggtitle("Boundaries") + 
  coord_sf()




print(sfmap) +
  geom_polygon(aes(x=lon, y=lat, group=group)) ,
  coord_equal() +
  data = aoi_boundary_SF ,
  color= "dark red" , alpha=.2 , linewidth=.2) 

print(sfmap) + 
  geom_sf(data = aoi_boundary_SF, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("WaterPressureZone") + 
  coord_sf() 



WPZ <- ggplot() + 
  geom_sf(data = aoi_boundary_SF, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("WaterPressureZone") + 
  coord_sf() 
print(WPZ)

print(sfmap) + geom_polygon(data= aoi_boundary_SF, aes(x = 35.6870, y = 105.9378))





####grid plot####

#library(gridExtra)
#grid.arrange(grobs = list(Zone2, Zone3), nrow=2)



ggsave("~/Desktop/2023-2024/Stakeholder Driven Analysis/Santa_Fe_Water_Data/Inside_Data")
