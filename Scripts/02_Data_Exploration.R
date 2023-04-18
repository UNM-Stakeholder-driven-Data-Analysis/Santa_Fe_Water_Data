#### read me ####

# the purpose of this script is to practice importing and plotting the City of Santa Fe Water Data
# need to schedule meeting with Janie to be sure that the drop off zones for water delivery system corresponds to cannabis producing 

#### libraries ####
library(tidyverse)
library(lubridate)
library(erikmisc)
library(MARSS)
library(nlme)
library(zoo)
library(lme4)
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

####Water use by zone ONLY####
dat_zone00 = dat_sub[dat_sub$Zone=="ZONE 00",]
dat_zone0 = dat_sub[dat_sub$Zone=="ZONE 0",]
dat_zone1 = dat_sub[dat_sub$Zone=="ZONE 1",]
dat_zone2 = dat_sub[dat_sub$Zone=="ZONE 2",]
dat_zone3 = dat_sub[dat_sub$Zone=="ZONE 3",]
dat_zone4 = dat_sub[dat_sub$Zone=="ZONE 4",]
dat_zone5 = dat_sub[dat_sub$Zone=="ZONE 5",]
dat_zone6 = dat_sub[dat_sub$Zone=="ZONE 6",]
dat_zone7 = dat_sub[dat_sub$Zone=="ZONE 7",]
dat_zone8 = dat_sub[dat_sub$Zone=="ZONE 8",]
dat_zone9 = dat_sub[dat_sub$Zone=="ZONE 9",]

#####Graphing plots by Zone####
Zones <- ggplot(data = dat_zones, aes(x=Month, y=Total_Comsumption_Gal))+
  geom_point()+
  geom_boxplot()+
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

####Plotting by different zones####
Zone00 <- ggplot(data = dat_zone00, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone00")
print(Zone00)
ggsave("Zone00.png", plot = Zone00)

Zone0 <- ggplot(data = dat_zone0, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone0")
print(Zone0)
ggsave("Zone0.png", plot = Zone0)

Zone1 <- ggplot(data = dat_zone1, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
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

Zone2 <- ggplot(data = dat_zone2, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
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


Zone3 <- ggplot(data = dat_zone3, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
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

Zone4 <- ggplot(data = dat_zone4, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone4")
print(Zone4)
ggsave("Zone4.png", plot = Zone4)

Zone5 <- ggplot(data = dat_zone5, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone5")
print(Zone5)
ggsave("Zone5.png", plot = Zone5)

Zone6 <- ggplot(data = dat_zone6, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone6")
print(Zone6)
ggsave("Zone6.png", plot = Zone6)

Zone7 <- ggplot(data = dat_zone7, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+
  geom_jitter(alpha =  1/10, width = 0.25) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone7")
print(Zone7)
ggsave("Zone7.png", plot = Zone7)

Zone8 <- ggplot(data = dat_zone8, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+ 
  geom_jitter(alpha =  1/10, width = 0.25) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone8")
print(Zone8)
ggsave("Zone8.png", plot = Zone8)

 Zone9 <- ggplot(data = dat_zone9, aes(x=Year, y=Total_Comsumption_Gal))+
  scale_y_log10() +
  geom_point()+ 
   geom_jitter(alpha =  7/10, width = 1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  facet_wrap(~"Zone9")
print(Zone9)
ggsave("Zone9.png", plot = Zone9)

####Grid plot####
library(gridExtra)
grid_zones<-grid.arrange(grobs = list(Zone00, Zone0, Zone1, Zone2, Zone3, Zone4, Zone5, Zone6, Zone7, Zone8, Zone9), nrow=4)
ggsave("grid_zones.png", plot = grid_zones)

####nlme model####



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
install.packages("rinat")
library(rinat)
library(rgdal)
library(sf)
library(grid)
library(ggmap)
library(terra)
library(tidyterra)

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
