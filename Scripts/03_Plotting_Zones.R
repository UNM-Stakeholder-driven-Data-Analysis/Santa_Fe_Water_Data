library(tidyverse)
library(lubridate)
library(erikmisc)
library(MARSS)
library(nlme)
library(zoo)
library(lme4)
library(car) 
library(chron)
library(emmeans)

#### load data ####
dat = read.csv("Outside_Data /SantaFeMonthlyDataExport_050920224.csv" , header = TRUE)
#non-scientific notation
options(scipen = 999)

####selecting variables####
dat_sub <-
  dat %>% 
  dplyr::select(
    Month.Name
    , Year
    , Zone
    , Total.Consumption..gal.
    , Month.Name
  )

#renaming variables
dat_sub <-
  dat_sub %>%
  dplyr::rename(
    Month                               = Month.Name
    , Total_Comsumption_Gal               = Total.Consumption..gal.
  )

####Format Date/Time####
dat_sub$Month_2 =match(dat_sub$Month , month.abb)

dat_sub$Date=as.Date(paste(dat_sub$Year,dat_sub$Month_2, "01", sep="-"))

####Combining columns####
dat_sub$date%>%
  paste(dat_sub$Month, dat_sub$Year)

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

####Graphing plots by Zone####
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
  geom_jitter(alpha =  0.25/10, width = 0.25) +
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
  geom_jitter(alpha =  0.15/10, width = 0.25) +
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
  geom_jitter(alpha =  0.15/10, width = 0.25) +
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
  geom_jitter(alpha =  .10/10, width = 0.25) +
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
  geom_jitter(alpha =  .10/10, width = 0.25) +
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
  geom_jitter(alpha =  .10/10, width = 0.25) +
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
  geom_jitter(alpha =  .10/10, width = 0.25) +
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
  geom_jitter(alpha =  .10/10, width = 0.25) +
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
  geom_jitter(alpha =  10/10, width = 1) +
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

####Grid plot arrangement####
library(gridExtra)
grid_zones<-grid.arrange(grobs = list(Zone00, Zone0, Zone1, Zone2, Zone3, Zone4, Zone5, Zone6, Zone7, Zone8, Zone9), nrow=4)
ggsave("grid_zones.png", plot = grid_zones)




