####TimeSeries Analysis####
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

####TimeSeries Analysis####
#This will take the mean of the year, zone, and total consumption
DF<-dat_sub%>%group_by(Year, Zone)%>%summarise(meanYZ = mean(Total_Comsumption_Gal))

#This code will plot the time series of different zones on a log10 scale
TS<-ggplot(DF, aes(x=Year, y=meanYZ, color=Zone))+
  scale_y_log10()+
  geom_path()+
  geom_point()+
  ggtitle("Water Consumption in Santa Fe New Mexico by Zone")+
  labs(y = "Water Consumption (log10)", x = "Year")
print(TS)
