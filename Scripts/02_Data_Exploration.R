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
library(car) # for Anova(), vif()
library(chron)
library(emmeans)
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
head(dat_sub$Date)
tail(dat_sub$Date)

####checking variable dimensions####
dim(dat_sub)
str(dat_sub)

####Means####
##This will run means per month, and total consumption
#M1<-mean(dat_sub[dat_sub$Month_2 == "1", "Total_Comsumption_Gal"])
#M2<-mean(dat_sub[dat_sub$Month_2 == "2", "Total_Comsumption_Gal"])
#M3<-mean(dat_sub[dat_sub$Month_2 == "3", "Total_Comsumption_Gal"])
#M4<-mean(dat_sub[dat_sub$Month_2 == "4", "Total_Comsumption_Gal"])
#M5<-mean(dat_sub[dat_sub$Month_2 == "5", "Total_Comsumption_Gal"])
#M6<-mean(dat_sub[dat_sub$Month_2 == "6", "Total_Comsumption_Gal"])
#M7<-mean(dat_sub[dat_sub$Month_2 == "7", "Total_Comsumption_Gal"])
#M8<-mean(dat_sub[dat_sub$Month_2 == "8", "Total_Comsumption_Gal"])
#M9<-mean(dat_sub[dat_sub$Month_2 == "9", "Total_Comsumption_Gal"])
#M10<-mean(dat_sub[dat_sub$Month_2 == "10", "Total_Comsumption_Gal"])
#M11<-mean(dat_sub[dat_sub$Month_2 == "11", "Total_Comsumption_Gal"])
#M12<-mean(dat_sub[dat_sub$Month_2 == "12", "Total_Comsumption_Gal"])
 
####nlme model####
# create the linear model
TWC_Zone <- lm(meanYZ ~ Zone, data = DF, na.action=na.omit)
# check assumptions
plot(TWC_Zone)
# run type 3 ANOVA
Anova(TWC_Zone, type = 3)
#post-hoc test
DF %>%
  select(meanYZ, Zone, Year) %>%
  drop_na() %>%
  ggplot(aes(x = Year, y = meanYZ)) + 
  geom_boxplot() + facet_grid(~Year)

