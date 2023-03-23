#### read me ####

# the purpose of this script is to practice importing and plotting the City of Santa Fe Water Data 

#### libraries ####

library(tidyverse)
library(lubridate)

#### load data ####

dat = read.csv("Outside_Data /SantaFeMonthlyDataExport_050920224.csv" , header = TRUE)
