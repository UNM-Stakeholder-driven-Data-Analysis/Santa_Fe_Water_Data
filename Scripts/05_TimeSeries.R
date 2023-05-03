####TimeSeries Analysis####
ts <- zoo(dat_zone0, Date)


timeseries<-ts(data=dat_zone0$Date, start = "2017-10-01", end = "2021-01-01", frequency = 12)
print(timeseries)
