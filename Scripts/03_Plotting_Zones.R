####Graphing plots by Zone####
Zones <- ggplot(data = dat_zones, aes(x=Month, y=Total_Comsumption_Gal))+
  geom_point()+
  geom_boxplot()+
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3
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

####Grid plot####
library(gridExtra)
grid_zones<-grid.arrange(grobs = list(Zone00, Zone0, Zone1, Zone2, Zone3, Zone4, Zone5, Zone6, Zone7, Zone8, Zone9), nrow=4)
ggsave("grid_zones.png", plot = grid_zones)




