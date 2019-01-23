# Measuring saliity and RAD at LTER 6 spatial and temporal

library(maptools)
library(tidyverse)
library(plotKML)
library(lubridate)
library(RColorBrewer)
library(sf)
library(gridExtra)
library(grid)
library(ggmap)
library(smooth)
library(ggnewscale)
library(ggpubr)
library(legendMap)
library(oce)


## read in the salinity data
Salinity<-read.csv('data/LTER6wRADSalinity.csv', skip = 2)
colnames(Salinity)<-c('x1','time','condHI','condlow','temperature','spC','Salinity')
#remove columns I don't want
Salinity<-Salinity[,c('time','temperature','Salinity')]
Salinity$time<-mdy_hms(Salinity$time)

## read in the GPS data
#read in the gps files
latlong<-readGPX('data/Track with RAD 010919.gpx', metadata = TRUE, bounds = TRUE, 
                 waypoints = TRUE, tracks = TRUE, routes = TRUE)
# pull out just the tracks
track<-as.data.frame(latlong$tracks[[1]])
#rename the columns
colnames(track)<-c('lon','lat','elevation','time')
# covert times to date time format
track$time<-ymd_hms(track$time)-hours(10)+seconds(5) #time zone was off

#Read in tidal predictions
tides<-read.table('data/tidepredicts.txt', skip = 14)
colnames(tides)<-c('date','day','time' ,'AMPM','tide')
tides$datetime<-paste(tides$date, tides$time, tides$AMPM)
tides$datetime<-ymd_hm(tides$datetime)

#Read in the Radon Data
radon<-read.csv('data/radon/LTER 6 Survey_RAD.csv')
radon<-radon[,c(2,49)]
colnames(radon)<-c('time','radon_mean') #radon is in DPM per L
radon$time<-mdy_hm(radon$time)- hours(1) + minutes(2)

### Merge the spatial data

SpatialData<-left_join(track,Salinity)
# remove salinity data <5 because out of water
SpatialData<-SpatialData[SpatialData$Salinity>5,]

# make a dataframe of radon data and 5 min averaged salinity and gps data

SpatialData$by5<- cut(SpatialData$time, breaks="6 min")
Spatial_Ave<-SpatialData %>%
  group_by(by5) %>% summarise_at(.vars = 1:6,.funs = mean)%>%
  select(-time)%>%
  rename(time = by5)
Spatial_Ave$time<-ymd_hms(Spatial_Ave$time)

Spatial_rad<-left_join(radon,Spatial_Ave)
# bad data points were anything before 13:10, at 15:15
Spatial_rad<-Spatial_rad[Spatial_rad$time>mdy_hm('01/09/2019 13:10'),]
Spatial_rad<-Spatial_rad[-c(Spatial_rad$time==mdy_hm('01/09/2019 15:15')),]


## read in the pharmacie data for comparison
pharmacie<-read.csv("data/pharmacie_rad.csv")
colnames(pharmacie)[3]<-c('radon_mean')
pharmacie$X<-NULL
pharmacie$time<-ymd_hms(pharmacie$time)

# read in all the kayak salininty data
AllSalinity<-read.csv('output/AllMerge.csv')
AllSalinity$X<-NULL
AllSalinity$time<-ymd_hms(AllSalinity$time)
#merge it with the LTER 6 spatial data
SpatialData$by5<-NULL
SpatialData$time<-ymd_hms(SpatialData$time)
AllSalinity<-rbind(AllSalinity,SpatialData)

#combine the LTER 6 and pharmacie data
Spatial_rad<-rbind(Spatial_rad, pharmacie)
# if rad <1 make 0 because that is undetectable
#Spatial_rad$radon_mean[Spatial_rad$radon_mean<1]<-0

# convert DPM L-1 to m-3
#Spatial_rad$radon_mean<-Spatial_rad$radon_mean*1000

# bring in shape file of french polynesia
aoi_boundary_FP <- st_read(
  "data/PYF_adm/PYF_adm0.shp")
my_breaks = c(15,20,25,30,35)
#make a map of the data on Moorea 
Moorea<-ggplot() + 
  geom_sf(data = aoi_boundary_FP, size = .5, color = "black", fill = "grey") + 
  ggtitle("Mo'orea") + 
  ylab("Longitude")+
  xlab("Latitude")+
  coord_sf(xlim = c(-149.93,-149.75), ylim = c(-17.6,-17.45))+
 # coord_sf(xlim = c(-149.915,-149.912), ylim = c(-17.520,-17.514))+
  geom_point(data = AllSalinity, aes(x = lon, y = lat, color = Salinity), pch = 19,size = 2, shape = 1)+
  #geom_point(data = SpatialData, aes(x = lon, y = lat, color = Salinity), pch = 19,size = 2, shape = 1)+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                         low = 'yellow', high = 'darkblue')+
  theme_bw()+
  geom_point(data = Spatial_rad, aes(x = lon, y = lat,size = radon_mean), bg = 'red',pch = 21, shape = 1)+
  labs(size=expression("Radon DPM M"^{-3}))+
#  scale_size_area(limits = c(0, 10000), breaks = c(0, 1000, 3000, 5000, 7000, 10000))+
  ggsn::scalebar(x.min = -149.93, x.max = -149.87,
                y.min = -17.59,  y.max = -17.58, dist = 2.5, st.dist = 0.8,
                st.size=4, height=0.5, dd2km = TRUE, model = 'WGS84')
# add the radon data
#LTER6_rad<-LTER6 +

LTER6<-Moorea+
  #coord_sf(xlim = c(-149.915,-149.912), ylim = c(-17.520,-17.514))+
  coord_sf(xlim = c(-149.918,-149.912), ylim = c(-17.520,-17.514))+
    ggtitle("LTER 6") +
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(-149.918, -149.912, by = 0.003))+
  ggsn::scalebar(x.min = -149.918, x.max = -149.915,
                 y.min = -17.5195,  y.max = -17.519, dist = .15, st.dist = 0.8,
                 st.size=2, height=0.5, dd2km = TRUE, model = 'WGS84')

## Pharmacie
North<-Moorea+coord_sf(xlim = c(-149.808,-149.82), ylim = c(-17.481,-17.485))+
  ggtitle("Pharmacie")+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq( -149.82,-149.808, by = 0.003))+
  ggsn::scalebar(x.min = -149.808, x.max = -149.82,
                 y.min = -17.4835,  y.max = -17.4845, dist = .15, st.dist = 0.2,
                 st.size=2, height=0.2, dd2km = TRUE, model = 'WGS84')

#LTER6+coord_sf(xlim = c(-149.915,-149.912), ylim = c(-17.520,-17.514))

  
Allplots<-ggarrange(Moorea,                                                 # First row with scatter plot
                    ggarrange(LTER6,North, ncol  = 2, #labels = c("B","C"),
                              heights = c(4,4), widths = c(4,4)), # Second row with box and dot plots
                    nrow = 3, 
                    #labels = "A",# Labels of the scatter plot
                    heights = c(6, 4),
                    widths = c(6,4)
)
  
  
ggsave('output/SpatialRadon.pdf', plot = Allplots, device = 'pdf', width = 8, height = 12, useDingbats=FALSE)

## 24 hour salinity data

#started at 5:30pm on 1/9/16 ended ~ 10am

TimeSeries<-Salinity[Salinity$time>mdy_hm('01/09/2019 17:30') & Salinity$time<mdy_hm('01/10/2019 10:00') ,]
tides<-tides[tides$datetime>mdy_hm('01/09/2019 17:30') & tides$datetime<mdy_hm('01/10/2019 10:00') ,]

# make a plot
sunrise<-mdy_hm('01/10/2019 5:33 AM')
sunset<-mdy_hm('01/09/2019 6:39 PM')

pdf('output/LTERTimeSeries.pdf', width = 8, height = 6, useDingbats = FALSE)
par(mar = c(5.1,4.1,4.1,9.1))
plot(TimeSeries$time, TimeSeries$Salinity, type = 'l', ylab = 'Salinity', xlab = 'Time', ylim =c(20,35))
par(new = T)
with(TimeSeries, plot(time, temperature,type = 'l', col = 'blue', axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Temperature')
par(new = T)
with(tides, plot(datetime, tide,type = 'l', lty = 2, axes=F, xlab=NA, ylab=NA, ylim = c(-.1,0.3)))
axis(4,lwd=2,line=5)
mtext(side = 4, line = 8, 'Tide (m)', ylim = c(-.1,0.3))
text(sunrise,0.3, 'Sunrise')
text(sunset,0.3, 'Sunset')

legend("bottomleft",legend = c('Temperature', 'Salinity','Tides'), lty=c(1,1,2), col=c("blue", "black",'black'), bty = 'n')
dev.off()

## read in the timeseries from gump
radon_gump<-read.csv('data/radon/Output_190105_gump.csv')
radon_gump$Full.Date<-mdy_hm(radon_gump$Full.Date)

# read in the tide data
tide_gump<-read.csv('data/radon/tide_190105_gump.csv')
tide_gump$Time<-mdy_hm(tide_gump$Time)

# read in the salinity data
cond_gump<-read.csv('data/radon/cond_190105_gump.csv')
cond_gump$datetime<-mdy_hm(cond_gump$datetime)
# convert conductivity and temp to salinity
cond_gump$Salinity<-swSCTp(conductivity = cond_gump$Conductivity/1000,temperature = cond_gump$Temperature, 
       conductivityUnit =  "mS/cm" )

pdf('output/timeseries_gump.pdf',6,6, useDingbats = FALSE)
par(mar = c(5.1,4.1,4.1,5.1))
#plot(cond_gump$datetime, cond_gump$Salinity, type = 'l', ylab = 'Salinity', xlab = 'Time', ylim =c(28,30))
#par(new = T)
#with(cond_gump, plot(datetime, Temperature,type = 'l', col = 'blue', axes=F, xlab=NA, ylab=NA, cex=1.2))
plot(radon_gump$Full.Date, radon_gump$Mean.Radon..DPM.L.,type = 'l',lwd = 2,main = '24 hr time series at Gump Station', col = 'blue', xlab='Time', ylab='Radon DM/L', cex=1.2)
#axis(side = 4)
#mtext(side = 4, line = 3, 'Radon DM/L')
par(new = T)
with(tide_gump, plot(Time, tide.level..m.,type = 'l', lty = 2, axes=F, xlab=NA, ylab=NA, ylim = c(-.1,0.4)))
axis(side=4)
mtext(side = 4, line = 3,'Tide (m)', ylim = c(-.1,0.4))
legend("topleft",legend = c('Radon', 'Tides'), lty=c(1,2), col=c("blue", 'black'), bty = 'n')
dev.off()