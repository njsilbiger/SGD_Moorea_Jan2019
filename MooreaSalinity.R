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

# read in the conductivity data
#Cond<-read.csv('data/Bow_SN_739_010619.csv', skip = 2)
Cond<-read.csv('data/Stern_SN_740_010619.csv', skip = 2)
#colnames(Cond)<-c('x1','time','conductivity','x2','temperature','SpC','Salinity')
colnames(Cond)<-c('x1','time','conductivity','temperature','SpC','Salinity')

Cond$time<-mdy_hms(Cond$time)
#remove columns I don't want
Cond<-Cond[,c('time','temperature','Salinity')]


#read in the gps files
latlong<-readGPX('data/Track_2019-01-06 145354.gpx', metadata = TRUE, bounds = TRUE, 
        waypoints = TRUE, tracks = TRUE, routes = TRUE)
# pull out just the tracks
track<-as.data.frame(latlong$tracks[[1]])
#rename the columns
colnames(track)<-c('lon','lat','elevation','time')
# covert times to date time format
track$time<-ymd_hms(track$time)-hours(10)+seconds(1) #time zone was off

# read in second GPS
latlong2<-readGPX('data/Track_219-01-06 172811.gpx', metadata = TRUE, bounds = TRUE, 
                 waypoints = TRUE, tracks = TRUE, routes = TRUE)
# pull out just the tracks
track2<-as.data.frame(latlong2$tracks[[1]])
#rename the columns
colnames(track2)<-c('lon','lat','elevation','time')
# covert times to date time format
track2$time<-ymd_hms(track2$time)-hours(10)-seconds(7) #time zone was off

a<-rbind(track,track2)

#merge the conductivity and the tract data by time
MergedData<-left_join(a,Cond)

# remove the last few data points because the sensor was out of the water

MergedData<-MergedData[-c(1:6, 910:960),]
# remove values <10 because the sensor was likely out of the water

MergedData<-MergedData[-which(MergedData$Salinity<10),]


# bring in shape file of french polynesia
aoi_boundary_FP <- st_read(
  "data/PYF_adm/PYF_adm0.shp")

# # make an interactive map of the data
# m <- leaflet() %>%
#   +     addTiles() %>%  # Add default OpenStreetMap map tiles
#   +     addMarkers(lng=-149.82, lat=-17.48, popup="Moorea")
#  m 


 my_breaks = c(30,31,32,33,34,35,36,37)
#make a map of the data on Moorea 
FP<-ggplot() + 
  geom_sf(data = aoi_boundary_FP, size = .5, color = "black", fill = "grey") + 
  ggtitle("French Polynesia") + 
  coord_sf(xlim = c(-149.93,-149.75), ylim = c(-17.6,-17.45))+
  geom_point(data = MergedData, aes(x = lon, y = lat, color = Salinity), shape = 1)+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                           low = 'lightblue', high = 'darkblue')+
  theme_bw()


# zoom into the north shore
FP +  coord_sf(xlim = c(-149.8,-149.82), ylim = c(-17.478,-17.495))


## read in Megan and Henry's data from Jan 5th
#read in the gps files
latlong3<-readGPX('data/2019-01-05 0937__20190105_0937.gpx', metadata = TRUE, bounds = TRUE, 
                 waypoints = TRUE, tracks = TRUE, routes = TRUE)
# pull out just the tracks
track3<-as.data.frame(latlong3$tracks[[1]])
#rename the columns
colnames(track3)<-c('lon','lat','elevation','time')
# covert times to date time format
track3$time<-ymd_hms(track3$time)-hours(11) # every 30 seconds

#megan and henry cond
Cond2<-read.csv('data/MeganHenry0105.csv', skip = 2)
colnames(Cond2)<-c('x1','time','conductivity','temperature','SpC','Salinity')

Cond2$time<-mdy_hms(Cond2$time)
#remove columns I don't want
Cond2<-Cond2[,c('time','temperature','Salinity')]

# convert to 1 min level data
Cond2$by30 <- cut(Cond2$time, breaks="1 min")

Cond2_ave<-Cond2 %>%
  group_by(by30) %>% summarise_at(.vars = 1:3,.funs = function(.){mean(.,na.rm=TRUE)})%>%
  select(-time)%>%
  rename(time = by30)
Cond2_ave$time<-ymd_hms(Cond2_ave$time)


track3$by30<- cut(track3$time, breaks="1 min")
track3_ave<-track3 %>%
  group_by(by30) %>% summarise_at(.vars = 1:4,.funs = mean)%>%
  select(-time)%>%
    rename(time = by30)

track3_ave$time<-ymd_hms(track3_ave$time)
# bring the cond and track data together
MergedData2<-left_join(track3_ave,Cond2_ave)
#remove the data <10 because likely out of the water
MergedData2<-MergedData2[which(MergedData2$Salinity>10),]

## Megan and Henry day 1
#read in the gps files
latlong4<-readGPX('data/2019-01-04 1205 Moorea 1 White__20190104_1205.gpx', metadata = TRUE, bounds = TRUE, 
                  waypoints = TRUE, tracks = TRUE, routes = TRUE)
# pull out just the tracks
track4<-as.data.frame(latlong4$tracks[[1]])
#rename the columns
colnames(track4)<-c('lon','lat','elevation','time')
# covert times to date time format
track4$time<-ymd_hms(track4$time)-hours(11) # every 30 seconds

## cond day 1
Cond3<-read.csv('data/LoggerU24_740_20190104.csv', skip = 2)
colnames(Cond3)<-c('x1','time','x2','conductivity','temperature','SpC','Salinity')

Cond3$time<-mdy_hms(Cond3$time)
#remove columns I don't want
Cond3<-Cond3[,c('time','temperature','Salinity')]

# convert to 1 min level data
Cond3$by30 <- cut(Cond3$time, breaks="1 min")

Cond3_ave<-Cond3 %>%
  group_by(by30) %>% summarise_at(.vars = 1:3,.funs = function(.){mean(.,na.rm=TRUE)})%>%
  select(-time)%>%
  rename(time = by30)
Cond3_ave$time<-ymd_hms(Cond3_ave$time)

track4$by30<- cut(track4$time, breaks="1 min")
track4_ave<-track4 %>%
  group_by(by30) %>% summarise_at(.vars = 1:4,.funs = mean)%>%
  select(-time)%>%
  rename(time = by30)

track4_ave$time<-ymd_hms(track4_ave$time)
# bring the cond and track data together
MergedData4<-left_join(track4_ave,Cond3_ave)
#remove the data <10 because likely out of the water
MergedData4<-MergedData4[which(MergedData4$Salinity>10),]


#bring together all the tracks into one large dataframe
AllMerge<-rbind(MergedData,MergedData2, MergedData4)

# add the westside data to the plot
FPWest<-FP+geom_point(data = MergedData2, aes(x = lon, y = lat, color = Salinity), 
              shape = 1)

FPWest +  coord_sf(xlim = c(-149.93,-149.85), ylim = c(-17.48,-17.58))

# remove the NA data
AllMerge<-AllMerge[-which(is.na(AllMerge$Salinity)),]


# make an image of all the salinity data
my_breaks = c(10,20,30,33,35,37)

FP2<-ggplot() + 
  geom_sf(data = aoi_boundary_FP, size = .5, color = "black", fill = "grey") + 
  ggtitle("Mo'orea") + 
  coord_sf(xlim = c(-149.93,-149.75), ylim = c(-17.6,-17.45))+
  geom_point(data = AllMerge, aes(x = lon, y = lat, color = Salinity), pch = 19, shape = 1, size=2)+
  theme_bw()+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                       low = 'lightblue', high = 'darkblue')

  
#  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
 #                        low = 'lightblue', high = 'darkblue')
  
#FP2 + coord_sf(xlim = c(-149.93,-149.85), ylim = c(-17.52,-17.56))+
 # scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
  #                       low = 'lightblue', high = 'darkblue')


# find all the data <30 ppt and zoom in and make a set of maps
less32<-which(AllMerge$Salinity<32)

W1<-FP2 + ggtitle('West 1')+
  new_scale_color() +
  coord_sf(xlim = c(-149.93,-149.85), ylim = c(-17.52,-17.56))+
  geom_point(data = AllMerge, aes(x = lon, y = lat, color = Salinity), pch = 19, shape = 1, size=2)+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                         low = 'lightblue', high = 'darkblue')

my_breaks = c(31,33,35,37)
N2<-FP2 +  ggtitle('North 2')+
  new_scale_color() +
  coord_sf(xlim = c(-149.88,-149.91), ylim = c(-17.487,-17.495))+
  geom_point(data = AllMerge, aes(x = lon, y = lat, color = Salinity), pch = 19, shape = 1, size=2)+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                         low = 'lightblue', high = 'darkblue', limits = c(31,37))

my_breaks = c(31,33,35,37)
# bring in the radon data and match the point where there was detection
radon1<-read.csv('data/radon/Output_190106.csv')
# make the date and it was off by an hour
radon1$Full.Date<-mdy_hm(radon1$Full.Date) - hours(1) + minutes(3)
#only pull out data needed
radon1<-radon1[,c('Full.Date','Mean.Radon..DPM.L.')]
colnames(radon1)[1]<-'time'
# pull out the timepoint with a radon detection (>1) and match with appropriate GPS point
detect<-which(radon1$Mean.Radon..DPM.L.>1)
radon1[detect,]

# average the AllMerge by minute so it will match with the radon
AllMerge$by30<- cut(AllMerge$time, breaks="1 min")
AllMerge_ave<-AllMerge %>%
  group_by(by30) %>% summarise_at(.vars = 1:6,.funs = mean)%>%
  select(-time)%>%
  rename(time = by30)
AllMerge_ave$time<-ymd_hms(AllMerge_ave$time)


# join with the one radon detection
radpoint<-left_join(radon1[detect,],AllMerge_ave)

North1<-FP2 + 
  ggtitle('North 1')+
  new_scale_color() + 
  coord_sf(xlim = c(-149.808,-149.82), ylim = c(-17.481,-17.485))+
  geom_point(data = AllMerge, aes(x = lon, y = lat, color = Salinity), pch = 19, shape = 1, size=2)+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                         low = 'lightblue', high = 'darkblue', limits = c(31,37))+
  new_scale_color()+ # add the radon data
  geom_point(data = radpoint,aes(x=lon, y=lat, col = 'red'),
             pch = 17, shape = 1, size = 5, show.legend = FALSE )
  #+ scale_color_manual(values = "red",label = "", name = "Radon Detection")

  

my_breaks = c(10,20,31,33,35,37)
West2<-FP2 + 
  ggtitle('West 2')+
  new_scale_color() + 
  coord_sf(xlim = c(-149.898,-149.9049), ylim = c(-17.53373,-17.545))+
  geom_point(data = AllMerge, aes(x = lon, y = lat, color = Salinity), pch = 19, shape = 1, size=2)+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                         low = 'lightblue', high = 'darkblue', limits = c(10,37))


## get the color scale back for the original image 
FP2<-ggplot() + 
  geom_sf(data = aoi_boundary_FP, size = .5, color = "black", fill = "grey") + 
  ggtitle("Mo'orea") + 
  coord_sf(xlim = c(-149.93,-149.75), ylim = c(-17.6,-17.45))+
  geom_point(data = AllMerge, aes(x = lon, y = lat, color = Salinity), pch = 19, shape = 1, size=2)+
  theme_bw()+
  scale_color_continuous(breaks = my_breaks, labels = my_breaks, 
                         low = 'lightblue', high = 'darkblue')


Allplots<-ggarrange(FP2,                                                 # First row with scatter plot
          ggarrange(North1, N2,  ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          ggarrange(W1, West2,  ncol = 2, labels = c("D", "E")),
          nrow = 3, 
          labels = "A",# Labels of the scatter plot
          heights = c(2, 1, 1),
          widths = c(2,1,1)
) 

ggsave(filename ='output/SalinityMap.pdf' ,plot = Allplots, device = 'pdf', width = 10, height = 10)
