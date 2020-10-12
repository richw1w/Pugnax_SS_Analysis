###########Map##########
install.packages(c("tidyverse", "maps", "mapdata", "ggrepel"))
library(ggplot2, ggmap, maps, ggrepel, mapdata)

#registering API key and saving
register_google(key = "AIzaSyDOaM1E7DIg1o1Q528uMXrbTyj99YyknwE", write = TRUE)

#loading data
Sites <- read.csv("Collection_Sites.csv")

#creating a frame for the map
box <- make_bbox(lon = Sites$lon, lat = Sites$lat, f = .1)
pointmean <- sapply(Sites[2:3], mean)
pointmean #center of the map

#getting the map from google
Site_map <- get_googlemap (center = c(lon = -70.43948, lat = 42.02006), #this is the center of the map (pointmean)
                                  maptype = "satellite", 
                                  source = "google", 
                                  zoom = 8)

#displaying the map
ggmap(Site_map) +
  geom_point(data = Sites,
             aes(x = lon, y = lat, color=name), 
             size = 2,
             alpha = 1) + #darkening the points
  scale_color_manual("Site", #setting the color to the Sites
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue",
                              "CC"="white")) +  
  geom_label_repel(data = Sites,
                   aes(label = paste(as.character(name))),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'white')+
  xlab('Longtitude')+
  ylab('Latititude')+
  theme(legend.position = "none")
  
