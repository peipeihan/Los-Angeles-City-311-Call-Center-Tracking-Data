
library(raster)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(dplyr)



###### Council District Boundary
#cd <- readShapeSpatial("geo_export_134c9b69-4187-4d97-bd68-afae8fc972e4.shp")
#fcd<- fortify(cd)
#fcd<- arrange(fcd,group,order)
#write.csv(fcd, file = "fcd.csv")

fcd1 <- read.csv("fcd.csv")

unique(fcd1$group)
str(fcd1$group)

qmap(location = "los angeles", color = "bw", zoom = 10, maptype = "roadmap")  +
  #  annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")
  
  geom_polygon(data = fcd1, aes(x = long, y = lat, group = group), fill = "green", alpha = 0.5) +
  geom_path(data = fcd1, aes(x = long, y = lat,group  = group), color = "blue") + 
  ggtitle("Council District Boundary")




###### ZipCode Boundary
zip <- readShapeSpatial("geo_export_04764e3c-d9c5-4b53-9d04-d229794051a8.shp")
zip<- fortify(zip)
zip<- arrange(zip,group,order)
write.csv(zip, file = "zip.csv")

zip1 <- read.csv("zip.csv")


qmap(location = "los angeles", color = "bw", zoom = 10, maptype = "roadmap")  +
  #  annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")
  geom_polygon(data = zip1, aes(x = long, y = lat, group = group), fill = "green", alpha = 0.5) +
  geom_path(data = zip1, aes(x = long, y = lat,group  = group), color = "blue") + 
  ggtitle("ZipCode Boundary")

