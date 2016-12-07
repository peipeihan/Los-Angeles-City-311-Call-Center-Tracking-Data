
library(maps)
library(ggmap)
library(ggplot2)

# exploration: all 311 data are located in la county
a = map_data('county')
library(dplyr)
b = a %>%
  filter(region == 'california')
unique(b$subregion)
la = b %>%
  filter(subregion %in% c('orange', 
                          'riverside', 
                          'san bernardino', 
                          'ventura', 
                          'los angeles')) %>%
  arrange(subregion, order)
p = ggplot(la, aes(x = long, y = lat, group =  group)) +
  geom_polygon(fill = 'white', color = 'black') +
  p +
  geom_point(data = data, aes(x = Longitude, y = Latitude, group = 1))

# filter 311 data to no NA in longitude and latitude, then get the limits
datamap = data %>%
  filter(Latitude != 'NA', Longitude != 'NA')
min(datamap$Latitude)
max(datamap$Latitude)
min(datamap$Longitude)
max(datamap$Longitude)
unique(datamap$ZipCode)

# merge demographic dataset and zipcode dataset together
demo = read.csv('zip_codes_states.csv')
demo = demo[-c(2:6)]
library(zipcode)
data(zipcode)
zipcode %>%
  filter(zip == '90013')
a = merge(demo, zipcode, by.x = 'zip_code', by.y = 'zip', all.x = T)
write.csv(a,file="zipcode.csv",row.names = FALSE)
rm(demo)
# or for short
a = read.csv('zipcode.csv')
demo1 = a %>%
  filter(latitude >=33.5, latitude <= 34.5, longitude >= -118.7, longitude <= -118.1)
sum(is.na(demo1$latitude))
sum(is.na(demo1$longitude))

# get dataset to plot contour map for population
demo2 = demo1
demo2$stand = (demo1$Population-min(demo1$Population))/(max(demo1$Population)-min(demo1$Population))
demo2 = demo2 %>%
  arrange(stand)
demo2$rep = ifelse(demo2$stand<=0.01, 1, round(demo2$stand*100, 0))
str(demo2)
fullinfo = NULL
for (i in 1:223) {
  print(i)
  number <- as.numeric(demo2[i, 9])
  zipcode <- rep(as.vector(demo2[i, 1]), number)
  lon <- rep(as.vector(demo2[i, 7]), number)
  lat <- rep(as.vector(demo2[i, 6]), number)
  pop <- rep(as.vector(demo2[i, 2]), number)
  a = cbind(zipcode, lon, lat, pop)
  fullinfo <- rbind(fullinfo, a)
}
write.csv(fullinfo, file = 'population.csv', row.names=F)
population = read.csv('population.csv')
rm(fullinfo, demo2)

# get dataset to plot contour map for income
demo2 = demo1
demo2$stand = (demo1$MedianIncome-min(demo1$MedianIncome))/(max(demo1$MedianIncome)-min(demo1$MedianIncome))
demo2 = demo2 %>%
  arrange(stand)
demo2$rep = ifelse(demo2$stand<=0.01, 1, round(demo2$stand*100, 0))
str(demo2)
fullinfo = NULL
for (i in 1:223) {
  print(i)
  number <- as.numeric(demo2[i, 9])
  zipcode <- rep(as.vector(demo2[i, 1]), number)
  lon <- rep(as.vector(demo2[i, 7]), number)
  lat <- rep(as.vector(demo2[i, 6]), number)
  income <- rep(as.vector(demo2[i, 3]), number)
  a = cbind(zipcode, lon, lat, income)
  fullinfo <- rbind(fullinfo, a)
}
write.csv(fullinfo, file = 'income.csv', row.names=F)
income = read.csv('income.csv')
rm(fullinfo, demo2)

# get los angeles county map and zoom to only area with 311 data
library(ggmap)
map = qmap('los angeles', zoom = 10, maptype = 'roadmap', color = 'bw') +
  coord_cartesian(ylim = c(33.5, 34.5)) +
  coord_cartesian(xlim = c(-118.7, -118.1))
# point map for 311 data
populationmap +
  stat_bin2d(data = datamap, aes(x = Longitude, y = Latitude, fill = RequestType), 
             bins = 30, alpha = 1/20)

# get population contour map
populationmap = map +
  stat_density2d(data = population, aes(x = lon, y = lat, 
                                        fill = ..level..), 
                 geom = 'polygon', alpha = 0.5) +
  scale_fill_gradient(low = 'white', high = '#bd0026', guide = F) 
#map +geom_point(data = demo1, aes(x = longitude, y = latitude, color = Population), size = 7, alpha = 0.6)

# get income contour map
incomemap = map +
  stat_density2d(data = income, aes(x = lon, y = lat, 
                                        fill = ..level..), 
                 geom = 'polygon', alpha = 0.7) +
  scale_fill_gradient(low = 'white', high = '#006699', guide = F)
#populationmap + 311 data
c = populationmap +  
  geom_point(data = datamap, aes(x = Longitude, y = Latitude), 
                                 color = '#2b8cbe', alpha = 1/40)
populationmap +
  geom_point(data = datamap, aes(x = Longitude, y = Latitude), 
             color = '#2b8cbe', alpha = 1/20) +
  facet_wrap(~RequestType, nrow = 3) 
library(gridExtra)
grid.arrange(c, populationmap, nrow = 1, top = '311 & population density')

# incomemap + 311 data
b = incomemap +  
  geom_point(data = datamap, aes(x = Longitude, y = Latitude), 
             alpha = 1/70)
incomemap +
  geom_point(data = datamap, aes(x = Longitude, y = Latitude), 
             alpha = 1/30) +
  facet_wrap(~RequestType, nrow = 3) 
library(gridExtra)
grid.arrange(b, incomemap, nrow = 1, top = '311 & median income')

# CD split map
map +
  geom_point(data = datamap, aes(x = Longitude, y = Latitude, color = CD), 
             alpha = 1/15) +
  facet_wrap(~RequestType, nrow = 3) +
  theme(legend.position = 'none') +
  ggtitle('CD split map') +
  theme(plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
map +
  geom_point(data = datamap, aes(x = Longitude, y = Latitude, color = CD), 
             alpha = 1/20) +
  geom_text(data = datamap, aes(label = CD))

# get individual map
unique(datamap$RequestType)
# feedback
a = datamap %>%
  filter(RequestType == 'Feedback')
populationmap +
  geom_point(data = a, aes(x = Longitude, y = Latitude), 
             color = '#2b8cbe', size = 7, alpha = 1/2) +
  ggtitle('feedback') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
# water waste
a = datamap %>%
  filter(RequestType == 'Report Water Waste')
incomemap +
  geom_point(data = a, aes(x = Longitude, y = Latitude), 
             size = 7, alpha = 1/6) +
  ggtitle('Report Water Waste') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
# streetlight
a = datamap %>%
  filter(RequestType == 'Single Streetlight Issue')
d = populationmap +
  geom_point(data = a, aes(x = Longitude, y = Latitude, color = CD), 
             size = 3, alpha = 1/5) +
  ggtitle('Single Streetlight Issue')
a = datamap %>%
  filter(RequestType == 'Multiple Streetlight Issue')
e = populationmap +
  geom_point(data = a, aes(x = Longitude, y = Latitude, color = CD), 
             size = 3, alpha = 1/3) +
  ggtitle('Multiple Streetlight Issue')
grid.arrange(d, e, nrow = 1)
# santa monica beach
a = datamap %>%
  filter(RequestType %in% c('Graffiti Removal', 
                            'Homeless Encampment', 
                            'Illegal Dumping Pickup'))
populationmap +
  geom_point(data = a, aes(x = Longitude, y = Latitude, color = CD), 
             size = 3, alpha = 1/5) +
  facet_wrap(~RequestType) +
  ggtitle('Santa Monica beach') +
  theme(plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
  

# malibu
a = datamap %>%
  filter(RequestType %in% c('Metal/Household Appliances', 
                            'Dead Animal Removal', 
                            'Electronic Waste'))
populationmap +
  geom_point(data = a, aes(x = Longitude, y = Latitude, 
             color = CD), alpha = 1/5) +
  facet_wrap(~RequestType) +
  ggtitle('Malibu') +
  theme(plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))

# sunland-tunjunga
a = datamap %>%
  filter(RequestType %in% c('Metal/Household Appliances', 
                            'Dead Animal Removal', 
                            'Electronic Waste', 
                            'Graffiti Removal', 
                            'Homeless Encampment', 
                            'Illegal Dumping Pickup', 
                            'Bulky Items'))
populationmap +
  geom_point(data = a, aes(x = Longitude, y = Latitude), 
             color = '#2b8cbe', alpha = 1/20) +
  facet_wrap(~RequestType) +
  ggtitle('Sunland-Tunjunga') +
  theme(plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))

# airport-inglewood
a = datamap %>%
  filter(RequestType %in% c('Metal/Household Appliances', 
                            'Dead Animal Removal', 
                            'Electronic Waste', 
                            'Illegal Dumping Pickup', 
                            'Bulky Items', 
                            'Single Streetlight Issue'))
populationmap +
  geom_point(data = a, aes(x = Longitude, y = Latitude), 
             color = '#2b8cbe', alpha = 1/20) +
  facet_wrap(~RequestType) +
  ggtitle('Airport-Inglewood') +
  theme(plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
  

# try
bulky = datamap %>%
  filter(RequestType == 'Bulky Items')
populationmap +  
  geom_point(data = bulky, aes(x = Longitude, y = Latitude), 
             alpha = 1/100)
populationmap +
  stat_density2d(data = bulky, aes(x = Longitude, y = Latitude, 
                                   fill = ..level..), 
                 geom = 'polygon', alpha = 0.3)
# dropped
library(dplyr)
pointdata = datamap %>%
  group_by(RequestType, ZipCode) %>%
  summarize(count = n(), lon = mean(Longitude), lat = mean(Latitude))
populationmap +
  geom_point(data = pointdata, aes(x = lon, y = lat, 
                                   color = RequestType, size = count))
populationmap +
  geom_point(data = pointdata, aes(x = lon, y = lat, color = count), 
             size = 8, alpha = 0.6) +
  facet_wrap(~RequestType)




