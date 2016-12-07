data0 <- read.csv('MyLA311_Service_Request_Data_2016.csv', na.strings = '')
data1 <- unique(data0)
# 13 duplicates
data2 <- data1[rowSums(is.na(data1)) != 33, ]
# no na obs
library(raster)
data3 <- trim(data2)
data <- data3
rm(data0, data1, data2)
library(ggplot2)

#clean
str(data)
table(data$ZipCode)
library(lubridate)
data$CreatedDate <- mdy_hms(data$CreatedDate)
data$UpdatedDate <- mdy_hms(data$UpdatedDate)
data$ServiceDate <- mdy_hms(data$ServiceDate)
data$ClosedDate <- mdy_hms(data$ClosedDate)

data$CD <- as.factor(data$CD)
# 2 levels less, need to break down
data$NC <- as.factor(data$NC)
# 1 level less, blank

unique(data$Owner)
unique(data$RequestType)
unique(data$RequestSource)
unique(data$Status)
unique(data$ActionTaken)
unique(data$Direction)
# direction
unique(data$ZipCode)
# zipcode 0
unique(data$RequestSource)
unique(data$CD)

data$year = year(data$CreatedDate)
unique(data$year)
s = data %>%
  filter(year == 2015) %>%
  distinct
data$quarter = quarters(data$CreatedDate)
data$month = month(data$CreatedDate, label = T)
data$weekday = wday(data$CreatedDate, label = T)
data$monthday = day(data$CreatedDate)
data$hour = hour(data$CreatedDate)
data$service = (data$ServiceDate-data$CreatedDate)/(24*60*60)
data$close = (data$ClosedDate - data$ServiceDate)/(24*60*60)
data$handle = as.numeric((data$UpdatedDate - data$CreatedDate)/(24*60*60))
data$week = round(difftime(data$CreatedDate, min(data$CreatedDate), units = 'weeks'), 0)
data$week = as.numeric(as.character(data$week))
data$service = as.numeric(as.character(data$service))
data$close = as.numeric(as.character(data$close))
data$monthnum = as.factor(paste(as.character(data$year), as.character(month(data$CreatedDate))))
data$monthnum = factor(data$monthnum, 
                       levels = levels(data$monthnum)[c(4, 5, 1:3, 6, 10, 11:17, 7:9)])
data$monthnum = ordered(data$monthnum, 
                       levels = rev(levels(data$monthnum)))
unique(data$monthnum)
levels(data$monthnum)


# Distribution
library(dplyr)

unique(data$RequestType)
dist1 <- data %>%
  group_by(RequestType) %>%
  summarise(count = n())
(hist1 = ggplot(dist1, aes(x = RequestType, y = count)) +
  geom_bar(stat = 'identity') +
  ggtitle('Request Distribution'))

unique(data$Owner)
dist2 <- data %>%
  filter(Owner != '') %>%
  group_by(Owner) %>%
  summarise(count = n())
(hist2 = ggplot(dist2, aes(x = Owner, y = count)) +
  geom_bar(stat = 'identity') +
  ggtitle('Department Distribution'))
dist21 <- data %>%
  filter(Owner != '') %>%
  group_by(Owner, RequestType) %>%
  summarise(count = n())
(hist21 = ggplot(dist21, aes(x = RequestType, y = count)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Owner, nrow = 6) +
  ggtitle('Request Distribution across department'))
(hist21 = ggplot(dist21, aes(x = RequestType, y = count)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Owner, nrow = 6, scales = 'free_y') +
  ggtitle('Request Distribution across department'))

dist3 <- data %>%
  filter(MobileOS != '') %>%
  group_by(MobileOS) %>%
  summarise(count = n())
(hist3 = ggplot(dist3, aes(x = MobileOS, y = count)) +
  geom_bar(stat = 'identity') +
  ggtitle('Mobile Channel Distribution'))
dist31 <- data %>%
  filter(MobileOS != '') %>%
  group_by(MobileOS, RequestType) %>%
  summarise(count = n())
(hist31 = ggplot(dist31, aes(x = RequestType, y = count)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~MobileOS, nrow = 2) +
  ggtitle('Mobile Channel Distribution'))

