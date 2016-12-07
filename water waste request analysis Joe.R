library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggmap)
library(maps)
library(zoo)
library(raster)
library(maptools)
library(rgdal)

#f6eff7
#bdc9e1
#67a9cf
#1c9099
#016c59

###data2
service = read.csv("311Service2016.csv")
service = tbl_df(service)

service$CreatedDate = mdy_hms(service$CreatedDate)
service$UpdatedDate = mdy_hms(service$UpdatedDate)
service$ServiceDate = mdy_hms(service$ServiceDate)
service$ClosedDate = mdy_hms(service$ClosedDate)


####### REQUIREMENTS

#1.Distribution of requests, distribution of departments referred to

Srequest = service %>%
  group_by(RequestType)%>%
  summarise(count = n())

ggplot(Srequest, aes(x = reorder(RequestType, -count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") + 
  theme_classic() + ylab("") + xlab("Request Type")

#pie chart - request type

pie1 <- ggplot(service, aes(x = factor(1), fill = factor(RequestType))) +
  geom_bar(width = 1) + scale_fill_discrete(name="Request Type") +
  ggtitle("Incident Counts by Request Type") +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(color="#3690c0",face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))

  

pie1 + coord_polar(theta = "y") +
  xlab("") + ylab("")

#2.App vs Phone call referrals, service type question for each input channel

Ssource = service %>%
  filter(RequestSource == "Mobile App" |RequestSource == "Call" |
           RequestSource == "Driver Self Report" |
           RequestSource == "Self Service") %>%
  group_by(RequestSource) %>%
  summarise(count = n())

ggplot(Ssource, aes(reorder(RequestSource, -count), count/10000)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ylab("") + xlab("") + theme_classic() +
  ggtitle("Request Source Counts in ten thousand")


#3.Overall trends in calls / apps: sums and counts

service$yearmon = as.yearmon(service$CreatedDate)

Strend = service %>%
  filter(RequestSource == "Mobile App" |RequestSource == "Call") %>%
  group_by(RequestSource, yearmon) %>%
  summarise(count = n())

call = Strend %>%
  filter(RequestSource == "Call")

mob = Strend %>%
  filter(RequestSource == "Mobile App")

str(call)

ggplot(call, aes(yearmon, count)) +
  geom_line(color = "#016c59") +
  geom_line(data = mob, aes(yearmon, count), color = "orange") +
  theme_linedraw() +
  ggtitle("Request Source: Mobile App vs. Call ")

  
## A graph with more significant trend

#5.Service Type breakdowns. Which requests are most common over time and areas.

service$zip = str_detect(service$ZipCode, "[0-9]{5}")

heat = service %>%
  filter(CDMember != "" & CDMember != "Vacant") %>%
  group_by(CDMember, yearmon) %>%
  summarise(count = n())

ggplot(heat, aes(x = yearmon, 
                 y = CDMember, 
                 fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_classic() + xlab("") +
  ggtitle("Request Counts in CD Member Districts by Month")

#6.Change volume of requests and input type (app/call) over time.

mob$callapp = call$count + mob$count
mob$perc = mob$count/mob$callapp

ggplot(mob, aes(yearmon, perc*100)) +
  geom_line(color = "#1c9099") +
  ggtitle("Mobile App Percentage") +
  xlab("") + ylab("") +
  theme_linedraw()



#7.Geographic + Service Type breakdown



##*1.LOCATION: Population map by zipcode: Scrapping(Census, Boundary)


###a. water and electric
WE = read.csv("Water_And_Electric_Rate_Zones.csv")

Wrate = rbind(
cbind(WE$Water.Zone.1..Low.,rep("low",80)),
cbind(WE$Water.Zone.2..Medium.,rep("medium",80)),
cbind(WE$Water.Zone.3..High.,rep("high",80)))


zipwater = service %>%
  filter(zip == "TRUE") %>%
  filter(RequestType == "Report Water Waste") %>%
  group_by(ZipCode) %>%
  summarise(count = n())



water = merge(zipwater, Wrate, by.x ="ZipCode", by.y = "V1")

water %>%
  group_by(V2) %>%
  summarise(Rate = sum(count)/n()) %>%
  ggplot(aes(V2, Rate)) +
  geom_bar(stat ="identity", fill = "lightblue")

## map

##Los Angeles Water Supply May Not Be Enough In A Few Decades. By 2025 — 10 years ahead of its long-term projections — the city expects to cut its Delta and Colorado imports in half, while boosting groundwater use to 16 percent, recycled sewer water to 8 percent, water conservation to 9 percent and stormwater capture to 3 percent.


###map of water waste LA by water rate
serviceW = service %>%
  filter(zip == "TRUE") %>%
  filter(Latitude != "" & Longitude != "") %>%
  filter(RequestType == "Report Water Waste")

serviceW = merge(serviceW, Wrate, by.x = "ZipCode", by.y = "V1")

lamap = qmap("Studio City", maptype = "roadmap", color = "bw", zoom = 10)

lamap +
  geom_point(data = serviceW, aes(Longitude, Latitude, color = V2), 
             alpha = 0.6, size = 1.8) +
  scale_color_manual(values=c("pink", "#67a9cf", "#1c9099"), 
                     name="Water Rate\nClassification",
                     breaks=c("low", "medium", "high")) +
  ggtitle("Water Rate Level in Los Angeles")

zipcode = read.csv("zipcode.csv")

zipcode = zipcode %>%
  filter(Population != 0) %>%
  filter(MedianIncome != 0)

zipwater = merge(zipwater, zipcode, by.x = "ZipCode", by.y = "zip_code")

zipwater$waste = zipwater$count/zipwater$Population

ggplot(zipwater, aes(waste)) + 
  geom_histogram()

##zipcode boundary

zip = read.csv("zip.csv")

lazipmap = qmap("Studio City", maptype = "roadmap", color = "bw", zoom = 10) +
  geom_polygon(data = zip, aes(x = long, y = lat, group = group), fill = NA , alpha = 0.5) +
  geom_path(data = zip, aes(x = long, y = lat,group  = group), color = "black", size = 0.2) 



lazipmap +
  geom_point(data = zipwater, aes(longitude, latitude, 
                                  color = waste),
             size = 5,
             alpha = 0.8) +
  scale_color_continuous(low = "#fee6ce", high = "red") + 
  theme(legend.position = "none",
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")) +
  ggtitle("Water Waste Report in 311")



head(zip,10
     )

# 90064 is too high: 

zipwater2 = zipwater %>%
  filter(ZipCode != 90064)

lazipmap +
  geom_point(data = zipwater2, aes(longitude, latitude, 
                                  color = sqrt(waste)),
             size = 5 ,
             alpha = 0.8) +
  scale_color_continuous(low = "#fee6ce", high = "red") +
  theme(legend.position = "none",
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")) +
  ggtitle("Water Waste Report in 311 without Outlier")

zipwater2 %>%
  filter(waste > 0.001) 

# top 5 water waste by population: 90010, 90019, 90067, 90077, 91403
#91403, Sherman Oaks has the highest income and also a higher White population. The other zip codes have more Latino residents. These areas have more single detached homes, larger lots and more space that needs irrigation. They also tend to have higher water use, with Sherman Oaks turning on the faucet the most.

### Electronic
colnames(WE)

Erate = rbind(
  cbind(WE$Electric.Zone.1..Low., rep("low", 80)),
  cbind(WE$Electric.Zone.2..High., rep("high", 80))
)

zipelec = service %>%
  filter(zip == "TRUE") %>%
  filter(RequestType == "Electronic Waste") %>%
  group_by(ZipCode) %>%
  summarise(count = n())

## distribution of E rate

serviceE = service %>%
  filter(zip == "TRUE") %>%
  filter(Latitude != "" & Longitude != "") %>%
  filter(RequestType == "Electronic Waste")

serviceE = merge(serviceE, Erate, by.x = "ZipCode", by.y = "V1")

lamap +
  geom_point(data = serviceE, aes(Longitude, Latitude, color = V2), 
             alpha = 0.5, size = 1) +
  scale_color_manual(values=c("pink", "lightblue"), 
                     name="Electronic Rate\nClassification",
                     breaks=c("low", "high")) +
  ggtitle("Electronic Rate Level in Los Angeles")

## distribution of E waste

zipelec = merge(zipelec, zipcode, by.x = "ZipCode", by.y = "zip_code")

zipelec$waste = zipelec$count/zipelec$Population

ggplot(zipelec, aes(waste)) + 
  geom_histogram()

lamap +
  geom_point(data = zipelec, aes(longitude, latitude, 
                                  color = waste),
             size = 5,
             alpha = 0.8) +
  scale_color_continuous(low = "white", high = "darkred") +
  ggtitle("Electronic Waste Report in 311")

zipelec %>%
  filter(waste > 0.0185)
  
## top 5 90042 90065 91040 91331 91345

## Insight






## Peipei

service %>%
  filter(RequestSource %in% c("Mobile App","Call") & RequestType %in% c("Bulky Items","Graffiti Removal")) %>%
  mutate(ReqTypeLevel = ifelse(RequestType =="Bulky Items","Bulky Items","Graffiti Removal")) %>%
  group_by(yearmon,RequestSource,ReqTypeLevel) %>%
  summarise(cnt = n()) %>%
  
  ggplot(aes(x = as.factor(yearmon), y = cnt, group = ReqTypeLevel, color = ReqTypeLevel )) +
  geom_jitter(alpha=0.5, position = position_jitter(h=0.7,w=0.4))+
  geom_smooth(method = "loess", se=F) +
  scale_color_manual(values = c("#e6550d","#2c7fb8"),
                     name = "") +
  xlab("") +
  ylab("Request Number")+
  ggtitle("Overall Trend of Call vs. App by Month")+
  facet_wrap(~RequestSource, ncol=1, scales = "free_y")+
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=11),
        legend.title=element_text(color="#3690c0",face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
