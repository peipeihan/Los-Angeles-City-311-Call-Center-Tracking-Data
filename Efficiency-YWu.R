#### others: get department name
calldata <- read.csv('311_Call_Center_Tracking_Data.csv')
unique(calldata$Department.Abbreviation)
a = calldata%>%
  distinct(Department.Abbreviation, Department.Name) %>%
  arrange(Department.Abbreviation)

# efficiency distribution
t = data %>%
  filter(Owner != 'NA', handle != 'NA') %>%
  group_by(Owner) %>%
  summarise(ave = mean(handle))
(t1 = ggplot(t, aes(x = Owner, y = ave)) +
  geom_bar(stat = 'identity') +
  ggtitle('average handle time of owner'))
t = data %>%
  filter(RequestType != 'NA', handle != 'NA') %>%
  group_by(RequestType) %>%
  summarise(ave = mean(handle))
(t1 = ggplot(t, aes(x = RequestType, y = ave)) +
  geom_bar(stat = 'identity') +
  ggtitle('average handle time of RequestType'))
t = data %>%
  filter(RequestSource != 'NA', handle != 'NA') %>%
  group_by(RequestSource) %>%
  summarise(ave = mean(handle))
(t1 = ggplot(t, aes(x = RequestSource, y = ave)) +
  geom_bar(stat = 'identity', fill = c('#016c59', '#045a8d','darkred', rep('#045a8d', 3), 'darkred', '#045a8d','#016c59', rep('#045a8d', 8)), alpha = 0.5) +
  ggtitle('average handle time of RequestSource')+
  ylab('handle time (days)'))

# TS - total
t = data %>%
  group_by(week) %>%
  summarize(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
(t1 = ggplot(t, aes(x = week, y = aves)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_area(aes(x = week, y = counts), fill = '#74a9cf', alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 60, 20), labels = c('201508', '201512', '201605', '201609')) +
  theme(axis.text.y = element_blank()) +
  ylab('Request number/handeling time') +
  xlab('') +
  ggtitle('request number & handle time'))
# handling funnel
a = data%>%
  filter(service > 0, close != 'NA') %>%
  group_by(monthnum) %>%
  summarise(s_ave = mean(service), c_ave = mean(close))
ggplot(a, aes(x = monthnum, y = s_ave+c_ave)) +
  geom_bar(stat = 'identity', fill = '#045a8d', color = '#2b8cbe', alpha = 0.6) +
  geom_bar(aes(x = monthnum, y = s_ave), stat = 'identity', 
           alpha = 0.4, fill = '#f1eef6', color = '#2b8cbe') +
  ggtitle('handling funnel by time') +
  ylab('days') +
  xlab('') +
  coord_flip() +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
# TS - request
t = data %>%
  group_by(RequestType, week) %>%
  summarize(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
(t1 = ggplot(t, aes(x = week, y = aves)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_area(aes(x = week, y = counts), fill = '#74a9cf', alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 60, 20), labels = c('201508', '201512', '201605', '201609')) +
  ylab('Request number/handeling time') +
  xlab('') +
  facet_wrap(~RequestType, nrow = 12, scale = 'free_y') +
  ggtitle('request number&handle time by requestType') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))

# TS - owner
t = data %>%
  filter(Owner != 'NA') %>%
  group_by(Owner, week) %>%
  summarize(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
(t1 = ggplot(t, aes(x = week, y = aves)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_area(aes(x = week, y = counts), fill = '#74a9cf', alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 60, 20), labels = c('201508', '201512', '201605', '201609')) +
  ylab('Request number/handeling time') +
  xlab('') +
  facet_wrap(~Owner, nrow = 7, scale = 'free_y') +
  ggtitle('request number&handle time by Owner') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))


# owner efficiency
t = data %>%
  filter(Owner != 'NA') %>%
  group_by(Owner) %>%
  summarise(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
t$Owner = ordered(t$Owner, levels = rev(levels(t$Owner)))
levels(t$Owner)
(t1 = ggplot(t, aes(x = Owner, y = aves, group = 1)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_bar(aes(x = Owner, y = counts), stat = 'identity', 
           fill = '#a6bddb', alpha = 0.4, color = '#a6bddb') +
  theme(axis.text.x = element_blank()) +
  ylab('Request number/handeling time') +
  coord_flip() +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))
t = data %>%
  filter(Owner != 'NA') %>%
  group_by(Owner, RequestType) %>%
  summarise(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
levels(t$RequestType)
t$RequestType = factor(t$RequestType, labels = c('bulky', 'deadAnimal', 'electronic', 'feedback', 'graffiti', 'homeless', 'dumping', 'appliance', 'streetlighM', 'other', 'water', 'streetlightS'))
(t2 = ggplot(t, aes(x = RequestType, y = aves, group = 1)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_bar(aes(x = RequestType, y = counts), stat = 'identity', 
           fill = '#a6bddb', alpha = 0.4, color = '#a6bddb') +
  theme(axis.text.y = element_blank()) +
  ylab('Request number/handeling time') +
  ggtitle('request number & handle time by owner') +
  facet_wrap(~Owner, nrow = 7, scale = 'free_y') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))
library(gridExtra)
grid.arrange(t1, t2, nrow = 1, top = 'request number & handle time by owner') +
  theme(plot.title=element_text(size=22,lineheight=0.8,
                                 face="bold", 
                                 margin=margin(10,0,10,0),
                                 family = "Times"))

# request type efficiency
t = data %>%
  filter(RequestType != 'NA') %>%
  group_by(RequestType) %>%
  summarise(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
t$RequestType = factor(t$RequestType, labels = c('bulky', 'deadAnimal', 'electronic', 'feedback', 'graffiti', 'homeless', 'dumping', 'appliance', 'streetlighM', 'other', 'water', 'streetlightS'))
(t1 = ggplot(t, aes(x = RequestType, y = aves, group = 1)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_bar(aes(x = RequestType, y = counts), stat = 'identity', 
           fill = '#a6bddb', alpha = 0.4, color = '#a6bddb') +
  theme(axis.text.y = element_blank()) +
  ylab('Request number/handeling time') +
  ggtitle('request number & handle time by RequestType') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))

# assignTo efficiency
t = data %>%
  filter(AssignTo != 'NA') %>%
  group_by(AssignTo) %>%
  summarise(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
(t1 = ggplot(t, aes(x = AssignTo, y = aves, group = 1)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_bar(aes(x = AssignTo, y = counts), stat = 'identity', 
           fill = '#a6bddb', alpha = 0.4, color = '#a6bddb') +
  ylab('Request number/handeling time') +
  ggtitle('request number & handle time by AssignTo') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))

# heatmap efficiency
h = data %>%
  group_by(weekday, hour) %>%
  summarise(efficiency = mean(handle))
h2 = ggplot(h, aes(weekday, hour, fill = efficiency)) +
  geom_tile() +
  scale_fill_gradient(high = '#edf8fb', low = '#006d2c', guide = F) +
  ggtitle('handle efficiency by hour') +
  theme_classic() +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))

h = data %>%
  group_by(weekday, hour) %>%
  summarise(number = n())
h1 = ggplot(h, aes(weekday, hour, fill = number)) +
  geom_tile() +
  scale_fill_gradient(high = '#016c59', low = '#f6eff7', guide = F) +
  ggtitle('request number by hour') +
  theme_classic() +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
grid.arrange(h1, h2, nrow = 1)

h = data %>%
  group_by(month, weekday) %>%
  summarise(efficiency = mean(handle))
h2 = ggplot(h, aes(weekday, month, fill = efficiency)) +
  geom_tile() +
  scale_fill_gradient(high = '#edf8fb', low = '#006d2c', guide = F) +
  ggtitle('handle efficiency by month') +
  theme_classic() +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
h = data %>%
  group_by(month, weekday) %>%
  summarise(number = n())
h1 = ggplot(h, aes(weekday, month, fill = number)) +
  geom_tile() +
  scale_fill_gradient(high = '#016c59', low = '#f6eff7', guide = F) +
  ggtitle('request number by month') +
  theme_classic() +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times"))
grid.arrange(h1, h2, nrow = 1)

# individual one
t = data %>%
  filter(RequestType %in% c('Bulky Items', 'Graffiti Removal')) %>%
  group_by(RequestType, week) %>%
  summarize(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
(t1 = ggplot(t, aes(x = week, y = aves)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_area(aes(x = week, y = counts), fill = '#74a9cf', alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 60, 20), labels = c('201508', '201512', '201605', '201609')) +
  ylab('Request number/handeling time') +
  xlab('') +
  facet_wrap(~RequestType, nrow = 12, scale = 'free_y') +
  ggtitle('request number&handle time by requestType') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))
t = data %>%
  filter(Owner %in% c('BOS', 'OCB')) %>%
  group_by(Owner, week) %>%
  summarize(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
(t1 = ggplot(t, aes(x = week, y = aves)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_area(aes(x = week, y = counts), fill = '#74a9cf', alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 60, 20), labels = c('201508', '201512', '201605', '201609')) +
  ylab('Request number/handeling time') +
  xlab('') +
  facet_wrap(~Owner, nrow = 7, scale = 'free_y') +
  ggtitle('request number&handle time by Owner') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))
t = data %>%
  filter(Owner != 'NA', RequestType %in% c('Feedback', 
                                           'Homeless Encampment', 
                                           'Multiple Streetlight Issue')) %>%
  group_by(RequestType, Owner) %>%
  summarise(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
(t1 = ggplot(t, aes(x = Owner, y = aves, group = 1)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_bar(aes(x = Owner, y = counts), stat = 'identity', 
           fill = '#a6bddb', alpha = 0.4, color = '#a6bddb') +
  ylab('Request number/handeling time') +
  facet_wrap(~RequestType, nrow = 3) +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))
t = data %>%
  filter(Owner %in% c('LADWP', 'BSS')) %>%
  group_by(Owner, RequestType) %>%
  summarise(ave = mean(handle), count = n())
t$aves = (t$ave-min(t$ave))/(max(t$ave)-min(t$ave))
t$counts = (t$count-min(t$count))/(max(t$count)-min(t$count))
levels(t$RequestType)
t$RequestType = factor(t$RequestType, labels = c('bulky', 'deadAnimal', 'electronic', 'feedback', 'graffiti', 'homeless', 'dumping', 'appliance', 'streetlighM', 'other', 'water', 'streetlightS'))
(t2 = ggplot(t, aes(x = RequestType, y = aves, group = 1)) +
  geom_line(size = 1, color = '#045a8d') +
  geom_bar(aes(x = RequestType, y = counts), stat = 'identity', 
           fill = '#a6bddb', alpha = 0.4, color = '#a6bddb') +
  theme(axis.text.y = element_blank()) +
  ylab('Request number/handeling time') +
  ggtitle('request number & handle time by owner') +
  facet_wrap(~Owner, nrow = 7, scale = 'free_y') +
  theme(axis.title.x=element_text(size=18,face="bold",family="Times"),
        axis.title.y=element_text(size=18,face="bold",family="Times"),
        axis.text=element_text(family="Times",size=14),
        legend.title=element_text(face="bold",size=16,family="Times"),
        legend.text=element_text(family="Times",size=12),
        plot.title=element_text(size=22,lineheight=0.8,
                                face="bold", 
                                margin=margin(10,0,10,0),
                                family = "Times")))



### dropped
# efficiency funnel by request type
a = data %>%
  filter(service > 0, close != 'NA', RequestType == 'Dead Animal Removal')
a = data %>%
  filter(RequestType == 'Dead Animal Removal')
b = data %>%
  filter(Owner == 'BSS')
unique(b$RequestType)
t = data %>%
  filter(service > 0, close != 'NA', Owner != 'NA', RequestType != 'Dead Animal Removal') %>%
  group_by(RequestType) %>%
  summarise(s_ave = mean(service), c_ave = mean(close))
(t1 = ggplot(t, aes(x = RequestType, y = s_ave+c_ave)) +
  geom_bar(stat = 'identity', fill = '#045a8d', color = '#2b8cbe', alpha = 0.6) +
  geom_bar(aes(x = RequestType, y = s_ave), stat = 'identity', 
           alpha = 0.4, fill = '#f1eef6', color = '#2b8cbe') +
  ggtitle('handling breakdown by requestType') +
  ylab('handling days'))