##Chlorophyll ncfb

library(tidyverse)
library(scales)
library(ggplot2)
library(chron)
library(splitstackshape) 
library(lubridate)

setwd('E:/FIU/Project/Florida_Bay/Salinity/Chlorophyll/Sep_2025/')
rank <- read.csv('E:/FIU/Project/Florida_Bay/Salinity/Chlorophyll/Sep_2025/rank_chl.csv')
gar <- read.csv('gar_chl.csv')
john <- read.csv('john_chl.csv')
head(rank)

rank2 <- rank %>% mutate(date = mdy(collectDate)) %>% dplyr::select(date, value)
str(rank2)
gar2 <- gar %>% mutate(date = mdy(collectDate)) %>% dplyr::select(date, value)
john2 <- john %>% mutate(date = mdy(collectDate)) %>% dplyr::select(date, value)
rank2$month <- format(as.Date(rank2$date, format='%Y-%m-%d'), format = '%Y-%m-01')
rank2$month <- ymd(rank2$month)
rank2 <- rank2 %>% group_by(date) %>% mutate(value = mean(value))
gar2$month <- format(as.Date(gar2$date, format='%Y-%m-%d'), format = '%Y-%m-01')
gar2$month <- ymd(gar2$month)
gar2 <- gar2 %>% group_by(date) %>% mutate(value = mean(value))
john2$month <- format(as.Date(john2$date, format='%Y-%m-%d'), format = '%Y-%m-01')
john2$month <- ymd(john2$month)

#group them
tot <- rbind(rank2, gar2)

tot2 <- tot %>% group_by(month) %>% summarize(mean_chl = mean(value))
tot2$month <-  ymd(tot2$month)

ggplot()+
  geom_line(data = tot2, aes(x = month, y = mean_chl), size = 0.5, color = 'darkgreen')+
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0,50))+
  scale_x_date(breaks = seq(as.Date('2010-01-01'),
                            as.Date('2025-12-31'), by = '1 year'), date_labels = '%Y')+
  theme_classic()+
  labs(title = 'Florida Bay Chlorophyll', x = 'Year', y = 'Chlorophyll (ug/L)')+
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        #egend.position = 'none',
        axis.title = element_text(size = 16, face = "bold"), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave(filename = "FB_chl_sep_2025.png", 
       units="in", width=8, height=4, 
       dpi=300)

#separate by basin. Garfield first
gar2 <- gar2 %>% dplyr::filter(month >= ymd('2011-01-01'))
ggplot()+
  geom_line(data = gar2, aes(x = month, y = value), size = 1.5, color = 'darkgreen')+
  scale_y_continuous(breaks = seq(0,40, 10), limits = c(0,40))+
  scale_x_date(breaks = seq(as.Date('2011-01-01'),
                            as.Date('2025-12-31'), by = '1 year'), date_labels = '%Y')+
  theme_classic()+
  labs(title = 'Garfield Chlorophyll', x = 'Year', y = 'Chlorophyll (ug/L)')+
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        #egend.position = 'none',
        axis.title = element_text(size = 16, face = "bold"), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
setwd('C:/Users/jonro/OneDrive/Desktop/Algal_blooms/')
ggsave(filename = "plots/Current_day/Gar_chl_oct_2025.png", 
       units="in", width=8, height=4, 
       dpi=300)

#separate by basin. Rankin
rank3 <- rank2 %>% dplyr::filter(month >= as.Date('2016-01-01'))
ggplot()+
  geom_line(data = rank3, aes(x = month, y = value), size = 1.5, color = 'darkgreen')+
  scale_y_continuous(breaks = seq(0,60, 10), limits = c(0,60))+
  scale_x_date(breaks = seq(as.Date('1992-01-01'),
                            as.Date('2025-12-31'), by = '1 year'), date_labels = '%Y')+
  theme_classic()+
  labs(title = 'Rankin Chlorophyll', x = 'Year', y = 'Chlorophyll (ug/L)')+
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        #egend.position = 'none',
        axis.title = element_text(size = 16, face = "bold"), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave(filename = "plots/Rank_chl_oct_2016_2025.png", 
       units="in", width=8, height=4, 
       dpi=300)

#separate by basin. Johnson
ggplot()+
  geom_line(data = john2, aes(x = month, y = value), size = 1.5, color = 'darkgreen')+
  scale_y_continuous(breaks = seq(0,50, 10), limits = c(0,50))+
  scale_x_date(breaks = seq(as.Date('2010-01-01'),
                            as.Date('2025-12-31'), by = '1 year'), date_labels = '%Y')+
  theme_classic()+
  labs(title = 'Johnson Chlorophyll', x = 'Year', y = 'Chlorophyll (ug/L)')+
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        #egend.position = 'none',
        axis.title = element_text(size = 16, face = "bold"), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave(filename = "John_chl_oct_2025.png", 
       units="in", width=8, height=4, 
       dpi=300)

#Rankin by month
rank_month <- rank3 %>% mutate(month2 = month(month)) %>% group_by(month2) %>% summarize(meanchl = mean(value), sdchl = sd(value))

ggplot()+
  geom_line(data = rank_month, aes(x = month2, y = meanchl), size = 1.5, color = 'darkgreen')+
  geom_ribbon(data = rank_month, aes(x = month2, ymin = meanchl-sdchl, ymax=meanchl+sdchl), inherit.aes = F, color = 'grey', alpha = 1)+
  scale_y_continuous(breaks = seq(0,20, 4), limits = c(0,20))+
  scale_x_continuous(breaks = seq(1,12,1), limits = c(1,12))+
  theme_classic()+
  labs(title = 'Monthly Average Rankin Chlorophyll', x = 'Month', y = 'Chlorophyll (ug/L)')+
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        #egend.position = 'none',
        axis.title = element_text(size = 16, face = "bold"), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave(filename = "plots/Rank_chl_monthly_average.png", 
       units="in", width=8, height=4, 
       dpi=300)

?geom_ribbon
  