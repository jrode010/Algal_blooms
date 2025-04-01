###Current day sampling data###
library(tidyverse)
library(terra)
library(lubridate)

##Start with chlorophyll visualization through Alligator Creek Subestuary and out into Garfield

chl <- read.csv(file = 'Data/Current_day/chl_4-1-2025.csv')
lakes <- vect('Data/Lakes.shp')
names(lakes)
names(chl)
chl <- chl %>% dplyr::rename(Site = Site_code)
str(chl)
chlsep <- chl %>% dplyr::filter(Sampling_event == '9/23/2025') %>% group_by(Site) %>% summarize(chlm = mean(Chl))

shpset <- merge(lakes, chlsep, by = 'Site')
plot(shpset, 'chlm')

writeVector(shpset, 'Chorophyll_sep2023.shp')

chlap <- chl %>% dplyr::filter(Sampling_event == '4/24/2025'| Sampling_event == '1/24/2025') %>% group_by(Site) %>% summarize(chlm = mean(Chl))

shpap <- merge(lakes, chlap, by = 'Site')
plot(shpap, 'chlm')

writeVector(shpap, 'Chorophyll_Apr2024.shp')

chlaug <- chl %>% dplyr::filter(Sampling_event == '8/24/2025') %>% group_by(Site) %>% summarize(chlm = mean(Chl))

shpaug <- merge(lakes, chlaug, by = 'Site')
plot(shpaug, 'chlm')

writeVector(shpaug, 'Chorophyll_Aug2024.shp')
