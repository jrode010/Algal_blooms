###Data wrangling and visualization - time series for concentration measurements###
##Author: Jon Rodemann
##Date last edited: 3/18/2025

#Load in libraries
library(tidyverse)
library(lubridate)

##So we have uploaded all of the data into the folder. I have cleaned some of the files. 
##First thing is to plot all of the easy data. So bring in:
## - Grab samples
## - flows
## - nutrients from creeks
## - Station data

c3d <- read.csv(file = 'Data/Clean/AC_MC_3day.csv') #3 day autosampler data for TN, TP
cm <- read.csv(file = 'Data/Clean/AC_MC_monthly.csv') #monthly grab samples at AC and MC for nutrients
af <- read.csv(file = 'Data/Clean/Alligator_Flow.csv') #Daily maxstage, minstage, meanstage, and flow for AC
mf <- read.csv(file = 'Data/Clean/McCormick_Flow.csv') #Daily maxstage, minstage, meanstage, and flow for MC
of <- read.csv(file = 'Data/Clean/Oyster_Flow.csv') #Daily maxstage, minstage, meanstage, and flow for Oyster Creek
wf <- read.csv(file = 'Data/Clean/Westlake_Flow.csv') #Daily meanstage and flow for West lake creek
gg <- read.csv(file = 'Data/Clean/GB_grab.csv') #monthly grab sample data in Garfield
rg <- read.csv(file = 'Data/Clean/RB_grab.csv') #monthly grab sample data in Rankin
tg <- read.csv(file = 'Data/Clean/TB_grab.csv') #monthly grab sample data in Terrapin
gs <- read.csv(file = 'Data/Clean/GB_station.csv') #Daily station data (CHL, DO, Rainfall, salinity, temperature, stage) for Garfield
ts <- read.csv(file = 'Data/Clean/TB_station.csv') #Daily station data (CHL, DO, Rainfall, salinity, temperature, stage) for Terrapin
mr <- read.csv(file = 'Data/Clean/Marsh_CHP_rain.csv') #Daily rainfall in the Marsh at CHP

##Now we have the easy stuff in, let's clean it all up, subsample to monthly, and plot
##
head(c3d)

mon_fun1 <- function(x, y, w, z){ #for data only needed to be grouped once, x = dataset, y = date column, w is column for the mean, z = quoted name for new mean column
  b <- x %>% mutate(date = mdy({{y}})) %>% mutate(date = as.Date(format(date, '%Y-%m-01')))%>% group_by(date) %>% summarize(mean = mean({{w}}, na.rm = T)) %>% dplyr::rename(!!z := mean)
  return(b)
}

mon_fun2 <- function(x, y, p, w, z){#for data needed to be grouped twice like flow, grab sample data, x = dataset, y = date column, p = other column to group by, w is column for the mean, z = quoted name for new mean column
  b <- x %>% mutate(date = mdy({{y}})) %>% mutate(date = as.Date(format(date, '%Y-%m-01')))%>% group_by(date, {{p}}) %>% summarize(mean = mean({{w}}, na.rm = T)) %>% dplyr::rename(!!z := mean)
  return(b)
}

mctn = mon_fun1(x = c3d, y = Date, w = MC.TN..µM., z = 'mctn')
actn = mon_fun1(x = c3d, y = Date, w = AC.TN..µM., z = 'actn')
mctp = mon_fun1(x = c3d, y = Date, w = MC.FIU.TP..µM., z = 'mctp')
actp = mon_fun1(x = c3d, y = Date, w = AC.FIU.TP..µM., z = 'actp')
mcsal = mon_fun1(x = c3d, y = Date, w = MC.Sal..ppt., z = 'mcsal')
acsal = mon_fun1(x = c3d, y = Date, w = AC.Sal..ppt., z = 'acsal')

c3dlist <- list(mctn, actn, mctp, actp, mcsal, acsal)
c3dm <- reduce(c3dlist, full_join, by = "date")

##final dataset for 3 day nutrients is c3dm

head(cm)

f <- cm %>%
  mutate(Date = mdy(Date),  # Convert to Date object
         month = as.Date(format(Date, "%Y-%m-01"))) %>%  # Extract year-month
  group_by(month) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"))
head(f)

#rename columns
f <- f %>% rename(date = month, mctnm = mean_MC.TN..µM., actnm = mean_AC.TN..µM., mctpm = mean_MC.FIU.TP..µM., actpm = mean_AC.FIU.TP..µM., mcnn = mean_MC.N.N..µM., acnn = mean_AC.N.N..µM., mcno3 = mean_MC.NO3..µM., acno3 = mean_AC.NO3..µM., mcno2 = mean_MC.NO2..µM., acno2 = mean_AC.NO2..µM., mcnh4 = mean_MC.NH4..µM., acnh4 = mean_AC.NH4..µM., mcsrp = mean_MC.SRP..µM., acsrp = mean_AC.SRP..µM., mcdoc = mean_MC.DOC..µM., acdoc = mean_AC.DOC..µM., mcsalm = mean_MC.Sal..ppt., acsalm = mean_AC.Sal..ppt.)

cdat <- merge(c3dm, f, by = 'date')

#final dataset for creek nutrients is cdat
#let's move onto creek discharge
head(af)

aflow <- mon_fun2(af, Daily.Date, DBKEY, Data.Value, 'af') %>% filter(DBKEY != "") %>% pivot_wider(names_from = DBKEY, values_from = af) %>% rename(aflow = flow, amaxstage = maxstage, aminstage = minstage)
mflow <- mon_fun2(mf, Daily.Date, DBKEY, Data.Value, 'mf') %>% filter(DBKEY != "") %>% pivot_wider(names_from = DBKEY, values_from = mf) %>% rename(mflow = flow, mmaxstage = maxstage, mminstage = minstage, mmeanstage = meanstage)
oflow <- mon_fun2(of, Daily.Date, DBKEY, Data.Value, 'of') %>% filter(DBKEY != "") %>% pivot_wider(names_from = DBKEY, values_from = of) %>% rename(oflow = flow, omaxstage = maxstage, ominstage = minstage, omeanstage = meanstage)
wflow <- mon_fun2(wf, Daily.Date, DBKEY, Data.Value, 'wf')%>% filter(DBKEY != "") %>% pivot_wider(names_from = DBKEY, values_from = wf) %>% rename(wflow = flow, wmeanstage = meanstage)



flowlist <- list(aflow, mflow, oflow, wflow)
flow <- reduce(flowlist, full_join, by = "date")

#final dataset for flow is flow lol
#now let's do grab samples
head(gg)

ggdat <- mon_fun2(gg, Collection_Date, Test.Name, Value, 'mean') %>% filter(Test.Name != "") %>% pivot_wider(names_from = Test.Name, values_from = mean) #%>% rename(aflow = flow, amaxstage = maxstage, aminstage = minstage)
str(ggdat)
colnames(ggdat) <- c('date', 'gTOC', 'trashchl', 'gDO', 'ktn', 'gNN', 'gNO3', 'gNO2', 'gTP', 'gsal', 'gturb', 'gtemp', 'gNH4', 'gAP', 'gOP', 'gsil', 'tn', 'gpH', 'gcar', 'trashchl2', 'trashchl3', 'trashchlb', 'trashchlc', 'trashph', 'gsecchi', 'con', 'gdepth', 'gchl', 'gchlb', 'gpheo', 'al', 'cal', 'chlor', 'hard', 'mg', 'pot', 'sod', 'sul', 'ds')
str(ggdat)

ggdat <- ggdat %>% dplyr::select(date, gTOC, gDO, ktn, gNN, gNO3, gNO2, gTP, gsal, gturb, gtemp, gNH4, gAP, gOP, tn, gpH, gsecchi, gdepth, gchl, gchlb) %>% mutate(gTN = coalesce(tn, ktn)) %>% dplyr::select(-tn, -ktn)

head(rg)
rgdat <- mon_fun2(rg, Collection_Date, Test.Name, Value, 'mean') %>% filter(Test.Name != "") %>% pivot_wider(names_from = Test.Name, values_from = mean) #%>% rename(aflow = flow, amaxstage = maxstage, aminstage = minstage)
str(rgdat)
colnames(rgdat) <- c('date', 'rTOC', 'trashchl', 'rDO', 'ktn', 'rNN', 'rNO3', 'rNO2', 'rTP', 'rsal', 'rturb', 'rtemp', 'rNH4', 'rAP', 'rOP', 'rsil', 'tn', 'rpH', 'rcar', 'trashchl2', 'trashchl3', 'trashchlb', 'trashchlc', 'trashph', 'rsecchi', 'con', 'rdepth', 'rchl', 'rchlb', 'rpheo', 'al', 'cal', 'chlor', 'hard', 'mr', 'pot', 'sod', 'sul', 'ds')
str(rgdat)

rgdat <- rgdat %>% dplyr::select(date, rTOC, rDO, ktn, rNN, rNO3, rNO2, rTP, rsal, rturb, rtemp, rNH4, rAP, rOP, tn, rpH, rsecchi, rdepth, rchl, rchlb) %>% mutate(rTN = coalesce(tn, ktn)) %>% dplyr::select(-tn, -ktn)

head(tg)

tgdat <- mon_fun2(tg, Collection_Date, Test.Name, Value, 'mean') %>% filter(Test.Name != "") %>% pivot_wider(names_from = Test.Name, values_from = mean) #%>% rename(aflow = flow, amaxstage = maxstage, aminstage = minstage)
str(tgdat)
colnames(tgdat) <- c('date', 'tTOC', 'trashchl', 'tDO', 'ktn', 'tNN', 'tNO3', 'tNO2', 'tTP', 'tsal', 'tturb', 'ttemp', 'tNH4', 'tAP', 'tOP', 'tsil', 'tn', 'tpH', 'tcar', 'trashchl2', 'trashchl3', 'trashchlb', 'trashchlc', 'trashph', 'tsecchi', 'con', 'tdepth', 'tchl', 'tchlb', 'tpheo', 'al', 'cal', 'chlor', 'hard', 'mt', 'pot', 'sod', 'sul', 'ds')
str(tgdat)

tgdat <- tgdat %>% dplyr::select(date, tTOC, tDO, ktn, tNN, tNO3, tNO2, tTP, tsal, tturb, ttemp, tNH4, tAP, tOP, tn, tpH, tsecchi, tdepth, tchl, tchlb) %>% mutate(tTN = coalesce(tn, ktn)) %>% dplyr::select(-tn, -ktn)

glist <- list(ggdat, rgdat, tgdat)
gdat <- reduce(glist, full_join, by = "date")

#final grab sample dataset is gdat
#next is rainfall
head(mr)

mrdat <- mr %>% mutate(date = mdy(Daily.Date)) %>% mutate(date = as.Date(format(date, '%Y-%m-01')))%>% group_by(date) %>% summarize(sum = sum(Data.Value, na.rm = T), mean = mean(Data.Value, na.rm = T)) %>% dplyr::rename(marshtotrain = sum, marshmeanrain = mean) %>% slice(-(1:187))

#clean dataset for rain is mrdat