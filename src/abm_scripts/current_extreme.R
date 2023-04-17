#Compile weather data - max current conditions (2011), weather data from 2011 in which the state of oklahoma experienced a drought.

library(suncalc) #used to calculate sunrise/sunset times
library(lubridate) #date conversion
library(ggplot2) #creating good graphs
library(okmesonet) #download 5min increments of weather data
library(dplyr) #data organization
library(data.table) #???
library(pbapply)
library(zoo) #data approximation

#Designating which computer R is being run on. 0 is personal computer, 1 is OBS computer
computer = 0
if(computer == 0){
  m = "meely"
} else if(computer == 1){
  m = "meelyn.pandit"
}

#Setting Working Directory
setwd(paste0("C:/Users/",m,"/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_092121/data_clean/",sep = ""))


model_dir = "abm_month_timeframe_092121/"
years = 2011 #only analyzing 2011 due to drought that occurred during the summer
mass = 16 #mass of bird being analyzed
tuc = (6.130*(log10(mass))) + 28.328 #upper critical limit

ext = pblapply(years,function(x){
  beginTime = paste0(x,"-05-01 00:00:00", sep = "") #start time is year in list of years, 05/01
  endTime = paste0(x,"-06-30 23:55", sep = "") #end time is year in list of years, 06/30
  
  stid <- "ERIC" #station ID, getting datta from Erick, OK
  stations <- read.csv(paste0("C:/Users/",m,"/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/",
                              model_dir, 
                              "data/okstations.csv", 
                              sep = ""), 
                       stringsAsFactors = F)
  lat = stations$nlat[which(stations$stid == stid)]
  lon = stations$elon[which(stations$stid == stid)]
  
  #Obtain data from OK Mesonet website
  updatestn() #get latest information on mesonet stations
  okstations = updatestn() #save latest information into okstations
  w <- okmts(begintime=beginTime,
               endtime=endTime, 
               variables = c("TAIR", "RELH","PRES"),
               station="eric",  
               localtime=F) #Need to download the data into UTC
  
  ###Changing Date to UTC and separating day, month, and year data
  w$DT = as.POSIXct(w$TIME, tz = "UTC")
  w$DTL = w$DT #Saves datetime into a new vector for local datetime
  w$DATE = as.POSIXct(substr(w$TIME,0,10))
  w$MONTH = as.factor(substr(w$TIME,6,7))
  w$DAY = as.factor(substr(w$TIME,9,10))
  w$YMD = w$DATE
  attributes(w$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
  w$YMDL <- as_date(w$DTL) #gives local (oklahoma) ymd date

  #Splitting w dataframe by date to calculate sunrise time
  dates = w %>% group_by(DATE)
  dates = as.list(group_split(dates))
  
  wd = NULL
  for(d in 1:length(dates)){
    weather = as.data.frame(dates[d])
    sr1 <- getSunlightTimes(date = as_date(weather$YMD[1]), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
    sr2 <- getSunlightTimes(date = as_date(weather$YMD[1]-86400), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
    sr1 <- sr1$sunrise
    sr2 <- sr2$sunrise
    weather$MAS <- ifelse(weather$DT<sr1, difftime(weather$DT, sr2, units = "mins"), difftime(weather$DT, sr1, units = "mins"))
    weather$MAS <- round(weather$MAS, 0)
    if(as.character(d) == as.character(dates[1])) {
      wd <- weather
    } else {
      wd <- rbind(wd,weather)
    }
    #print(paste0("Date Completed:", sep = " ", dates[d]))
  }
  
  labels <- seq(0,1435,5)
  wd$bins <- cut(wd$MAS,seq(0,1440,5), labels = labels, right = FALSE)               #make 5 minute bins
  wd$bins <- as.numeric(as.character(wd$bins))
  wd$DEW <- wd$TAIR-((100-wd$RELH)/5)
  wdsum <- data.frame(bin1 = wd$bins, 
                      bin2 = wd$bins+5,
                      date = wd$YMD, 
                      dateLocal = wd$YMDL,
                      monthLocal = substr(wd$YMDL,6,7),
                      dayLocal = substr(wd$YMDL, 9,10),
                      dayMonthLocal = substr(wd$YMDL,6,10),
                      year = substr(wd$YMDL,1,4),
                      DEW = wd$DEW,
                      TAIR = wd$TAIR,
                      RELH = wd$RELH,
                      PRES = wd$PRES)
  
  wdavg = wdsum %>% group_by(dayMonthLocal,
                             bin1,
                             bin2) %>%
    summarise(DEW = mean(DEW),
              TAIR = mean(TAIR),
              RELH = mean(RELH),
              PRES = mean(PRES) #,
              # EWL = mean(EWL),
              # TEWL = mean(TEWL)
    )
  
  wdavg$TEWL = 0 #total evaporative water loss increases until it reaches 2.4
  wdavg$EWL = NA
  return(wdavg)
})

extreme = ext[[1]] #exporting dataframe from lapply function, dataframe still has missing rows

#determine which rwos are missing from extreme dataframe by ocmparing it to current datagrame
load(paste0("C:/Users/",m,"/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/", model_dir, "data/ERIC_current.Rdata", sep = ""))
current = wdavg #save the dataframe as current
missing_data = current[,c(1:3)] %>% anti_join(extreme[,c(1:3)])

combined = bind_rows(extreme,missing_data) %>%
  arrange(dayMonthLocal,bin1,bin2)
missing_data2 = combined[,c(1:3)] %>% anti_join(current[,c(1:3)])

wdavg = combined %>%
  filter(!is.na(bin1))

wdavg = wdavg %>%
  arrange(dayMonthLocal,bin1,bin2)

wdavg$DEW = na.approx(wdavg$DEW) #approximate dewpoint temperatuer
wdavg$TAIR = na.approx(wdavg$TAIR) #approximate air temperature
wdavg$RELH = na.approx(wdavg$RELH) #approximate relative humidity
wdavg$PRES = na.approx(wdavg$PRES) #approximate air pressure


source(paste0("C:/Users/",m,"/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/", model_dir, "src/ewl_calculations.R", sep = ""))

#Calculate EWL for each 5 min interval
wdavg$EWL = sapply(wdavg, function(x) ewl_albright(mass,wdavg$TAIR)) #albright equation

for(j in 1:length(wdavg$bin1)){ #moved EWL equations after summarising data to see if the average EWL differs at all.
  if(j == 1){
    wdavg$TEWL[j] = 0 + wdavg$EWL[j]
  } else if(wdavg$bin1[j] == 0 && wdavg$bin2[j] == 5){
    wdavg$TEWL[j] = 0
  } else if(wdavg$bin1[j] == 0 && wdavg$bin1[j-1] == 1435){
    wdavg$TEWL[j] = 0
  } else if(wdavg$bin1[j] == 5 && wdavg$bin1[j-1] != 0){
    wdavg$TEWL[j] = 0
  } else if(is.na(wdavg$TAIR[j])==TRUE){
    wdavg$EWL[j] = 0
  } else {
    wdavg$TEWL[j] = wdavg$TEWL[j-1]+wdavg$EWL[j]
  }
}



#Use station ID to access folder and get a list of data files
setwd(paste0("C:/Users/",m,"/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/", model_dir, "data/", sep = ""))
stid <- "ERIC"
fname = paste0(stid, "_current_extreme")
file_list <- as.list(list.files(fname))
save(wdavg, file = paste0(fname, ".Rdata"))




# Graveyard ---------------------------------------------------------------

w=NULL
for(i in years){ #beginning of weather data loop
  beginTime = paste0(i,"-05-01 00:00:00", sep = "") #start time is year in list of years, 05/01
  endTime = paste0(i,"-06-30 23:55", sep = "") #end time is year in list of years, 06/30
  
  stid <- "ERIC" #station ID, getting datta from Erick, OK
  stations <- read.csv(paste0("C:/Users/",m,"/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/",
                              model_dir, 
                              "data/okstations.csv", 
                              sep = ""), 
                       stringsAsFactors = F)
  lat = stations$nlat[which(stations$stid == stid)]
  lon = stations$elon[which(stations$stid == stid)]
  
  #Obtain data from OK Mesonet website
  updatestn() #get latest information on mesonet stations
  okstations = updatestn() #save latest information into okstations
  wok <- okmts(begintime=beginTime,
               endtime=endTime, 
               variables = c("TAIR", "RELH","PRES"),
               station="eric",  
               localtime=F) #Need to download the data into UTC
  w = rbind(wok,w)
}

###Changing Date to UTC and separating day, month, and year data
w$DT = as.POSIXct(w$TIME, tz = "UTC")
w$DTL = w$DT #Saves datetime into a new vector for local datetime
w$DATE = as.POSIXct(substr(w$TIME,0,10))
w$MONTH = as.factor(substr(w$TIME,6,7))
w$DAY = as.factor(substr(w$TIME,9,10))
w$YMD = w$DATE
attributes(w$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
w$YMDL <- as_date(w$DTL) #gives local (oklahoma) ymd date

#Splitting w dataframe by date to calculate sunrise time
dates = w %>%
  group_by(DATE)

dates = as.list(group_split(dates))

wd = NULL
for(d in 1:length(dates)){
  weather = as.data.frame(dates[d])
  sr1 <- getSunlightTimes(date = as_date(weather$YMD[1]), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
  sr2 <- getSunlightTimes(date = as_date(weather$YMD[1]-86400), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
  sr1 <- sr1$sunrise
  sr2 <- sr2$sunrise
  weather$MAS <- ifelse(weather$DT<sr1, difftime(weather$DT, sr2, units = "mins"), difftime(weather$DT, sr1, units = "mins"))
  weather$MAS <- round(weather$MAS, 0)
  if(as.character(d) == as.character(dates[1])) {
    wd <- weather
  } else {
    wd <- rbind(wd,weather)
  }
  #print(paste0("Date Completed:", sep = " ", dates[d]))
}

labels <- seq(0,1435,5)
wd$bins <- cut(wd$MAS,seq(0,1440,5), labels = labels, right = FALSE)               #make 5 minute bins
wd$bins <- as.numeric(as.character(wd$bins))
wd$DEW <- wd$TAIR-((100-wd$RELH)/5)

wd = wd %>%
  # filter(!is.na(TAIR)) %>%
  filter(!is.na(bins))

setwd(paste0("C:/Users/meelyn.pandit/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/",
             model_dir, 
             "data/", 
             sep = ""))
save(wd, file = "current_extreme.Rdata")

load("current_extreme.Rdata")

wdsum <- data.frame(bin1 = wd$bins, 
                    bin2 = wd$bins+5,
                    date = wd$YMD, 
                    dateLocal = wd$YMDL,
                    monthLocal = substr(wd$YMDL,6,7),
                    dayLocal = substr(wd$YMDL, 9,10),
                    dayMonthLocal = substr(wd$YMDL,6,10),
                    year = substr(wd$YMDL,1,4),
                    DEW = wd$DEW,
                    TAIR = wd$TAIR,
                    RELH = wd$RELH, #10% decrease in relative humidity
                    PRES = wd$PRES)

wdavg = wdsum %>% group_by(dayMonthLocal,
                           bin1,
                           bin2) %>%
  summarise(DEW = mean(DEW),
            TAIR = mean(TAIR),
            RELH = mean(RELH),
            PRES = mean(PRES) #,
            # EWL = mean(EWL),
            # TEWL = mean(TEWL)
  )

wdavg$TEWL = 0 #total evaporative water loss increases until it reaches 2.4
wdavg$EWL = NULL

source(paste0("C:/Users/",m,"/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/",
              model_dir, 
              "src/ewl_calculations.R", 
              sep = ""))

#Calculate EWL for each 5 min interval
wdavg$EWL = sapply(wdavg, 
                   function(x) ewl_albright(mass,
                                            wdavg$TAIR)) #albright equation


for(j in 1:length(wdavg$bin1)){ #moved EWL equations after summarising data to see if the average EWL differs at all.
  if(j == 1){
    wdavg$TEWL[j] = 0 + wdavg$EWL[j]
  } else if(wdavg$bin1[j] == 0 & wdavg$bin2[j] == 5){
    wdavg$TEWL[j] = 0
  } else if(wdavg$bin1[j] == 0 && wdavg$bin1[j-1] == 1435){
    wdavg$TEWL[j] = 0
  } else if(wdavg$bin1[j] == 5 && wdavg$bin1[j-1] != 0){
    wdavg$TEWL[j] = 0
  } else if(is.na(wdavg$TAIR[j])==TRUE){
    wdavg$EWL[j] = 0
  } else {
    wdavg$TEWL[j] = wdavg$TEWL[j-1]+wdavg$EWL[j]
  }
}

# wdavg$RMR = rmr_equation(mass,wdavg$TAIR, tuc)
# wdavg$dateLocal = paste0("2010-",wdavg$dayMonthLocal,sep ="") #have to add a year to this variable so the model will actualy run

#Use station ID to access folder and get a list of data files
setwd(paste0("C:/Users/meelyn.pandit/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/",
             model_dir, 
             "data/", 
             sep = ""))
stid <- "ERIC"
fname = paste0(stid, "_current_extreme")
file_list <- as.list(list.files(fname))

#06/13/2011 and 06/14/2011 have missing rows, each bin1 differs by 10 instead of 5, run the following code to filter out these dates
wdavg = wdavg %>%
  filter(dayMonthLocal != "06-13") %>%
  filter(dayMonthLocal != "06-14")

save(wdavg, file = paste0(fname, ".Rdata"))




#Graph some data.
load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_110220/data/ERIC_weather_normal/ERIC_weather_normal.Rdata") #Load wdsum data so you do not need to run code again:

Song_volume = 85
Song_detection = 30
Song_freq = 7000
source("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_110220/src/Atmospheric_sound_attenuation.R")
wdsum$CallRad <- mapply(aud_range,Song_volume,Song_detection,Song_freq,wdsum$TAIR,wdsum$RELH,wdsum$PRES)
wdtemp <- wdsum[which(wdsum$bin1<=600),]
wdtemp$dateLocal <- as.factor(wdtemp$dateLocal )

wmeans = wdtemp %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(model, bin1) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   CallRadMean = mean(CallRad), 
                   CallRadSE = (sd(CallRad)/sqrt(n)),
                   TAIRMean = mean(TAIR),
                   RELHMean = mean(RELH),
                   PRESMean = mean(PRES),
                   DEWMean = mean(DEW)
  )
ggplot( data = wmeans, aes(x=bin1, y=CallRadMean, group=model, color = model)) +
  geom_line()+
  theme_classic()

ggplot( data = wmeans, aes(x=bin1, y=TAIRMean, group=model, color = model)) +
  geom_line()+
  theme_classic()

ggplot( data = wmeans, aes(x=bin1, y=DEWMean, group=model, color = model)) +
  geom_line() +
  theme_classic()+
  labs(x= "Min from Sunrise",
       y = "Dewpoint \nTemperature")

ggplot( data = wdtemp, aes(x=bin1, y=RELH, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()

ggplot( data = wdtemp, aes(x=DEW, y=CallRad, group=dateLocal, color = dateLocal)) +
  geom_line()

ggplot( data = wdtemp, aes(x=DEW, y=CallRad)) +
  geom_point()

ggplot( data = wdtemp, aes(x=TAIR, y=DEW, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()+
  labs(x= "Air Temperature (C)",
       y = "Dewpoint \nTemperature")

ggplot( data = wdtemp, aes(x=TAIR, y=RELH, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()+
  labs(x= "Air Temperature (C)",
       y = "Relative \nHumidity (%)")
