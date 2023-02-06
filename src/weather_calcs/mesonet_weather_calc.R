library(nlme) #AICc tests
library(lme4) #linear mixed models
library(lmerTest) #get p-values for LMMs
library(ggplot2) #graphing
library(ggthemes) #classical them for ggplot2
library(gridExtra) #create multipanel plots in ggplot2
library(nortest) #???
library(car) #Anova() function
library(multcomp) #posthoc tests for ANOVA type III effects
library(dplyr) #data organization
library(tidyverse) #data organization
library(suncalc) #caculate sunrise time and sun altitude
library(lubridate) #date manipulation
library(hms) #time manipulation
library(bbmle) #AIC
library(readr) #make loading csvs easier?
library(zoo) #approximate missing values
library(okmesonet) #obtain weather data from OK mesonet
library(beepr)


# Calculating Maximum Saturation Humidty ratio of air (mass of water vapor in kg/ mass of dry air in kg) --------
max_humid = data.frame(temp = c(0,5,10,15,20,25,30),
                      max_sat = c(0.003767,0.005387,0.007612,0.01062,0.014659,0.019826,0.027125))
#Determined in Excel
# max_sat = 0.0039(e^(0.0656*x))
max_sat <- function(temp){
  y = 0.0039*(exp(0.0656*temp))
  return(y)
}

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/")


# Obtain Weather Data from OK Mesonet for LWMA and SSWMA ------------------
# okstations = updatestn();okstations #save latest information into okstations
beginTime = as.POSIXct("2021-05-01 00:00:00", tz = "UTC") #2021-04-30 19:00:00 CDT
endTime = as.POSIXct("2021-08-31 23:55::00", tz = "UTC") #2021-08-31 18:55:00 CDT

# LWMA - Washington OK Station --------------------------------------------

lwma_weather <- okmts(begintime=beginTime,
             endtime=endTime, 
             variables = "ALL",
             station="WASH",  
             localtime=TRUE,
             missingNA = TRUE) #downloads date time into UTC
lwma_weather$date_time = as_datetime(lwma_weather$TIME, tz = "UTC");lwma_weather$date_time




load("lwma_sunrise.Rdata")

# lwma_sunrise = NULL
# for(i in 1:length(lwma_weather$date_time)){
#   sunrise_time = getSunlightTimes(date = as_date(lwma_weather$date_time[i]), lat = 34.98224, lon = -97.52109, tz = "UTC", keep = c("sunrise"))
#   sunrise_loc = getSunlightPosition(date = lwma_weather$date_time[i], lat = 34.98224, lon = -97.52109, keep = c("altitude"))
#   sunrise_time$altitude = sunrise_loc$altitude
#   sunrise_time$date_time = lwma_weather$date_time[i]
#   sunrise_time$lat = 34.98224
#   sunrise_time$lon = -97.52109
#   
#   lwma_sunrise = rbind(sunrise_time,lwma_sunrise)
# }
lwma_weather$lat = 34.98224
lwma_weather$lon = -97.52109
lwma_weather2 = inner_join(lwma_weather,lwma_sunrise, by = c("date_time"))

lwma_weather2 = lwma_weather2 %>% dplyr::arrange(date_time)
lwma_weather2$TAIR = na.approx(lwma_weather2$TAIR, na.rm = FALSE) # air temperature at 1.5 m
lwma_weather2$RELH = na.approx(lwma_weather2$RELH, na.rm = FALSE) # relative humidity at 1.5 m
lwma_weather2$PRES = na.approx(lwma_weather2$PRES, na.rm = FALSE) # atmospheric pressure
lwma_weather2$RAIN = na.approx(lwma_weather2$RAIN, na.rm = FALSE) # precipitation
lwma_weather2$WSPD = na.approx(lwma_weather2$WSPD, na.rm = FALSE) # wind speed at 10m
lwma_weather2$WS2M = na.approx(lwma_weather2$WS2M, na.rm = FALSE) # wind speed at 2m

# find relationship between wind speed at 2m and at 10m so you can estimate wind speed at cbma and kiowa sites

lm(WS2M ~ WSPD, data = lwma_weather) # slope = 0.8440, 
                                     # intercept = -0.5302


# lwma_weather$sunrise = na.approx(lwma_weather$sunrise, na.rm = FALSE)
# lwma_weather$altitude = na.approx(lwma_weather$altitude, na.rm = FALSE)

lwma_weather3 = lwma_weather2 %>%
  mutate(hour = hour(date_time),
         site = "lwma",
         dew = TAIR-((100-RELH)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,
                                   sunrise,
                                   units = c("mins"))),
         rain = if_else((lead(RAIN)-RAIN) >= 0, (lead(RAIN)-RAIN), 0),
         gh = (25+(19*WS2M) * 1 *(max_sat(TAIR)-(RELH/100)))) %>%
  dplyr::rename(temp = "TAIR",
         relh = "RELH") 
# Evaporation rate equation: https://www.engineeringtoolbox.com/evaporation-water-surface-d_690.html
# gh = Θ A (xs - x); amounted of water evaporated per hour
# gh = (25 + (19*v)) * A * (xs - x)
# A = area of water being evaporated; 1 m2 in our equation
# Θ = (25 + (19*v)); v = velocity in m/s
# xs = max humidity ratio of saturated air
# x = humidity air ratio (relh in our equation)

lwma_missing = lwma_weather3 %>% dplyr::filter(is.na(temp)==TRUE)

# labels <- seq(0,1435,5)
# bins <- cut(lwma_weather$mas,seq(0,1440,5), labels = labels, right = FALSE)#make 5 minute bins
# bins <- as.numeric(as.character(bins))

lwma_hour = lwma_weather3 %>%
  mutate(hour = hour(date_time),
         site = "lwma",
         dew = temp-((100-relh)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site,hour) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))

# SSWMA - Erick, OK Station -----------------------------------------------
sswma_weather <- okmts(begintime=beginTime,
                endtime=endTime, 
                variables = "ALL",
                station="ERIC",  
                localtime=FALSE,
                missingNA = TRUE) #downloads date time into UTC
sswma_weather$date_time = as_datetime(sswma_weather$TIME, tz = "UTC");sswma_weather$date_time

load("sswma_sunrise.Rdata")

# sswma_sunrise = NULL
# for(i in 1:length(sswma_weather$date_time)){
#   sunrise_time = getSunlightTimes(date = as_date(sswma_weather$date_time[i]), lat = 35.20494, lon = -99.803449, tz = "UTC", keep = c("sunrise"))
#   sunrise_loc = getSunlightPosition(date = sswma_weather$date_time[i], lat = 35.20494, lon = -99.803449, keep = c("altitude"))
#   sunrise_time$altitude = sunrise_loc$altitude
#   sunrise_time$date_time = sswma_weather$date_time[i]
#   # sunrise_time$site = sswma_weather$site[i]
#   # sunrise_time$aru = sswma_weather$aru[i]
#   
#   sswma_sunrise = rbind(sunrise_time,sswma_sunrise)
# }

sswma_weather2 = inner_join(sswma_weather,
                            sswma_sunrise, by = c("date_time"))
sswma_weather2 = sswma_weather2 %>% dplyr::arrange(date_time)

sswma_weather2$TAIR = na.approx(sswma_weather2$TAIR, na.rm = FALSE)
sswma_weather2$RELH = na.approx(sswma_weather2$RELH, na.rm = FALSE)
sswma_weather2$PRES = na.approx(sswma_weather2$PRES, na.rm = FALSE)
sswma_weather2$TAIR = na.approx(sswma_weather2$TAIR, na.rm = FALSE) # air temperature at 1.5 m
sswma_weather2$RELH = na.approx(sswma_weather2$RELH, na.rm = FALSE) # relative humidity at 1.5 m
sswma_weather2$PRES = na.approx(sswma_weather2$PRES, na.rm = FALSE) # atmospheric pressure
sswma_weather2$RAIN = na.approx(sswma_weather2$RAIN, na.rm = FALSE) # precipitation
sswma_weather2$WSPD = na.approx(sswma_weather2$WSPD, na.rm = FALSE) # wind speed at 10m
sswma_weather2$WS2M = na.approx(sswma_weather2$WS2M, na.rm = FALSE) # wind speed at 2m

# find relationship between wind speed at 2m and at 10m so you can estimate wind speed at cbma and kiowa sites
plot(sswma_weather2$WSPD, sswma_weather2$WS2M)
lm(WS2M ~ WSPD, data = sswma_weather2) # slope = 0.7607, 
# intercept = -0.3861

#estimating wind speed at 2m with wind speed at 10m data
# https://www.researchgate.net/post/Is-that-possible-to-convert-wind-speed-measured-in-10-m-height-to-a-possible-2-m-height-wind-speed
u2 = (u10*4.87)/(log((67.8*10)-5.42))
# u2 is wind speed at 2m
# u10 is wind speed at 10m
# u2 is approximately  = 0.75 * u10


# sswma_weather$sunrise = na.approx(sswma_weather$sunrise, na.rm = FALSE)
# sswma_weather$altitude = na.approx(sswma_weather$altitude, na.rm = FALSE)

sswma_weather3 = sswma_weather2 %>%
  mutate(hour = hour(date_time),
         site = "sswma",
         dew = TAIR-((100-RELH)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))),
         rain = if_else((lead(RAIN)-RAIN) >= 0, (lead(RAIN)-RAIN), 0),
         gh = (25+(19*WS2M) * 1 *(max_sat(TAIR)-(RELH/100)))) %>%
  dplyr::rename(temp = "TAIR",
         relh = "RELH")
  

sswma_missing = sswma_weather3 %>% dplyr::filter(is.na(temp)==TRUE)

sswma_hour = sswma_weather3 %>%
  mutate(hour = hour(date_time),
         site = "sswma",
         dew = temp-((100-relh)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site,hour) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))

# Clean CBMA Weather Data obtained from Texas Mesonet ---------------------

# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/")

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data/mesonet_data")

cbma_weather = read_csv("cbma_mesonet.csv", col_names = TRUE, col_types = "ccnnnTnnnnnnnnnnnnnc") %>%
  dplyr::rename(station = "Station_ID",
                name = "Name",
                lat = "Latitude",
                lon = "Longitude",
                elev_m = "Elevation (m)",
                date_time = "Date_Time (UTC)",
                dew = "Dew Point (c)",
                temp = "Temperature (c)",
                relh = "Relative Humidity (%)",
                pres = "Sea_level pressure (pa)",
                rain = "Precipitation 1hr (mm)",
                rain24 = "Precipitation 24hr (mm)") %>%
  dplyr::filter(date(date_time) > "2021-04-30")

# Need to convert Cbma wind values from knots to m/s
#1 kn = 0.514444 m/s
cbma_wind = read_csv("cbma_mesonet_wind.csv", col_names = TRUE, col_types = "ccnnnTnnnnn") %>%
  dplyr::rename(station = "Station_ID",
                name = "Name",
                lat = "Latitude",
                lon = "Longitude",
                elev_m = "Elevation (ft)",
                date_time = "Date_Time (UTC)",
                peak_wind_dir = "Peak_Wind Direction (Degrees)",
                peak_wind_speed = "Peak_Wind Speed (mph)",
                wind_dir = "Wind Direction 10m (deg)",
                wind_gust = "Wind Gust 10m (kn)",
                wind_speed_10m = "Wind Speed 10m (kn)") %>%
  dplyr::filter(date(date_time) > "2021-04-30") %>%
  dplyr::mutate(ws10m = 0.514444*wind_speed_10m) %>%
  dplyr::mutate(ws2m = (ws10m*4.87)/(log((67.8*10)-5.42))
    )

cbma_weather = cbma_weather %>% arrange(date_time)
cbma_weather$temp = na.approx(cbma_weather$temp, na.rm = FALSE)
cbma_weather$relh = na.approx(cbma_weather$relh, na.rm = FALSE)
cbma_weather$pres = na.approx(cbma_weather$pres, na.rm = FALSE)
cbma_weather$dew = na.approx(cbma_weather$dew, na.rm = FALSE)
# cbma_weather$rain = na.approx(cbma_weather$rain, na.rm = FALSE)
# cbma_weather$rain24 = na.approx(cbma_weather$rain24, 
                                 # na.rm = FALSE)

# don't need to approximate rain, only using presence/absence of rain to filter out files where rain would distort audio
# Get Sunrise data for CBMA site (or load it if you have already obtained it)
load("cbma_sunrise.Rdata")

# cbma_sunrise = NULL
# for(i in 1:length(cbma_weather$date_time)){ #have to use lwma date_time because it is complete data_time vector
#   sunrise_time = getSunlightTimes(date = as_date(cbma_weather$date_time[i]), lat = 35.695, lon = -101.395, tz = "UTC", keep = c("sunrise"))
#   sunrise_loc = getSunlightPosition(date = cbma_weather$date_time[i], lat = 35.695, lon = -101.395, keep = c("altitude"))
#   sunrise_time$altitude = sunrise_loc$altitude
#   sunrise_time$date_time = cbma_weather$date_time[i]
#   # sunrise_time$site = cbma_weather$site[i]
#   # sunrise_time$aru = cbma_weather$aru[i]
#   
#   cbma_sunrise = rbind(sunrise_time,cbma_sunrise)
# }

cbma_weather1.5 = right_join(cbma_weather,cbma_wind[,-c(1:5)], by = c("date_time"))

cbma_weather2 = right_join(cbma_weather1.5,cbma_sunrise, by = "date_time")
# cbma_weather2= cbma_weather%>%arrange(date_time) %>%
#   dplyr::filter(minute(date_time) == 0 | 
#                   minute(date_time) == 5|
#                   minute(date_time) == 10|
#                   minute(date_time) == 15|
#                   minute(date_time) == 20|
#                   minute(date_time) == 25|
#                   minute(date_time) == 30|
#                   minute(date_time) == 35|
#                   minute(date_time) == 40|
#                   minute(date_time) == 45|
#                   minute(date_time) == 50|
#                   minute(date_time) == 55)
# cbma_weather2 = cbma_weather2 %>%
#   dplyr::filter(is.na(sunrise) == TRUE)

# Approximating values again so there are no NA values
cbma_weather2 = cbma_weather2 %>% arrange(date_time)
cbma_weather2$temp = na.approx(cbma_weather2$temp, na.rm = FALSE)
cbma_weather2$relh = na.approx(cbma_weather2$relh, na.rm = FALSE)
cbma_weather2$pres = na.approx(cbma_weather2$pres, na.rm = FALSE)
cbma_weather2$dew = na.approx(cbma_weather2$dew, na.rm = FALSE)
cbma_weather2$ws2m = na.approx(cbma_weather2$ws2m, 
                               na.rm = FALSE)
# cbma_weather$relh2 = relhum(cbma_weather$temp, cbma_weather$dew)
##just use relh from cbma sites

cbma_missing = cbma_weather2 %>% dplyr::filter(is.na(temp)==TRUE)
# only missing data from 08/31 so we are good

cbma_weather3 = cbma_weather2 %>%
  mutate(hour = hour(date_time),
         site = "cbma",
         dew = temp-((100-relh)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,
                                   sunrise,
                                   units = c("mins"))),
         gh = (25+(19*ws2m) * 1 *(max_sat(temp)-(relh/100))))

cbma_weather3$gh = na.approx(cbma_weather3$gh, na.rm = FALSE)
# just 08/31 so ok

cbma_hour = cbma_weather3 %>%
  mutate(hour = hour(date_time),
         site = "cbma",
         dew = temp-((100-relh)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site,hour) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))

# Seeing if relh or gh is missing
cbma_missing = cbma_weather3 %>% dplyr::filter(is.na(gh)==TRUE)
# only 08/31 so good

ggplot(data = cbma_weather3,
       aes(x = relh, y = gh)) + 
  geom_point()

# Clean Kiowa Dataset obtained from Texas Mesonet -------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data/mesonet_data")

kiowa_weather = read_csv("kiowa_mesonet.csv", 
                         col_names = TRUE, 
                         col_types = "ccnnnTnnnnnnnnnnc") %>%
  dplyr::rename(station = "Station_ID",
                lat = "Latitude",
                lon = "Longitude",
                elev_m = "Elevation (m)",
                date_time = "Date_Time (UTC)",
                temp = "Temperature (c)",
                relh = "Relative Humidity (%)",
                rain_acc = "Precipitation accumulated (mm)")%>%
  arrange(date_time)

# Determine kiowa rain (mm) per day like OK mesonet sites
kiowa_weather = kiowa_weather %>%
  dplyr::mutate(rain = if_else((lead(rain_acc)-rain_acc) >= 0, (lead(rain_acc)-rain_acc), 0))

# Calculate wind speeds from 3 adjacent sites from iowa mesonet (Clayton, Raton, Las Vegas)

devtools::install_github("rspatial/rspat")
devtools::install_github("r-spatial/gstat")

library(rspat)
library(gstat)
library(tidyverse)
library(sp)
library(raster)

nm_mesonet = read_csv("kiowa_wind.csv") %>% 
  dplyr::rename(date_time = "valid") %>%
  dplyr::arrange(date_time)

nm_sites = nm_mesonet %>%
  dplyr::filter(station != "kiowa") %>%
  dplyr::filter(is.na(sknt) == FALSE)

kiowa = nm_mesonet %>%
  dplyr::filter(station == "kiowa")

nm_mesonet2 = rbind(nm_sites, kiowa) %>%
  dplyr::arrange(date_time)

coordinates(nm_mesonet)=~lat+lon
# d$N[c(5,6,7)]=NA
valid = !is.na(nm_mesonet$sknt)
predictions = idw(sknt~1, 
                  locations=nm_mesonet[valid,,drop=FALSE],
                  newdata=nm_mesonet[!valid,,drop=FALSE])
nm_mesonet$sknt[!valid] = predictions$var1.pred #putting interpolated data back into dataframe

notkiowa_wind = nm_mesonet@data %>% 
  dplyr::filter(station != "kiowa")

kiowa_wind = nm_mesonet@data %>%
  dplyr::filter(station == "kiowa") %>%
  dplyr::mutate(date_time = mdy_hm(date_time)) %>%
  dplyr::select(date_time, sknt) %>%
  dplyr::arrange(date_time)

kiowa_weather1.5 = left_join(kiowa_weather, kiowa_wind[,c("date_time","sknt")], by = "date_time")

# Calculate sunrise times for kiowa site (or load data if calculated before)
load("kiowa_sunrise.Rdata")


# kiowa_sunrise = NULL
# for(i in 1:length(kiowa_weather$date_time)){
#   sunrise_time = getSunlightTimes(date = as_date(kiowa_weather$date_time[i]), lat = 36.055, lon = -104.324, tz = "UTC", keep = c("sunrise"))
#   sunrise_loc = getSunlightPosition(date = kiowa_weather$date_time[i], lat = 36.055, lon = -104.324, keep = c("altitude"))
#   sunrise_time$altitude = sunrise_loc$altitude
#   sunrise_time$date_time = kiowa_weather$date_time[i]
#   # sunrise_time$site = kiowa_weather$site[i]
#   # sunrise_time$aru = kiowa_weather$aru[i]
#   
#   kiowa_sunrise = rbind(sunrise_time,kiowa_sunrise)
# }

# kiowa_date = as.data.frame(kiowa_sunrise$date_time)
# names(kiowa_date) = c("date_time")
# kiowa_weather2 = full_join(kiowa_weather1.5,kiowa_sunrise, by = c("date_time")) %>% arrange(date_time) %>% dplyr::distinct(date_time, .keep_all = TRUE)

kiowa_weather2 = full_join(kiowa_weather1.5,kiowa_sunrise, by = c("date_time"))%>%
  arrange(date_time)


kiowa_weather2$temp = na.approx(kiowa_weather2$temp, na.rm = FALSE)
kiowa_weather2$relh = na.approx(kiowa_weather2$relh, na.rm = FALSE)
kiowa_weather2$rain = na.approx(kiowa_weather2$rain, na.rm = FALSE)
kiowa_weather2$ws10m = na.approx(kiowa_weather2$sknt, na.rm = FALSE)


# kiowa_weather2$altitude = na.approx(kiowa_weather2$altitude, na.rm = FALSE)

# kiowa_weather2$pres = na.approx(kiowa_weather2$pres, na.rm = FALSE)

kiowa_weather3 = kiowa_weather2 %>%
  dplyr::filter(date(date_time)> "2021-04-30")%>%
  dplyr::filter(date(date_time)< "2021-09-01")%>%
  dplyr::mutate(hour = hour(date_time),
         site = "kiowa",
         dew = temp-((100-relh)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,
                                   sunrise,
                                   units = c("mins"))),
         ws2m = (ws10m*4.87)/(log((67.8*10)-5.42))) %>%
  dplyr::mutate(gh = (25+(19*ws2m) * 1 *(max_sat(temp)-(relh/100))))

kiowa_missing = kiowa_weather %>% dplyr::filter(is.na(temp)==TRUE)

kiowa_hour = kiowa_weather3 %>%
  mutate(hour = hour(date_time),
         site = "kiowa",
         dew = temp-((100-relh)/5),
         arid = abs((1/dew)),
         mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site,hour) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))

save(lwma_sunrise, file = "lwma_sunrise.Rdata")
save(sswma_sunrise, file = "sswma_sunrise.Rdata")
save(cbma_sunrise, file = "cbma_sunrise.Rdata")
save(kiowa_sunrise, file = "kiowa_sunrise.Rdata")

lwma_mesonet = lwma_weather3 %>%
  select(site, date_time,sunrise,hour,temp,relh,dew,arid,mas,altitude,gh, rain, ws2m)
sswma_mesonet = sswma_weather3 %>%
  select(site, date_time,sunrise,hour,temp,relh,dew,arid,mas,altitude,gh, rain, ws2m)
cbma_mesonet = cbma_weather3 %>%
  select(site, date_time,sunrise,hour,temp,relh,dew,arid,mas,altitude,gh, rain, ws2m)
kiowa_mesonet = kiowa_weather3 %>%
  dplyr::select(site, date_time,sunrise,hour,temp,relh,dew,arid,mas,altitude,gh, rain, ws2m)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean/mesonet_data")

save(lwma_mesonet, file = "lwma_mesonet.Rdata")
save(sswma_mesonet, file = "sswma_mesonet.Rdata")
save(cbma_mesonet, file = "cbma_mesonet.Rdata")
save(kiowa_mesonet, file = "kiowa_mesonet.Rdata")



# Filtering data for graphs -----------------------------------------------
lwma = lwma_hour %>%
  select(site, hour,mean_temp,mean_relh,mean_dew,mean_arid,mean_mas,mean_sunalt,mean_gh)
sswma = sswma_hour %>%
  select(site, hour,mean_temp,mean_relh,mean_dew,mean_arid,mean_mas,mean_sunalt,mean_gh)
cbma = cbma_hour %>%
  select(site, hour,mean_temp,mean_relh,mean_dew,mean_arid,mean_mas,mean_sunalt,mean_gh)
kiowa = kiowa_hour %>%
  select(site, hour,mean_temp,mean_relh,mean_dew,mean_arid,mean_mas,mean_sunalt,mean_gh)

sites = rbind(lwma,sswma,cbma,kiowa)
sites$site = factor(sites$site, levels=c("lwma","sswma","cbma","kiowa"))

ggplot(data = sites, aes(x = hour, y = mean_relh, color = site))+
  geom_line()

#something is wrong with hour 5 in kiowa dataset
hour5 = kiowa_weather %>% dplyr::filter(hour(date_time)==5) %>%
  mutate(dew = temp-((100-relh)/5),
         arid = (1/dew))
         


# Historical Data 2005-2021 LWMA, Washington, OK Station-----------------------------------------------
# year = as.list(c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) #16 years of historic Mesonet weather data
# year = as.list(c(2005,2006)) #have to break up into smaller chunks because Mesonet will lose connection to API
# year = as.list(c(2007,2008))
# year = as.list(c(2009,2010))
# year = as.list(c(2011,2012))
# year = as.list(c(2013,2014))
# year = as.list(c(2015,2016))
# year = as.list(c(2017,2018))
year = as.list(c(2019,2020,2021))
# year = as.list(c(2007,2008,2009,2010,2011)) #comment/uncomment years that you are going to acquire
# year = as.list(c(2012,2013,2014,2015,2016))
# year = as.list(c(2017,2018,2019,2020,2021))

#Obtain data from Mesonet API
lwma_history = lapply(year,function(x){
  beginTime = as.POSIXct(paste0(x,"-05-01 00:00:00", tz = "UTC")) #2021-04-30 19:00:00 CDT
  endTime = as.POSIXct(paste0(x,"-08-31 23:55::00", tz = "UTC")) #2021-08-31 18:55:00 CDT
  lwma_weather <- okmts(begintime=beginTime,
                        endtime=endTime, 
                        variables = c("TAIR", "RELH","PRES"),
                        station="WASH",  
                        localtime=TRUE, #get local time and convert to UTC using as_datetime
                        missingNA = TRUE) #downloads date time into UTC
  lwma_weather$date_time = as_datetime(lwma_weather$TIME, tz = "UTC") #set date_time to utc
  lwma_weather$date = as_date(lwma_weather$date_time) #get the date from the date_time
  # return(lwma_weather)

  #calculate sunrise times on unique dates
  lwma_hsunrise = NULL
  for(i in unique(lwma_weather$date)){
    sunrise_time = getSunlightTimes(date = as_date(i), lat = 35.061784, lon = -97.188124, tz = "UTC", keep = c("sunrise"))
    # sunrise_loc = getSunlightPosition(date = lwma_weather$TIME[i], lat = 34.98224, lon = -97.52109, keep = c("altitude"))
    # sunrise_time$altitude = sunrise_loc$altitude
    sunrise_time$date =as_date(i)
    # sunrise_time$lat = 34.98224
    # sunrise_time$lon = -97.52109
    lwma_hsunrise = rbind(sunrise_time,lwma_hsunrise)
  }
  
  lwma_haltitude = NULL
  for(i in 1:length(lwma_weather$date_time)){
    sunrise_loc = getSunlightPosition(date = lwma_weather$date_time[i], lat = 35.061784, lon = -97.188124, keep = c("altitude"))
    # sunrise_alt$altitude = sunrise_loc$altitude
    sunrise_loc$date_time = lwma_weather$date_time[i]
    lwma_haltitude = rbind(sunrise_loc,lwma_haltitude)
  }
  
  lwma_haltitude2 = lwma_haltitude %>%
    select(-date,-lat,-lon)
  
  # labels = seq(0,length(lwma_weather$date_time)-5,5)

  
  lwma_combo = left_join(lwma_weather,lwma_haltitude2, by = c("date_time")) #adding sun altitude variable to historic mesonet dataset
  lwma_combo2 = left_join(lwma_combo,lwma_hsunrise, by = "date") #combining sunrise time to historic mesonet dataset
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  
  lwma_combo3 = lwma_combo2 %>%
    dplyr::filter(is.na(TAIR)==FALSE)%>% #filtering out rows with no temperature readings
    dplyr::filter(is.na(RELH)==FALSE)%>% #filtering out rows with no relative humidity readings
    mutate(min_as = as.numeric(difftime(date_time,sunrise,units = c("mins"))), #create minutes before/after sunrise
           mas = cut(min_as, seq(-725,760,5),labels = labels, right = FALSE),
           site = "lwma", #designate site
           month_day = format(as.Date(date_time), "%m-%d"), #create separate month_day variable, for individual days
           hour_utc = hour(date_time),
           gh_hobs = (25)*(1)*(max_sat(TAIR)-(RELH/100))) #create bins for minutes before/after sunrise, ranging from -725 to 760
  lwma_combo3$ghhobs_scaled = scale(lwma_combo3$gh_hobs)

  # print(paste0("Year ", x," Done"))
  lwma_missing = lwma_combo3 %>% dplyr::filter(is.na(mas)==TRUE) #see if any mas were not calculated properly
  
  # lwma_missing = lwma_combo4 %>% dplyr::filter(is.na(mas)==TRUE)
  
  return(lwma_combo3)
 
  # labels <- seq(0,1435,5)
  # wd$bins <- cut(wd$MAS,seq(0,1440,5), labels = labels, right = FALSE)      
  
});beep(sound = 2)
# lwma_history2 = rbind(lwma_history[[1]],lwma_history[[2]],lwma_history[[3]],lwma_history[[4]],lwma_history[[5]],lwma_history[[6]],lwma_history[[7]],lwma_history[[8]],lwma_history[[9]],lwma_history[[10]],lwma_history[[11]],lwma_history[[12]],lwma_history[[13]],lwma_history[[14]],lwma_history[[15]],lwma_history[[16]],lwma_history[[17]])

# lwma2005 = as.data.frame(lwma_history[[1]])
# lwma2006 = as.data.frame(lwma_history[[2]])

# lwma_missing = lwma2005 %>% dplyr::filter(is.na(gh_hobs)==TRUE) #check to see if theres any missing gh scaled values

# lwma2007 = as.data.frame(lwma_history[[1]])
# lwma2008 = as.data.frame(lwma_history[[2]])

# lwma2009 = as.data.frame(lwma_history[[1]])
# lwma2010 = as.data.frame(lwma_history[[2]])

# lwma2011 = as.data.frame(lwma_history[[1]])
# lwma2012 = as.data.frame(lwma_history[[2]])

# lwma2013 = as.data.frame(lwma_history[[1]])
# lwma2014 = as.data.frame(lwma_history[[2]])

# lwma2015 = as.data.frame(lwma_history[[1]])
# lwma2016 = as.data.frame(lwma_history[[2]])
# lwma_missing = lwma2016 %>% dplyr::filter(is.na(ghwinsite)==TRUE) #check to see if theres any missing gh scaled values
# 
# lwma2017 = as.data.frame(lwma_history[[1]])
# lwma2018 = as.data.frame(lwma_history[[2]])
# 
lwma2019 = as.data.frame(lwma_history[[1]])
lwma2020 = as.data.frame(lwma_history[[2]])
lwma2021 = as.data.frame(lwma_history[[3]])

lwma_missing = lwma2017 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
lwma_missing = lwma2018 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
lwma_missing = lwma2019 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
lwma_missing = lwma2020 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
lwma_missing = lwma2021 %>% dplyr::filter(is.na(gh_hobs)==TRUE)

lwma_history2 = rbind(lwma2005,lwma2006,lwma2007,lwma2008,lwma2009,lwma2010,lwma2012,lwma2013,lwma2014,lwma2015,lwma2016,lwma2017,lwma2018,lwma2019,lwma2020,lwma2021) %>%
  rename(local_time = "TIME",
         temp = "TAIR",
         relh = "RELH") %>%
  mutate(local_time = as_datetime(local_time, tz = "US/Central"))
# lwma_history2$local_time = as_datetime(lwma_history2$TIME, tz = "US/Central")

lwma_history2$temp = na.approx(lwma_history2$temp, na.rm = FALSE)
lwma_history2$relh = na.approx(lwma_history2$relh, na.rm = FALSE)
lwma_history2$PRES = na.approx(lwma_history2$PRES, na.rm = FALSE)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/")
save(lwma_history2, file = "lwma_historic_mesonet.Rdata")
load("lwma_historic_mesonet.Rdata")
# lwma_history2$sunrise = na.approx(lwma_history2$sunrise, na.rm = FALSE)
# lwma_history2$altitude = na.approx(lwma_history2$altitude, na.rm = FALSE)

lwma_history3 = lwma_history2 %>%
  dplyr::filter(is.na(temp)==FALSE)%>%
  dplyr::filter(is.na(relh)==FALSE)%>%
  group_by(month_day,mas)%>%#standardize by minutes after sunrise and day
  summarise(n = n(),
            ghmean_time = mean(gh_hobs),
            ghsd_time = sd(gh_hobs),
            ghse_time = (sd(gh_hobs))/sqrt(n)) #higher gh means more arid!!!
lwma_history3$ghsite_scaled = scale(lwma_history3$ghmean_time)

# lwmah_missing = lwma_history3 %>% dplyr::filter(is.na(temp)==TRUE)
# lwmah_missing = lwma_history3 %>% dplyr::filter(is.na(gh_winscale)==TRUE)
# 
# lwmah_date = lwma_history3 %>%
#   group_by(month_day,hour_utc)%>%
#   summarise_at(vars(gh_winscale),funs(mean,sd,se=sd(.)/sqrt(n())))%>%
#   rename(meanscale_ghsite = "mean", sdscale_ghsite = "sd", sescale_ghsite = "se")

lwma_history4 = inner_join(lwma_history2, lwma_history3, by = c("month_day","mas"))
# lwma_history4$mean_ghtotal = mean(lwma_history4$gh)
# lwma_history4$se_ghtotal = (sd(lwma_history4$gh)/sqrt(354240))
lwma_wh = lwma_history4
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/")
save(lwma_wh, file = "lwma_wh.Rdata")


ggplot(data = lwma_history4%>% dplyr::filter(year(date) == 2021), aes(x = date, y = mean_ghmd))+
  geom_point()+
  geom_smooth(method = "lm")

lwmah_hour = lwma_history4 %>%
  group_by(mas) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            # mean_dew = mean(dew),
            # mean_arid = mean(abs(arid)),
            # mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_ghhobs = mean(ghhobs_scaled),
            mean_ghsite = mean(ghsite_scaled))

ggplot(data = lwmah_hour, aes(x = mas, y = mean_ghhobs))+
  geom_point()+
  geom_smooth(method = "lm")



# SSWMA Historic Weather Data ---------------------------------------------
# year = as.list(c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) #16 years of historic Mesonet weather data
# year = as.list(c(2005,2006)) #have to break up into smaller chunks because Mesonet will lose connection to API
# year = as.list(c(2007,2008))
# year = as.list(c(2009,2010))
# year = as.list(c(2011,2012))
# year = as.list(c(2013,2014))
# year = as.list(c(2015,2016))
# year = as.list(c(2017,2018))
year = as.list(c(2019,2020,2021))
# year = as.list(c(2007,2008,2009,2010,2011)) #comment/uncomment years that you are going to acquire
# year = as.list(c(2012,2013,2014,2015,2016))
# year = as.list(c(2017,2018,2019,2020,2021))

#Obtain data from Mesonet API
sswma_history = lapply(year,function(x){
  beginTime = as.POSIXct(paste0(x,"-05-01 00:00:00", tz = "UTC")) #2021-04-30 19:00:00 CDT
  endTime = as.POSIXct(paste0(x,"-08-31 23:55::00", tz = "UTC")) #2021-08-31 18:55:00 CDT
  sswma_weather <- okmts(begintime=beginTime,
                        endtime=endTime, 
                        variables = c("TAIR", "RELH","PRES"),
                        station="ERIC",  
                        localtime=TRUE, #get local time and convert to UTC using as_datetime
                        missingNA = TRUE) #downloads date time into UTC
  sswma_weather$date_time = as_datetime(sswma_weather$TIME, tz = "UTC") #set date_time to utc
  sswma_weather$date = as_date(sswma_weather$date_time) #get the date from the date_time
  # return(sswma_weather)
  
  #calculate sunrise times on unique dates
  sswma_hsunrise = NULL
  for(i in unique(sswma_weather$date)){
    sunrise_time = getSunlightTimes(date = as_date(i), lat = 35.057294, lon = -99.904372, tz = "UTC", keep = c("sunrise"))
    # sunrise_loc = getSunlightPosition(date = sswma_weather$TIME[i], lat = 34.98224, lon = -97.52109, keep = c("altitude"))
    # sunrise_time$altitude = sunrise_loc$altitude
    sunrise_time$date =as_date(i)
    # sunrise_time$lat = 34.98224
    # sunrise_time$lon = -97.52109
    sswma_hsunrise = rbind(sunrise_time,sswma_hsunrise)
  }
  
  sswma_haltitude = NULL
  for(i in 1:length(sswma_weather$date_time)){
    sunrise_loc = getSunlightPosition(date = sswma_weather$date_time[i], lat = 35.057294, lon = -99.904372, keep = c("altitude"))
    # sunrise_alt$altitude = sunrise_loc$altitude
    sunrise_loc$date_time = sswma_weather$date_time[i]
    sswma_haltitude = rbind(sunrise_loc,sswma_haltitude)
  }
  
  sswma_haltitude2 = sswma_haltitude %>%
    select(-date,-lat,-lon)
  
  # labels = seq(0,length(sswma_weather$date_time)-5,5)
  
  
  sswma_combo = left_join(sswma_weather,sswma_haltitude2, by = c("date_time")) #adding sun altitude variable to historic mesonet dataset
  sswma_combo2 = left_join(sswma_combo,sswma_hsunrise, by = "date") #combining sunrise time to historic mesonet dataset
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  
  sswma_combo3 = sswma_combo2 %>%
    dplyr::filter(is.na(TAIR)==FALSE)%>% #filtering out rows with no temperature readings
    dplyr::filter(is.na(RELH)==FALSE)%>% #filtering out rows with no relative humidity readings
    mutate(min_as = as.numeric(difftime(date_time,sunrise,units = c("mins"))), #create minutes before/after sunrise
           mas = cut(min_as, seq(-725,760,5),labels = labels, right = FALSE),
           site = "sswma", #designate site
           month_day = format(as.Date(date_time), "%m-%d"), #create separate month_day variable, for individual days
           hour_utc = hour(date_time),
           gh_hobs = (25)*(1)*(max_sat(TAIR)-(RELH/100))) #create bins for minutes before/after sunrise, ranging from -725 to 760
  sswma_combo3$ghhobs_scaled = scale(sswma_combo3$gh_hobs)
  
  # print(paste0("Year ", x," Done"))
  sswma_missing = sswma_combo3 %>% dplyr::filter(is.na(mas)==TRUE) #see if any mas were not calculated properly
  
  # sswma_missing = sswma_combo4 %>% dplyr::filter(is.na(mas)==TRUE)
  
  return(sswma_combo3)
  
  # labels <- seq(0,1435,5)
  # wd$bins <- cut(wd$MAS,seq(0,1440,5), labels = labels, right = FALSE)      
  
});beep(sound = 2)
# sswma_history2 = rbind(sswma_history[[1]],sswma_history[[2]],sswma_history[[3]],sswma_history[[4]],sswma_history[[5]],sswma_history[[6]],sswma_history[[7]],sswma_history[[8]],sswma_history[[9]],sswma_history[[10]],sswma_history[[11]],sswma_history[[12]],sswma_history[[13]],sswma_history[[14]],sswma_history[[15]],sswma_history[[16]],sswma_history[[17]])
# 
# sswma2005 = as.data.frame(sswma_history[[1]])
# sswma2006 = as.data.frame(sswma_history[[2]])

# sswma2007 = as.data.frame(sswma_history[[1]])
# sswma2008 = as.data.frame(sswma_history[[2]])

# sswma2009 = as.data.frame(sswma_history[[1]])
# sswma2010 = as.data.frame(sswma_history[[2]])

# sswma2011 = as.data.frame(sswma_history[[1]])
# sswma2012 = as.data.frame(sswma_history[[2]])

# sswma2013 = as.data.frame(sswma_history[[1]])
# sswma2014 = as.data.frame(sswma_history[[2]])

# sswma2015 = as.data.frame(sswma_history[[1]])
# sswma2016 = as.data.frame(sswma_history[[2]])
# sswma_missing = sswma2016 %>% dplyr::filter(is.na(ghwinsite)==TRUE) #check to see if theres any missing gh scaled values
# 
# sswma2017 = as.data.frame(sswma_history[[1]])
# sswma2018 = as.data.frame(sswma_history[[2]])
# 
sswma2019 = as.data.frame(sswma_history[[1]])
sswma2020 = as.data.frame(sswma_history[[2]])
sswma2021 = as.data.frame(sswma_history[[3]])

sswma_missing = sswma2017 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
sswma_missing = sswma2018 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
sswma_missing = sswma2019 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
sswma_missing = sswma2020 %>% dplyr::filter(is.na(gh_hobs)==TRUE)
sswma_missing = sswma2021 %>% dplyr::filter(is.na(gh_hobs)==TRUE)

sswma_history2 = rbind(sswma2005,sswma2006,sswma2007,sswma2008,sswma2009,sswma2010,sswma2012,sswma2013,sswma2014,sswma2015,sswma2016,sswma2017,sswma2018,sswma2019,sswma2020,sswma2021) %>%
  rename(local_time = "TIME",
         temp = "TAIR",
         relh = "RELH") %>%
  mutate(local_time = as_datetime(local_time, tz = "US/Central"))
# sswma_history2$local_time = as_datetime(sswma_history2$TIME, tz = "US/Central")

sswma_history2$temp = na.approx(sswma_history2$temp, na.rm = FALSE)
sswma_history2$relh = na.approx(sswma_history2$relh, na.rm = FALSE)
sswma_history2$PRES = na.approx(sswma_history2$PRES, na.rm = FALSE)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/")
save(sswma_history2, file = "sswma_historic_mesonet.Rdata")
load("sswma_historic_mesonet.Rdata")
# sswma_history2$sunrise = na.approx(sswma_history2$sunrise, na.rm = FALSE)
# sswma_history2$altitude = na.approx(sswma_history2$altitude, na.rm = FALSE)

sswma_history3 = sswma_history2 %>%
  dplyr::filter(is.na(temp)==FALSE)%>%
  dplyr::filter(is.na(relh)==FALSE)%>%
  group_by(month_day,mas)%>%#standardize by minutes after sunrise and day
  summarise(n = n(),
            ghmean_time = mean(gh_hobs),
            ghsd_time = sd(gh_hobs),
            ghse_time = (sd(gh_hobs))/sqrt(n)) #higher gh means more arid!!!
sswma_history3$ghsite_scaled = scale(sswma_history3$ghmean_time)

# sswmah_missing = sswma_history3 %>% dplyr::filter(is.na(temp)==TRUE)
# sswmah_missing = sswma_history3 %>% dplyr::filter(is.na(gh_winscale)==TRUE)
# 
# sswmah_date = sswma_history3 %>%
#   group_by(month_day,hour_utc)%>%
#   summarise_at(vars(gh_winscale),funs(mean,sd,se=sd(.)/sqrt(n())))%>%
#   rename(meanscale_ghsite = "mean", sdscale_ghsite = "sd", sescale_ghsite = "se")

sswma_history4 = inner_join(sswma_history2, sswma_history3, by = c("month_day","mas"))
# sswma_history4$mean_ghtotal = mean(sswma_history4$gh)
# sswma_history4$se_ghtotal = (sd(sswma_history4$gh)/sqrt(354240))
sswma_wh = sswma_history4
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/")
save(sswma_wh, file = "sswma_wh.Rdata")


ggplot(data = sswma_history4%>% dplyr::filter(year(date) == 2021), aes(x = date, y = mean_ghmd))+
  geom_point()+
  geom_smooth(method = "lm")

sswmah_hour = sswma_history4 %>%
  group_by(mas) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            # mean_dew = mean(dew),
            # mean_arid = mean(abs(arid)),
            # mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_ghhobs = mean(ghhobs_scaled),
            mean_ghsite = mean(ghsite_scaled))

ggplot(data = sswmah_hour, aes(x = mas, y = mean_ghhobs))+
  geom_point()+
  geom_smooth(method = "lm")

# CBMA Historic Weather Data ----------------------------------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/")
cbma_files = list.files(pattern = ".csv")
cbma_history = lapply(cbma_files,function(x){
  cbma_weather = read_csv(x, col_names = TRUE, col_types = "ccnnnTnnnnnnnnnnnnnc") %>%
    dplyr::rename(station = "Station_ID",
                  name = "Name",
                  lat = "Latitude",
                  lon = "Longitude",
                  elev_m = "Elevation (m)",
                  date_time = "Date_Time (UTC)",
                  dew = "Dew Point (c)",
                  temp = "Temperature (c)",
                  relh = "Relative Humidity (%)",
                  pres = "Sea_level pressure (pa)")%>%
    mutate(date = as_date(date_time))
  # return(cbma_weather)
  # %>%
  #   dplyr::filter(date(date_time) > "2021-04-30")
  
  #calculate sunrise times on unique dates
  cbma_hsunrise = NULL
  for(i in unique(cbma_weather$date)){
    sunrise_time = getSunlightTimes(date = as_date(i), lat = 35.41396, lon = -101.95244, tz = "UTC", keep = c("sunrise"))
    # sunrise_loc = getSunlightPosition(date = cbma_weather$TIME[i], lat = 34.98224, lon = -97.52109, keep = c("altitude"))
    # sunrise_time$altitude = sunrise_loc$altitude
    sunrise_time$date =as_date(i)
    # sunrise_time$lat = 34.98224
    # sunrise_time$lon = -97.52109
    cbma_hsunrise = rbind(sunrise_time,cbma_hsunrise)
  }
  
  cbma_haltitude = NULL
  for(i in 1:length(cbma_weather$date_time)){
    sunrise_loc = getSunlightPosition(date = cbma_weather$date_time[i], lat = 35.41396, lon = -101.95244, keep = c("altitude"))
    # sunrise_alt$altitude = sunrise_loc$altitude
    sunrise_loc$date_time = cbma_weather$date_time[i]
    cbma_haltitude = rbind(sunrise_loc,cbma_haltitude)
  }
  
  cbma_haltitude2 = cbma_haltitude %>%
    select(-date,-lat,-lon)
  
  # labels = seq(0,length(cbma_weather$date_time)-5,5)
  
  
  cbma_combo = left_join(cbma_weather,cbma_haltitude2, by = c("date_time")) #adding sun altitude variable to historic mesonet dataset
  cbma_combo2 = left_join(cbma_combo,cbma_hsunrise, by = "date") #combining sunrise time to historic mesonet dataset
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  
  cbma_combo3 = cbma_combo2 %>%
    dplyr::filter(is.na(temp)==FALSE)%>% #filtering out rows with no temperature readings
    dplyr::filter(is.na(relh)==FALSE)%>% #filtering out rows with no relative humidity readings
    mutate(min_as = as.numeric(difftime(date_time,sunrise,units = c("mins"))), #create minutes before/after sunrise
           mas = cut(min_as, seq(-725,760,5),labels = labels, right = FALSE),
           site = "cbma", #designate site
           month_day = format(as.Date(date_time), "%m-%d"), #create separate month_day variable, for individual days
           hour_utc = hour(date_time),
           gh_hobs = (25)*(1)*(max_sat(temp)-(relh/100))) #create bins for minutes before/after sunrise, ranging from -725 to 760
  cbma_combo3$ghhobs_scaled = scale(cbma_combo3$gh_hobs)
  
  # print(paste0("Year ", x," Done"))
  
  # cbma_missing = cbma_combo4 %>% dplyr::filter(is.na(mas)==TRUE)
  
  return(cbma_combo3)
  

})
cbma_history2 = rbind(cbma_history[[1]],cbma_history[[2]],cbma_history[[3]],cbma_history[[4]],cbma_history[[5]],cbma_history[[6]],cbma_history[[7]],cbma_history[[8]],cbma_history[[9]],cbma_history[[10]],cbma_history[[11]],cbma_history[[12]],cbma_history[[13]],cbma_history[[14]],cbma_history[[15]],cbma_history[[16]],cbma_history[[17]])

cbma_history2 = cbma_history2 %>% arrange(date_time)
cbma_history2$temp = na.approx(cbma_history2$temp, na.rm = FALSE)
cbma_history2$relh = na.approx(cbma_history2$relh, na.rm = FALSE)
cbma_history2$pres = na.approx(cbma_history2$pres, na.rm = FALSE)

cbma_missing = cbma_history2 %>% dplyr::filter(is.na(temp)==TRUE)

cbma_history3 = cbma_history2 %>%
  dplyr::filter(is.na(temp)==FALSE)%>%
  dplyr::filter(is.na(relh)==FALSE)%>%
  group_by(month_day,mas)%>%#standardize by minutes after sunrise and day
  summarise(n = n(),
            ghmean_time = mean(gh_hobs),
            ghsd_time = sd(gh_hobs),
            ghse_time = (sd(gh_hobs))/sqrt(n)) #higher gh means more arid!!!
cbma_history3$ghsite_scaled = scale(cbma_history3$ghmean_time)

# cbmah_missing = cbma_history3 %>% dplyr::filter(is.na(temp)==TRUE)
# cbmah_missing = cbma_history3 %>% dplyr::filter(is.na(gh_winscale)==TRUE)
# 
# cbmah_date = cbma_history3 %>%
#   group_by(month_day,hour_utc)%>%
#   summarise_at(vars(gh_winscale),funs(mean,sd,se=sd(.)/sqrt(n())))%>%
#   rename(meanscale_ghsite = "mean", sdscale_ghsite = "sd", sescale_ghsite = "se")

cbma_history4 = inner_join(cbma_history2, cbma_history3, by = c("month_day","mas"))
# cbma_history4$mean_ghtotal = mean(cbma_history4$gh)
# cbma_history4$se_ghtotal = (sd(cbma_history4$gh)/sqrt(354240))
cbma_wh = cbma_history4
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/")
save(cbma_wh, file = "cbma_wh.Rdata")

cbmah_hour = cbma_wh %>%
  group_by(mas) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            # mean_dew = mean(dew),
            # mean_arid = mean(abs(arid)),
            # mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_ghhobs = mean(ghhobs_scaled),
            mean_ghsite = mean(ghsite_scaled))

ggplot(data = cbmah_hour, aes(x = mas, y = mean_ghhobs))+
  geom_point()+
  geom_smooth(method = "lm")


# KIOWA Historic Data -----------------------------------------------------
# year = as.list(c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/kiowa_historic_data/")
kiowa_files = list.files(pattern = ".csv")
kiowa_history = lapply(kiowa_files,function(x){
  kiowa_weather = read_csv(x, col_names = TRUE, col_types = "ccnnnTnnnnnnnnnnc") %>%
    dplyr::rename(station = "Station_ID",
                  lat = "Latitude",
                  lon = "Longitude",
                  elev_m = "Elevation (m)",
                  date_time = "Date_Time (UTC)",
                  temp = "Temperature (c)",
                  relh = "Relative Humidity (%)")%>%
    mutate(date = as_date(date_time))
  # return(cbma_weather)
  # %>%
  #   dplyr::filter(date(date_time) > "2021-04-30")
  
  #calculate sunrise times on unique dates
  kiowa_hsunrise = NULL
  for(i in unique(kiowa_weather$date)){
    sunrise_time = getSunlightTimes(date = as_date(i), lat = 36.059589, lon = -104.339575, tz = "UTC", keep = c("sunrise"))
    # sunrise_loc = getSunlightPosition(date = kiowa_weather$TIME[i], lat = 34.98224, lon = -97.52109, keep = c("altitude"))
    # sunrise_time$altitude = sunrise_loc$altitude
    sunrise_time$date =as_date(i)
    # sunrise_time$lat = 34.98224
    # sunrise_time$lon = -97.52109
    kiowa_hsunrise = rbind(sunrise_time,kiowa_hsunrise)
  }
  
  kiowa_haltitude = NULL
  for(i in 1:length(kiowa_weather$date_time)){
    sunrise_loc = getSunlightPosition(date = kiowa_weather$date_time[i], lat = 36.059589, lon = -104.339575, keep = c("altitude"))
    # sunrise_alt$altitude = sunrise_loc$altitude
    sunrise_loc$date_time = kiowa_weather$date_time[i]
    kiowa_haltitude = rbind(sunrise_loc,kiowa_haltitude)
  }
  
  kiowa_haltitude2 = kiowa_haltitude %>%
    select(-date,-lat,-lon)
  
  # labels = seq(0,length(kiowa_weather$date_time)-5,5)
  
  
  kiowa_combo = left_join(kiowa_weather,kiowa_haltitude2, by = c("date_time")) #adding sun altitude variable to historic mesonet dataset
  kiowa_combo2 = left_join(kiowa_combo,kiowa_hsunrise, by = "date") #combining sunrise time to historic mesonet dataset
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  
  kiowa_combo3 = kiowa_combo2 %>%
    dplyr::filter(is.na(temp)==FALSE)%>% #filtering out rows with no temperature readings
    dplyr::filter(is.na(relh)==FALSE)%>% #filtering out rows with no relative humidity readings
    mutate(min_as = as.numeric(difftime(date_time,sunrise,units = c("mins"))), #create minutes before/after sunrise
           mas = cut(min_as, seq(-725,760,5),labels = labels, right = FALSE),
           site = "kiowa", #designate site
           month_day = format(as.Date(date_time), "%m-%d"), #create separate month_day variable, for individual days
           hour_utc = hour(date_time),
           gh_hobs = (25)*(1)*(max_sat(temp)-(relh/100))) #create bins for minutes before/after sunrise, ranging from -725 to 760
  kiowa_combo3$ghhobs_scaled = scale(kiowa_combo3$gh_hobs)
  
  # print(paste0("Year ", x," Done"))
  
  # kiowa_missing = kiowa_combo4 %>% dplyr::filter(is.na(mas)==TRUE)
  
  return(kiowa_combo3)
  
  
})
kiowa_history2 = rbind(kiowa_history[[1]],kiowa_history[[2]],kiowa_history[[3]],kiowa_history[[4]],kiowa_history[[5]],kiowa_history[[6]],kiowa_history[[7]],kiowa_history[[8]],kiowa_history[[9]],kiowa_history[[10]],kiowa_history[[11]],kiowa_history[[12]],kiowa_history[[13]],kiowa_history[[14]],kiowa_history[[15]],kiowa_history[[16]],kiowa_history[[17]])

kiowa_history2 = kiowa_history2 %>% arrange(date_time)
kiowa_history2$temp = na.approx(kiowa_history2$temp, na.rm = FALSE)
kiowa_history2$relh = na.approx(kiowa_history2$relh, na.rm = FALSE)
kiowa_history2$pres = na.approx(kiowa_history2$pres, na.rm = FALSE)

kiowa_missing = kiowa_history2 %>% dplyr::filter(is.na(temp)==TRUE)

kiowa_history3 = kiowa_history2 %>%
  dplyr::filter(is.na(temp)==FALSE)%>%
  dplyr::filter(is.na(relh)==FALSE)%>%
  group_by(month_day,mas)%>%#standardize by minutes after sunrise and day
  summarise(n = n(),
            ghmean_time = mean(gh_hobs),
            ghsd_time = sd(gh_hobs),
            ghse_time = (sd(gh_hobs))/sqrt(n)) #higher gh means more arid!!!
kiowa_history3$ghsite_scaled = scale(kiowa_history3$ghmean_time)

# kiowah_missing = kiowa_history3 %>% dplyr::filter(is.na(temp)==TRUE)
# kiowah_missing = kiowa_history3 %>% dplyr::filter(is.na(gh_winscale)==TRUE)
# 
# kiowah_date = kiowa_history3 %>%
#   group_by(month_day,hour_utc)%>%
#   summarise_at(vars(gh_winscale),funs(mean,sd,se=sd(.)/sqrt(n())))%>%
#   rename(meanscale_ghsite = "mean", sdscale_ghsite = "sd", sescale_ghsite = "se")

kiowa_history4 = inner_join(kiowa_history2, kiowa_history3, by = c("month_day","mas"))
# kiowa_history4$mean_ghtotal = mean(kiowa_history4$gh)
# kiowa_history4$se_ghtotal = (sd(kiowa_history4$gh)/sqrt(354240))
kiowa_wh = kiowa_history4
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/")
save(kiowa_wh, file = "kiowa_wh.Rdata")

kiowah_hour = kiowa_wh %>%
  group_by(mas) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            # mean_dew = mean(dew),
            # mean_arid = mean(abs(arid)),
            # mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_ghhobs = mean(ghhobs_scaled),
            mean_ghsite = mean(ghsite_scaled))

ggplot(data = kiowah_hour, aes(x = mas, y = mean_ghhobs))+
  geom_point()+
  geom_smooth(method = "lm")


