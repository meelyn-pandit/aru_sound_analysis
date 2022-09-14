library(dplyr) #data manipulation
library(tibble) #data manipulation
library(lubridate) #manipulating date and time
library(hms) #manipulate time
library(zoo) #for na.approx to approximate missing values in weather dataset
library(ggplot2) #graphs
library(lme4) #lmm and glmm analysis
library(lmerTest) #get p-values for lmm tests
library(reshape2) #???
library(suncalc)
library(zoo)
library(car)
library(multcomp) #posthoc tests for ANOVA type III effects

# Calculating Number of Species/Hour --------------------------------------
sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("sswma"))
water_full = NULL
for(s in sites){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
  
  if(s == "sswma"){
    load("sswma_aru_results.Rdata")
    data = sswma_aru_results
    load("weather_data/sswma_ncep_weather.Rdata")
    weather_data = sswma_data
  } else if(s == "cbma"){
    load("cbma_aru_results.Rdata")
    data = cbma_aru_results
    load("weather_data/cbma_ncep_weather.Rdata")
    weather_data = cbma_data
  } 
  
  if(s == "kiowa"){
    tz = "US/Mountain"
    #i want to only subtract 3600s from 06/17/21 and 06/18/21
  } else {
    tz = "US/Central"
  }
  
  data_temp = data %>%
    mutate(date = force_tz(date, tz = tz),
           date_time = force_tz(date_time, tz = tz),
           time = as_hms(date_time),
           hour = hour(date_time)) %>%
    # filter(date > "2021-04-30 CDT")%>%
    dplyr::filter(is.na(common_name) == FALSE) %>% 
    group_by(common_name,date,time,date_time,hour,lat,lon,aru,site) %>%
    summarise(num_vocals = n(),
              species_diversity = n_distinct(common_name))%>%
    arrange(date_time) 
  if(s == "kiowa"){
    kiowa_time_bad = data_temp %>% dplyr::filter(hour ==12)%>%
      mutate(date_time = date_time-3600,
             time = as_hms(date_time),
             hour = hour(date_time))
    kiowa_time_good = data_temp %>% dplyr::filter(hour !=12)
    data_temp = rbind(kiowa_time_bad,kiowa_time_good)
    
  } else {
    
  }
  
  sunrise_final = NULL
  for(i in 1:length(data_temp$date_time)){
    sunrise_time = getSunlightTimes(date = as_date(data_temp$date[i]), lat = data_temp$lat[i], lon = data_temp$lon[i], tz = tz, keep = c("sunrise"))
    sunrise_loc = getSunlightPosition(date = as_date(data_temp$date[i]), lat = data_temp$lat[i], lon = data_temp$lon[i], keep = c("altitude"))
    sunrise_time$altitude = sunrise_loc$altitude
    sunrise_final = rbind(sunrise_time,sunrise_final)
  }
  
  sunrise_final = sunrise_final %>%
    arrange(date)
  
  data_temp$sunrise = sunrise_final$sunrise
  data_temp$sun_alt = sunrise_final$altitude
  
  weather_data = weather_data %>%
    mutate(date_time = local_time) %>%
    arrange(date_time)
  data_temp = left_join(data_temp, weather_data, by = "date_time")
  
  data_temp = data_temp %>% 
    arrange(date,aru)
  data_temp$temperature = na.approx(data_temp$temperature, na.rm = FALSE)
  data_temp$humidity = na.approx(data_temp$humidity, na.rm = FALSE)
  data_temp$pressure = na.approx(data_temp$pressure, na.rm = FALSE)
  data_temp$windspeed = na.approx(data_temp$windspeed, na.rm = FALSE)
  data_temp$winddir = na.approx(data_temp$winddir, na.rm = FALSE)
  data_temp$emissivity = na.approx(data_temp$emissivity, na.rm = FALSE)
  data_temp$cloudcover = na.approx(data_temp$cloudcover, na.rm = FALSE)
  data_temp$netlong = na.approx(data_temp$netlong, na.rm = FALSE)
  data_temp$uplong = na.approx(data_temp$uplong, na.rm = FALSE)
  data_temp$downlong = na.approx(data_temp$downlong, na.rm = FALSE)
  data_temp$rad_dni = na.approx(data_temp$rad_dni, na.rm = FALSE)
  data_temp$rad_dif = na.approx(data_temp$rad_dif, na.rm = FALSE)
  data_temp$szenith = na.approx(data_temp$szenith, na.rm = FALSE)
  data_temp$relh = na.approx(data_temp$relh, na.rm = FALSE)
  
  data_temp2 = data_temp %>%
    filter(is.na(temperature) == FALSE) %>% 
    mutate(mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))),
           # bin1 = seq(from = min(mas), to = max(mas), by=5),
           dew = temperature-((100-relh)/5),
           arid = -dew,
           site = factor(site, levels=c("sswma","cbma")))%>%
    filter(aru != "aru01" | aru!="aru02"| aru != "aru03"| aru != "aru04"| aru != "aru05")
  # filter(aru == "aru01" | aru == "aru02"| aru == "aru03"| aru == "aru04"| aru == "aru05")
  
  water_full = rbind(data_temp2,water_full) %>%
    arrange(site,aru,hour)
  
}

#Data for statistical analyses
water_full2 = water_full %>% #saving it a different name so you don't overwrite it
  dplyr::filter(is.na(mas)==FALSE) 
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(water_full2, file = "water_supp.Rdata")

#Data is averaged by hour across aru and breeding season
water_hour = water_full2 %>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  arrange(site,aru,hour) %>%
  group_by(site,lat,lon,hour) %>%
  summarise(mean_sunalt = mean(sun_alt),
            mean_num_vocals = mean(num_vocals),
            mean_species = mean(species_diversity),
            mean_dew = mean(dew),
            mean_arid = mean(arid))%>%
  arrange(hour)
save(water_hour, file = "water_hour.Rdata")

water_month = water_full2 %>%
  arrange(site,aru,hour) %>%
  mutate(month = month(date_time))%>%
  group_by(site,lat,lon,month) %>%
  summarise(mean_sunalt = mean(sun_alt),
            mean_num_vocals = mean(num_vocals),
            mean_species = mean(species_diversity),
            mean_dew = mean(dew),
            mean_arid = mean(arid))%>%
  arrange(hour)
save(water_hour, file = "water_month.Rdata")




