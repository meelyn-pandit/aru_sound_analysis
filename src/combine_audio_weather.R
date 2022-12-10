
library(rmarkdown)
library(soundecology) #obtain broad scale acoustic metrics
library(tuneR) #loading and reading sound files
library(seewave) #soundwave analysis
library(osfr) #downloading files from osf
library(dplyr) #data management and conversion
library(tidyverse)
library(lubridate) #convert date types
library(lme4) #linear mixed models
library(lmerTest) #statistical tests for linear mixed models
library(ggplot2) #create good graphs
library(extrafont) #change fonts for ggplot2
library(lsmeans) #post-hoc tests for mixed models
library(zoo) #approximate missing values


# Aridity Gradient Data ---------------------------------------------------
## Aridity Gradient- Load broad acoustic data ====

# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data")
sites = as.list(c("lwma","sswma","cbma","kiowa"))
aru = as.list(c("aru01","aru02","aru03","aru04","aru05"))

# read.csv(paste0("broad_acoustic_data/lwma_arus_raw/aru01_aci_results.csv"), header = TRUE)
aci = NULL
for(s in sites){
  print(paste0("broad_acoustic_data/",s,"_arus_raw/aci_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/aci_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  aci = rbind(metric_temp, aci)
}

aci2 = aci %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), 
                                               format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(aci = "left_channel")

aci2$local_time = ifelse(aci2$site == "kiowa",force_tz(aci2$local_time, tz = "US/Mountain"),force_tz(aci2$local_time, tz = "US/Central"))
aci2$date_time = as_datetime(aci2$local_time, tz = "UTC")

## ADI
adi = NULL
for(s in sites){
  print(paste0("broad_acoustic_data/",s,"_arus_raw/adi_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/adi_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  adi = rbind(metric_temp, adi)
}

adi2 = adi %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(adi = "left_channel")

adi2$local_time = ifelse(adi2$site == "kiowa",force_tz(adi2$local_time, tz = "US/Mountain"),force_tz(adi2$local_time, tz = "US/Central"))
adi2$date_time = as_datetime(adi2$local_time, tz = "UTC")

## AEI
aei = NULL
for(s in sites){
  print(paste0("broad_acoustic_data/",s,"_arus_raw/aei_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/aei_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  aei = rbind(metric_temp, aei)
}

aei2 = aei %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(aei = "left_channel")

aei2$local_time = ifelse(aei2$site == "kiowa",force_tz(aei2$local_time, tz = "US/Mountain"),force_tz(aei2$local_time, tz = "US/Central"))
aei2$date_time = as_datetime(aei2$local_time, tz = "UTC")

## BIO
bio = NULL
for(s in sites){
  print(paste0("broad_acoustic_data/",s,"_arus_raw/bio_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/bio_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  bio = rbind(metric_temp, bio)
}
bio2 = bio %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(bio = "left_channel")

bio2$local_time = ifelse(bio2$site == "kiowa",force_tz(bio2$local_time, tz = "US/Mountain"),force_tz(bio2$local_time, tz = "US/Central"))
bio2$date_time = as_datetime(bio2$local_time, tz = "UTC")


aco = full_join(aci2,bio2, by = c("filename", "site","aru")) %>%
  dplyr::select(filename,samplingrate.x,bit.x,fft_w.x,site,aru,date_time.x,local_time.x,aci,bio) %>%
  dplyr::rename(samplingrate = "samplingrate.x", bit = "bit.x", fft_w = "fft_w.x",date_time = "date_time.x",local_time = "local_time.x")

## Filter ADI dataset to only include - and -70 db thresholds
adi3 = adi2 %>%
  dplyr::filter(db_threshold == "-" | db_threshold == -70)

aco2 = full_join(aco, adi3, by = c("filename", "site", "aru", "samplingrate", "bit", "date_time", "local_time")) %>%
  dplyr::select(filename, samplingrate, bit, fft_w, db_threshold,site, aru, date_time, local_time, aci, bio, adi)

## ## Filter AEI dataset to only include - and -70 db thresholds
aei3 = aei2 %>%
  dplyr::filter(db_threshold == "-" | db_threshold == -70)

aco3 = full_join(aco2, aei3, by = c("filename", "site", "aru", "samplingrate", "bit","db_threshold", "date_time", "local_time")) %>%
  dplyr::select(filename, samplingrate, bit, fft_w, db_threshold,site, aru, date_time, local_time, aci, bio, adi, aei)

acoustic = aco3

# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
save(acoustic, file = "acoustic_metrics.Rdata")


## Aridity Gradient - Combine the broad acoustic metrics with BirdNET data (with NA --------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")

sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("lwma"))
arid_full = NULL
water_full = NULL
for(s in sites){
  # setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  # setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  if(s == "lwma"){
    # load lwma bird data
    load("birdnet_data/lwma_aru_results.Rdata")
    data = lwma_aru_results
    tz = "US/Central"
    
  } else if(s == "sswma"){
    # load sswma bird data
    load("birdnet_data/sswma_aru_results.Rdata")
    data = sswma_aru_results
    tz = "US/Central"
    
    
  } else if(s == "cbma"){
    # load cbma bird data
    load("birdnet_data/cbma_aru_results.Rdata")
    data = cbma_aru_results
    tz = "US/Central"
    
    
  } else if(s == "kiowa"){
    # load kiowa bird data
    load("birdnet_data/kiowa_aru_results.Rdata")
    data = kiowa_aru_results
    tz = "US/Mountain"
    
  }
  
  # summarize birdnet data into number of vocalizations and number of species
  data_temp = data %>%
    mutate(date = as_date(date),
           local_time = force_tz(date_time, tz = tz),
           date_time = as_datetime(local_time, tz = "UTC"),
           time = hms::as_hms(date_time),
           hour_local = hour(local_time),
           hour_utc = hour(date_time)
    ) %>%
    # filter(date > "2021-04-30 CDT")%>%
    # dplyr::filter(is.na(common_name) == FALSE) %>% 
    group_by(date,time,local_time,date_time,hour_local,hour_utc,lat,lon,aru,site) %>%
    dplyr::summarise(num_vocals = n(),
                     species_diversity = n_distinct(common_name))%>%
    arrange(date_time) %>%
    mutate(num_vocals = replace(num_vocals, is.na(lat)==TRUE && num_vocals == 1, 0),
           species_diversity = replace(species_diversity, is.na(lat)==TRUE && species_diversity == 1, 0))
  
  if(s == "kiowa"){
    kiowa_time_bad = data_temp %>% 
      dplyr::filter(date == "2021-06-17" | date == "2021-06-18")%>%
      dplyr::filter(aru == "aru03") 
    kiowa_time_good = setdiff(data_temp, kiowa_time_bad)
    kiowa_time_bad = kiowa_time_bad %>%
      mutate(date_time = date_time-3600,
             local_time = local_time-3600,
             time = hms::as_hms(date_time),
             hour_local = hour(local_time),
             hour_utc = hour(date_time))
    data_temp = rbind(kiowa_time_good,kiowa_time_bad)
    
  } else {
    
  }
  
  data_temp_arid = data_temp %>%
    filter(aru == "aru01" | aru == "aru02"| aru == "aru03"| aru == "aru04"| aru == "aru05")
  
  data_temp_water = data_temp %>%
    filter(aru == "ws01" | aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05" |
             aru == "ws06" | aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10" | 
             aru == "ws11" | aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15" |
             aru == "wg01" | aru == "wg02"| aru == "wg03"| aru == "wg04"| aru == "wg05")
  
  arid_full = rbind(data_temp_arid,arid_full) %>%
    arrange(site,aru,hour_utc)
  
  water_full = rbind(data_temp_water,water_full) %>%
    arrange(site,aru,hour_utc)
  
}

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data/birdnet_data")
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data/birdnet_data")
save(arid_full, file = "aridity_gradient_ml_raw.Rdata")
save(water_full, file = "water_supp_ml_raw.Rdata")
# load("data/birdnet_data/aridity_gradient_ml_raw.Rdata")

# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
#Load broad acoustic metrics
load("acoustic_metrics.Rdata")

acoustic2 = full_join(acoustic, arid_full, by = c("site", "aru", "date_time")) %>%
  dplyr::rename(local_time = "local_time.y") %>%
  dplyr::select(-local_time.x) %>%
  dplyr::select(filename, samplingrate, bit, fft_w, db_threshold, site, aru, lat, lon, local_time, date_time, aci, bio, adi, aei, num_vocals, species_diversity)

# acoustic2$month_day = format(as.Date(acoustic2$date_time), "%m-%d")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
save(acoustic2, file = "acoustic_and_birdnet_data.Rdata")


## Aridity Gradient - Combine 2021 Mesonet and Historic Weather Data -----------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
# acoustic2 = import("acoustic_and_birdnet_data.Rdata")

sites = as.list(c("lwma","sswma","cbma","kiowa"))
wfull = NULL
for(s in sites){
  setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  if(s == "lwma"){
    
    #load 2021 mesonet data
    load("mesonet_data/lwma_mesonet.Rdata")
    wd = lwma_mesonet %>%
      mutate(arid_within = scale(gh),
             month_day = format(as.Date(date_time), "%m-%d"))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    #load historic data from 2005-2021
    load("historic_weather_data/lwma_wh.Rdata")
    hd = lwma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghsite_scaled) %>%
      dplyr::rename(gh_hist = "gh_hobs", hist_within = "ghhobs_scaled", hist_across = "ghsite_scaled") %>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
  } else if(s == "sswma"){
    load("mesonet_data/sswma_mesonet.Rdata")
    wd = sswma_mesonet%>%
      mutate(arid_within = scale(gh),
             month_day = format(as.Date(date_time), "%m-%d"))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    load("historic_weather_data/sswma_wh.Rdata")
    hd = sswma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghsite_scaled) %>%
      dplyr::rename(gh_hist = "gh_hobs", hist_within = "ghhobs_scaled", hist_across = "ghsite_scaled") %>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
  } else if(s == "cbma"){
    load("mesonet_data/cbma_mesonet.Rdata")
    wd = cbma_mesonet%>%
      mutate(arid_within = scale(gh),
             month_day = format(as.Date(date_time), "%m-%d"))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    load("historic_weather_data/cbma_wh.Rdata")
    hd = cbma_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghsite_scaled) %>%
      dplyr::rename(gh_hist = "gh_hobs", hist_within = "ghhobs_scaled", hist_across = "ghsite_scaled") %>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(s == "kiowa"){
    # load kiowa bird data
    load("mesonet_data/kiowa_mesonet.Rdata")
    wd = kiowa_mesonet%>%
      mutate(arid_within = scale(gh),
             month_day = format(as.Date(date_time), "%m-%d"))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    load("historic_weather_data/kiowa_wh.Rdata")
    hd = kiowa_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghsite_scaled) %>%
      dplyr::rename(gh_hist = "gh_hobs", hist_within = "ghhobs_scaled", hist_across = "ghsite_scaled") %>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Mountain"
    
  }
  
  weather_temp = left_join(wd, hd, by = c("month_day","mas"))
  wfull = rbind(weather_temp, wfull) %>%
    arrange(month_day, mas)
}

wfull$gh_hist = na.approx(wfull$gh_hist, na.rm = FALSE) 
wfull$hist_within = na.approx(wfull$hist_within, na.rm = FALSE) 
wfull$hist_across = na.approx(wfull$hist_across, na.rm = FALSE) 

wfull$arid_across = scale(wfull$gh)

wfull = wfull %>%
  dplyr::select(-arid)

save(wfull, file = "mesonet_historic_weather.Rdata")


## Aridity Gradient - Combine Acoustic and Weather Data --------------------
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
load("data_clean/acoustic_and_birdnet_data.Rdata")
load("data_clean/mesonet_historic_weather.Rdata")

aw = full_join(acoustic2, wfull, by = c("site","date_time")) %>%
  arrange(site,aru,date_time)

aw$gh_hist = na.approx(aw$gh_hist, na.rm = FALSE) 
aw$arid_within = na.approx(aw$arid_within, na.rm = FALSE)
aw$arid_across = na.approx(aw$arid_across, na.rm = FALSE)
aw$hist_within = na.approx(aw$hist_within, na.rm = FALSE) 
aw$hist_across = na.approx(aw$hist_across, na.rm = FALSE) 

aw2 = aw %>%
  dplyr::filter(is.na(num_vocals) == FALSE)

aw2$mas_num = as.numeric(as.character(aw2$mas))

# observed aridity scaled within sites
aw2 = aw2 %>% 
  arrange(arid_within) %>%
  mutate(arid_within = cut(arid_within,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

# observed aridity scaled across sites
aw2 = aw2 %>% 
  arrange(arid_across) %>%
  mutate(arid_across = cut(arid_across,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

# historic aridity scaled within sites
aw2 = aw2 %>% 
  arrange(hist_within) %>%
  mutate(hist_within = cut(hist_within,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

#%>% #historic aridity scaled across sites
aw2 = aw2 %>% 
  arrange(hist_across) %>%
  mutate(hist_across = cut(hist_across,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

# cutting minutes after sunrise (mas) into bins based on breaks
aw2$mas_bin = cut(aw2$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

aw2$site = factor(aw2$site, levels = c("lwma","sswma","cbma","kiowa"))

setwd("data_clean")
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
save(aw2, file = "audio_and_weather_data.Rdata")








# Water Supplementation Data ----------------------------------------------

## Water Supplementation - Load broad acoustic Data ----------------------------------------
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data")

ws_sites = as.list(c("sswma","cbma"))
aci = NULL
for(s in ws_sites){
  print(paste0("broad_acoustic_data/",s,"_wlg_raw/aci_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/aci_wlg_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  aci = rbind(metric_temp, aci)
}

aci2 = aci %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), 
                                               format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(aci = "left_channel")

aci2$local_time = ifelse(aci2$site == "kiowa",force_tz(aci2$local_time, tz = "US/Mountain"),force_tz(aci2$local_time, tz = "US/Central"))
aci2$date_time = as_datetime(aci2$local_time, tz = "UTC")
aci2$aru = substr(aci2$aru,1,4)

## ADI - Compile all SSWMA files
# ws_sites = "sswma"
# aru = as.list(c("ws01","ws02","ws03","ws04","ws05","ws06","ws07","ws08","ws09","ws10","ws11","ws12","ws13","ws14","ws15"))
# adi_sswma = NULL
# for(s in ws_sites){
#   for(a in aru){
#     # print(paste0("broad_acoustic_data/",s,"_wlg_raw/adi_",s,"_full.csv"))
#     # metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/adi_wlg_",s,"_full.csv"), header = TRUE)
#     print(paste0("broad_acoustic_data/",s,"_wlg_raw/",a,"_adi_results.csv"))
#     metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/",a,"_adi_results.csv"))
#     names(metric_temp) = tolower(names(metric_temp))
#     metric_temp$site = s
#     metric_temp$aru = a
#     # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
#     adi_sswma = rbind(metric_temp, adi_sswma)
#   }
# }
# 
# setwd("broad_acoustic_data/sswma_wlg_raw")
# write.csv(adi_sswma, file = "adi_sswma_full.csv", row.names = FALSE)

adi = NULL
for(s in ws_sites){
  print(paste0("broad_acoustic_data/",s,"_wlg_raw/adi_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/adi_wlg_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  adi = rbind(metric_temp, adi)
}


adi2 = adi %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(adi = "left_channel")

adi2$local_time = ifelse(adi2$site == "kiowa",force_tz(adi2$local_time, tz = "US/Mountain"),force_tz(adi2$local_time, tz = "US/Central"))
adi2$date_time = as_datetime(adi2$local_time, tz = "UTC")
adi2$aru = substr(adi2$aru,1,4)

## aei - Compile all SSWMA files
# ws_sites = "sswma"
# aru = as.list(c("ws01","ws02","ws03","ws04","ws05","ws06","ws07","ws08","ws09","ws10","ws11","ws12","ws13","ws14","ws15"))
# aei_sswma = NULL
# for(s in ws_sites){
#   for(a in aru){
#     # print(paste0("broad_acoustic_data/",s,"_wlg_raw/aei_",s,"_full.csv"))
#     # metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/aei_wlg_",s,"_full.csv"), header = TRUE)
#     print(paste0("broad_acoustic_data/",s,"_wlg_raw/",a,"_aei_results.csv"))
#     metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/",a,"_aei_results.csv"))
#     names(metric_temp) = tolower(names(metric_temp))
#     metric_temp$site = s
#     metric_temp$aru = a
#     # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
#     aei_sswma = rbind(metric_temp, aei_sswma)
#   }
# }
# 
# setwd("broad_acoustic_data/sswma_wlg_raw")
# write.csv(aei_sswma, file = "aei_wlg_sswma_full.csv", row.names = FALSE)


## AEI
aei = NULL
for(s in ws_sites){
  print(paste0("broad_acoustic_data/",s,"_wlg_raw/aei_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/aei_wlg_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  aei = rbind(metric_temp, aei)
}

aei2 = aei %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(aei = "left_channel")

aei2$local_time = ifelse(aei2$site == "kiowa",force_tz(aei2$local_time, tz = "US/Mountain"),force_tz(aei2$local_time, tz = "US/Central"))
aei2$date_time = as_datetime(aei2$local_time, tz = "UTC")
aei2$aru = substr(aei2$aru,1,4)

## BIO
bio = NULL
for(s in ws_sites){
  print(paste0("broad_acoustic_data/",s,"_wlg_raw/bio_",s,"_full.csv"))
  metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_wlg_raw/bio_wlg_",s,"_full.csv"), header = TRUE)
  names(metric_temp) = tolower(names(metric_temp))
  metric_temp$site = s
  # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  bio = rbind(metric_temp, bio)
}
bio2 = bio %>%
  mutate(year = substr(filename,1,4),
         month = substr(filename,5,6),
         day = substr(filename,7,8),
         hour = as.character(substr(filename, 10,11)),
         min = as.character(substr(filename, 12,13)),
         second = as.character(substr(filename,14,15)),
         date = as_date(substr(filename, 1,8)),
         time = as.character(paste0(hour,":",min,":",second)),
         local_time = as_datetime(as.character(paste0(date," ",time), format = "%Y-%m-%d %H:%M:%S"))) %>%
  dplyr::rename(bio = "left_channel")

bio2$local_time = ifelse(bio2$site == "kiowa",force_tz(bio2$local_time, tz = "US/Mountain"),force_tz(bio2$local_time, tz = "US/Central"))
bio2$date_time = as_datetime(bio2$local_time, tz = "UTC")
bio2$aru = substr(bio2$aru,1,4)

## Water Supplementation - Combine broad acoustic metrics into  big df --------

aco = full_join(aci2,bio2, by = c("filename", "site","aru")) %>%
  dplyr::select(filename,samplingrate.x,bit.x,fft_w.x,site,aru,date_time.x,local_time.x,aci,bio) %>%
  dplyr::rename(samplingrate = "samplingrate.x", bit = "bit.x", fft_w = "fft_w.x",date_time = "date_time.x",local_time = "local_time.x")

## Filter ADI dataset to only include - and -70 db thresholds
adi3 = adi2 %>%
  dplyr::filter(db_threshold == "-" | db_threshold == -70)

aco2 = full_join(aco, adi3, by = c("filename", "site", "aru", "samplingrate", "bit", "date_time", "local_time")) %>%
  dplyr::select(filename, samplingrate, bit, fft_w, db_threshold,site, aru, date_time, local_time, aci, bio, adi)

## ## Filter AEI dataset to only include - and -70 db thresholds
aei3 = aei2 %>%
  dplyr::filter(db_threshold == "-" | db_threshold == -70)

aco3 = full_join(aco2, aei3, by = c("filename", "site", "aru", "samplingrate", "bit","db_threshold", "date_time", "local_time")) %>%
  dplyr::select(filename, samplingrate, bit, fft_w, db_threshold,site, aru, date_time, local_time, aci, bio, adi, aei)

water_acoustics = aco3

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
save(water_acoustics, file = "water_acoustic_metrics.Rdata")


## Water Supplementation - Combine the broad acoustic metrics with BirdNET data (with NA --------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")

sites = as.list(c("sswma","cbma"))
arid_full = NULL
water_full = NULL
for(s in sites){
  # setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  # setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  if(s == "lwma"){
    # load lwma bird data
    load("birdnet_data/lwma_aru_results.Rdata")
    data = lwma_aru_results
    tz = "US/Central"
    
  } else if(s == "sswma"){
    # load sswma bird data
    load("birdnet_data/sswma_aru_results.Rdata")
    data = sswma_aru_results
    tz = "US/Central"
    
    
  } else if(s == "cbma"){
    # load cbma bird data
    load("birdnet_data/cbma_aru_results.Rdata")
    data = cbma_aru_results
    tz = "US/Central"
    
    
  } else if(s == "kiowa"){
    # load kiowa bird data
    load("birdnet_data/kiowa_aru_results.Rdata")
    data = kiowa_aru_results
    tz = "US/Mountain"
    
  }
  
  # summarize birdnet data into number of vocalizations and number of species
  data_temp = data %>%
    mutate(date = as_date(date),
           local_time = force_tz(date_time, tz = tz),
           date_time = as_datetime(local_time, tz = "UTC"),
           time = hms::as_hms(date_time),
           hour_local = hour(local_time),
           hour_utc = hour(date_time)
    ) %>%
    # filter(date > "2021-04-30 CDT")%>%
    # dplyr::filter(is.na(common_name) == FALSE) %>% 
    group_by(date,time,local_time,date_time,hour_local,hour_utc,lat,lon,aru,site) %>%
    dplyr::summarise(num_vocals = n(),
                     species_diversity = n_distinct(common_name))%>%
    arrange(date_time) %>%
    mutate(num_vocals = replace(num_vocals, is.na(lat)==TRUE && num_vocals == 1, 0),
           species_diversity = replace(species_diversity, is.na(lat)==TRUE && species_diversity == 1, 0))
  # wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
  
  
  if(s == "kiowa"){
    kiowa_time_bad = data_temp %>% 
      dplyr::filter(date == "2021-06-17" | date == "2021-06-18")%>%
      dplyr::filter(aru == "aru03") 
    kiowa_time_good = setdiff(data_temp, kiowa_time_bad)
    kiowa_time_bad = kiowa_time_bad %>%
      mutate(date_time = date_time-3600,
             local_time = local_time-3600,
             time = hms::as_hms(date_time),
             hour_local = hour(local_time),
             hour_utc = hour(date_time))
    data_temp = rbind(kiowa_time_good,kiowa_time_bad)
    
  } else {
    
  }
  
  data_temp_arid = data_temp %>%
    filter(aru == "aru01" | aru == "aru02"| aru == "aru03"| aru == "aru04"| aru == "aru05")
  
  data_temp_water = data_temp %>%
    filter(aru == "ws01" | aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05" |
             aru == "ws06" | aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10" | 
             aru == "ws11" | aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15" |
             aru == "wg01" | aru == "wg02"| aru == "wg03"| aru == "wg04"| aru == "wg05")
  
  arid_full = rbind(data_temp_arid,arid_full) %>%
    arrange(site,aru,hour_utc)
  
  water_full = rbind(data_temp_water,water_full) %>%
    arrange(site,aru,hour_utc)
  
}

# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data/birdnet_data")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data/birdnet_data")
save(water_full, file = "water_supp_ml_raw.Rdata")
# load("data/birdnet_data/aridity_gradient_ml_raw.Rdata")

# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
#Load broad acoustic metrics
load("water_acoustics.Rdata")

water_acoustics2 = full_join(water_acoustics, water_full, by = c("site", "aru", "date_time")) %>%
  dplyr::rename(local_time = "local_time.y") %>%
  dplyr::select(-local_time.x) %>%
  dplyr::select(filename, samplingrate, bit, fft_w, db_threshold, site, aru, lat, lon, local_time, date_time, aci, bio, adi, aei, num_vocals, species_diversity)

# Rounding datetime for cbma audio files that were recording every 2 min
wac2bad = water_acoustics2 %>%
  dplyr::filter(site == "cbma") %>%
  dplyr::filter(date(date_time) == "2021-06-16" | date(date_time) == "2021-06-17" | date(date_time) == "2021-06-18" | date(date_time) == "2021-06-19" | date(date_time) == "2021-06-20") %>%
  dplyr::filter(aru == "wg01")

wac2good = setdiff(water_acoustics2, wac2bad)

wac2bad = wac2bad %>%
  mutate(date_time = round_date(date_time, "10 mins")) %>%
  # group_by(filename, samplingrate, bit, fft_w, db_threshold, site, aru, lat, lon, date_time) %>%
  group_by(date_time) %>%
  summarise_at(c("aci", "bio", "adi", "aei", "num_vocals", "species_diversity"), mean) %>%
  mutate(local_time = as_datetime(date_time, tz = "US/Central"),
         filename = NA,
         samplingrate = 48000,
         bit = 16,
         fft_w = 512,
         db_threshold = "-",
         site = "cbma",
         aru = "wg01",
         lat = 35.3965,
         lon = -101.9742)

water_acoustics3 = rbind(wac2good,wac2bad)


# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
save(water_acoustics3, file = "water_acoustic_and_birdnet_data.Rdata")



## Water Supplementation - Combine 2021 Mesonet and Historic Weather Data -----------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
# acoustic2 = import("acoustic_and_birdnet_data.Rdata")

sites = as.list(c("sswma","cbma"))
wfull = NULL
for(s in sites){
  # setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
    
  if(s == "sswma"){
    load("mesonet_data/sswma_mesonet.Rdata")
    wd = sswma_mesonet%>%
      mutate(arid_within = scale(gh),
             month_day = format(as.Date(date_time), "%m-%d"))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    load("historic_weather_data/sswma_wh.Rdata")
    hd = sswma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghsite_scaled) %>%
      dplyr::rename(gh_hist = "gh_hobs", hist_within = "ghhobs_scaled", hist_across = "ghsite_scaled") %>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
  } else if(s == "cbma"){
    load("mesonet_data/cbma_mesonet.Rdata")
    wd = cbma_mesonet%>%
      mutate(arid_within = scale(gh),
             month_day = format(as.Date(date_time), "%m-%d"))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    load("historic_weather_data/cbma_wh.Rdata")
    hd = cbma_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghsite_scaled) %>%
      dplyr::rename(gh_hist = "gh_hobs", hist_within = "ghhobs_scaled", hist_across = "ghsite_scaled") %>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
  }
  weather_temp = left_join(wd, hd, by = c("month_day","mas"))
  wfull = rbind(weather_temp, wfull) %>%
    arrange(month_day, mas)
}

wfull$gh_hist = na.approx(wfull$gh_hist, na.rm = FALSE) 
wfull$hist_within = na.approx(wfull$hist_within, na.rm = FALSE) 
wfull$hist_across = na.approx(wfull$hist_across, na.rm = FALSE) 

wfull$arid_across = scale(wfull$gh)

wfull = wfull %>%
  dplyr::select(-arid)
water_wfull = wfull
save(water_wfull, file = "water_mesonet_historic_weather.Rdata")


## Water Supplementation - Combine Acoustic and Weather Data --------------------
load("water_mesonet_historic_weather.Rdata")
load("water_acoustic_and_birdnet_data.Rdata")
water_weather = full_join(water_acoustics3, water_wfull, by = c("site","date_time")) %>%
  arrange(site,aru,date_time)
# %>%
#   dplyr::filter(date(date_time) == "2021-06-17" | date(date_time) == "2021-06-18" | date(date_time) == "2021-06-19")

water_weather$gh_hist = na.approx(water_weather$gh_hist, na.rm = FALSE) 
water_weather$arid_within = na.approx(water_weather$arid_within, na.rm = FALSE)
water_weather$arid_across = na.approx(water_weather$arid_across, na.rm = FALSE)
water_weather$hist_within = na.approx(water_weather$hist_within, na.rm = FALSE) 
water_weather$hist_across = na.approx(water_weather$hist_across, na.rm = FALSE) 

water_weather2 = water_weather %>%
  dplyr::filter(is.na(num_vocals) == FALSE)

water_weather2$mas_num = as.numeric(as.character(water_weather2$mas))

# observed aridity scaled within sites
water_weather2 = water_weather2 %>% 
  arrange(arid_within) %>%
  mutate(arid_within = cut(arid_within,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

# observed aridity scaled across sites
water_weather2 = water_weather2 %>% 
  arrange(arid_across) %>%
  mutate(arid_across = cut(arid_across,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

# historic aridity scaled within sites
water_weather2 = water_weather2 %>% 
  arrange(hist_within) %>%
  mutate(hist_within = cut(hist_within,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

# historic aridity scaled across sites
water_weather2 = water_weather2 %>% 
  arrange(hist_across) %>%
  mutate(hist_across = cut(hist_across,
                           breaks = 5, 
                           labels = c(1,2,3,4,5)))

# cutting minutes after sunrise (mas) into bins based on breaks
water_weather2$mas_bin = cut(water_weather2$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

# recode sites so they are ordered east to west
water_weather2$site = factor(water_weather2$site, levels = c("lwma","sswma","cbma","kiowa"))

save(water_weather2, file = "raw_water_audio_weather.Rdata")

# Separating Sites and Designating Water Sites ----------------------------

#Separating out CBMA water sites
full_water1 = water_weather2 %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date(date_time) >= "2021-06-04" & date(date_time) <"2021-06-25"| date(date_time) >= "2021-07-19" & date(date_time) < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = water_weather2 %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)

cbma_full_water = rbind(full_water1, full_water2)

#Separating out SSWMA water sites
sswma_full_water1 = water_weather2 %>%
  filter(aru == "ws01"| aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05")%>%
  mutate(water = ifelse(date(date_time) >= "2021-05-17" & date(date_time) <"2021-05-30"| date(date_time) >= "2021-06-13" & date(date_time) < "2021-07-02", 1,0),
         ws_site = 1)

sswma_full_water2 = water_weather2 %>%
  filter(aru == "ws06"| aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10") %>%
  mutate(water = ifelse(date(date_time) >= "2021-05-30" & date(date_time) <"2021-06-12"| date(date_time) >= "2021-07-03" & date(date_time) < "2021-08-07", 1,0),
         ws_site = 2)

sswma_full_water3 = water_weather2 %>%
  filter(aru == "ws11"| aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15") %>%
  mutate(water = 0,
         ws_site = 3)

sswma_full_water = rbind(sswma_full_water1, sswma_full_water2, sswma_full_water3)
water_weather3 = rbind(cbma_full_water,sswma_full_water)

# water_weather3 = water_weather2 %>%
#   dplyr::filter(is.na(mas_bin) == FALSE)

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
save(water_weather3, file = "water_audio_and_weather_data.Rdata")

