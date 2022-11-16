
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

# ## Load Aridity Gradient Broad acoustic data ----------------------------

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


# ## Combine the broad acoustic metrics with BirdNET data (with NA --------

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


# ## Combine 2021 Mesonet and Historic Weather Data -----------------------

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

## Combine Acoustic and Weather Dataset

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
aw2$arid_within = factor(cut(aw2$arid_within, breaks = 5, labels = c(1,2,3,4,5)), levels = c(1,2,3,4,5)) # observed aridity scaled within sites
aw2$arid_across = cut(aw2$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
aw2$hist_within = cut(aw2$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
aw2$hist_across = cut(aw2$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
aw2$mas_bin = cut(aw2$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

aw2$site = factor(aw2$site, levels = c("lwma","sswma","cbma","kiowa"))

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
save(aw2, file = "audio_and_weather_data.Rdata")


