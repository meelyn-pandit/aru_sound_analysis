---
title: "Combine Acoustic, Mesonet Data, and Historic Weather Data"
author: "Meelyn M. Pandit"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

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
```

## Load Aridity Gradient Broad acoustic data
getwd()
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data")
sites = as.list(c("lwma","sswma","cbma","kiowa"))
aru = as.list(c("aru01","aru02","aru03","aru04","aru05"))


# ACI Data Organization ---------------------------------------------------

aci = NULL
for(s in sites){
  for(a in aru){
    print(paste0("broad_acoustic_data/",s,"_arus_raw/aci_",s,"_full.csv"))
      metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/aci_",s,"_full.csv"), header = TRUE)
      names(metric_temp) = tolower(names(metric_temp))
      metric_temp$site = s
      # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  
    }
    aci = rbind(metric_temp, aci)
    aci2 = aci %>%
      mutate(year = substr(filename,1,4),
             month = substr(filename,5,6),
             day = substr(filename,7,8),
             hour = as.character(substr(filename, 10,11)),
             min = as.character(substr(filename, 12,13)),
             second = as.character(substr(filename,14,15)),
             date = as_date(substr(filename, 1,8)),
             time = as.character(paste0(hour,":",min,":",second)),
             local_time = as_datetime(as.character(paste0(date," ",time), format = "%Y-%m-%d %H:%M:%S"))) %>%
      rename(aci = "left_channel")
    
    aci2$local_time = ifelse(aci2$site == "kiowa",force_tz(aci2$local_time, tz = "US/Mountain"),force_tz(aci2$local_time, tz = "US/Central"))
    aci2$date_time = as_datetime(aci2$local_time, tz = "UTC")

}

# ADI Data Organization ---------------------------------------------------
adi = NULL
for(s in sites){
  for(a in aru){
    print(paste0("broad_acoustic_data/",s,"_arus_raw/adi_",s,"_full.csv"))
      metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/adi_",s,"_full.csv"), header = TRUE)
      names(metric_temp) = tolower(names(metric_temp))
      metric_temp$site = s
      # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")

    }
    adi = rbind(metric_temp, adi)
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
      rename(adi = "left_channel")
    
    adi2$local_time = ifelse(adi2$site == "kiowa",force_tz(adi2$local_time, tz = "US/Mountain"),force_tz(adi2$local_time, tz = "US/Central"))
    adi2$date_time = as_datetime(adi2$local_time, tz = "UTC")

}




# AEI Data Organization ---------------------------------------------------

aei = NULL
for(s in sites){
  for(a in aru){
    print(paste0("broad_acoustic_data/",s,"_arus_raw/aei_",s,"_full.csv"))
      metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/aei_",s,"_full.csv"), header = TRUE)
      names(metric_temp) = tolower(names(metric_temp))
      metric_temp$site = s
      # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  
    }
    aei = rbind(metric_temp, aei)
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
      rename(aei = "left_channel")
    
    aei2$local_time = ifelse(aei2$site == "kiowa",force_tz(aei2$local_time, tz = "US/Mountain"),force_tz(aei2$local_time, tz = "US/Central"))
    aei2$date_time = as_datetime(aei2$local_time, tz = "UTC")

}



# BIO Data Organization ---------------------------------------------------

bio = NULL
for(s in sites){
  for(a in aru){
    print(paste0("broad_acoustic_data/",s,"_arus_raw/bio_",s,"_full.csv"))
      metric_temp = read.csv(paste0("broad_acoustic_data/",s,"_arus_raw/bio_",s,"_full.csv"), header = TRUE)
      names(metric_temp) = tolower(names(metric_temp))
      metric_temp$site = s
      # metric_temp$tz = if_else(site == "kiowa", "US/Mountain","US/Central")
  
    }
    bio = rbind(metric_temp, bio)
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
      rename(bio = "left_channel")
    
    bio2$local_time = ifelse(bio2$site == "kiowa",force_tz(bio2$local_time, tz = "US/Mountain"),force_tz(bio2$local_time, tz = "US/Central"))
    bio2$date_time = as_datetime(bio2$local_time, tz = "UTC")

}


# Combining Acoustic Metrics ----------------------------------------------

aco = full_join(aci2,bio2, by = c("filename", "site","aru")) %>%
  select(filename,samplingrate.x,bit.x,fft_w.x,site,aru,date_time.x,local_time.x,aci,bio) %>%
  rename(samplingrate = "samplingrate.x", bit = "bit.x", fft_w = "fft_w.x",date_time = "date_time.x",local_time = "local_time.x")

## Filter ADI dataset to only include - and -70 db thresholds
adi3 = adi2 %>%
  dplyr::filter(db_threshold == "-" | db_threshold == -70)

aco2 = full_join(aco, adi3, by = c("filename", "site", "aru", "samplingrate", "bit", "date_time", "local_time")) %>%
  select(filename, samplingrate, bit, fft_w, db_threshold,site, aru, date_time, local_time, aci, bio, adi)

## ## Filter AEI dataset to only include - and -70 db thresholds
aei3 = aei2 %>%
  dplyr::filter(db_threshold == "-" | db_threshold == -70)

aco3 = full_join(aco2, aei3, by = c("filename", "site", "aru", "samplingrate", "bit","db_threshold", "date_time", "local_time")) %>%
  select(filename, samplingrate, bit, fft_w, db_threshold,site, aru, date_time, local_time, aci, bio, adi, aei)

acoustic = aco3

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/data_clean")
save(acoustic, file = "acoustic_metrics.Rdata")


# Combining Acoustic Metrics with Weather Data ----------------------------

#load LWMA 2021 mesonet data
lwma_acoustic = acoustic %>% dplyr::filter(site == "lwma")

load("mesonet_data/lwma_mesonet.Rdata")
wd = lwma_mesonet %>%
  mutate(ghobs_scaled = scale(gh))
wd$month_day = format(as.Date(wd$date_time), "%m-%d")
labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)

#load historic data from 2005-2021
load("historic_weather_data/lwma_wh.Rdata")
hd = lwma_wh %>%
  dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
  dplyr::filter(is.na(mas)==FALSE)%>%
  dplyr::filter(is.na(gh_hobs)==FALSE)%>%
  group_by(month_day, mas) %>%
  summarise_all(funs(mean))
tz = "US/Central"


# SSWMA -------------------------------------------------------------------

sswma_acoustic = acoustic %>% dplyr::filter(site == "sswma")

  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/sswma_mesonet.Rdata")
  wd = sswma_mesonet%>%
    mutate(ghobs_scaled = scale(gh))
  wd$month_day = format(as.Date(wd$date_time), "%m-%d")
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
  
  
  #load historic data from 2005-2021
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/sswma_wh.Rdata")
  hd = sswma_wh %>%
    dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    dplyr::filter(is.na(mas)==FALSE)  %>%
    dplyr::filter(is.na(gh_hobs)==FALSE)  %>%
    group_by(month_day, mas) %>%
    summarise_all(funs(mean))
  tz = "US/Central"
  
  

# CBMA --------------------------------------------------------------------

  cbma_acoustic = acoustic %>% dplyr::filter(site == "cbma")
  
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/cbma_mesonet.Rdata")
  wd = cbma_mesonet%>%
    mutate(ghobs_scaled = scale(gh))
  wd$month_day = format(as.Date(wd$date_time), "%m-%d")
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
  
  #load historic data from 2005-2021
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/cbma_wh.Rdata")
  hd = cbma_wh %>% 
    dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    dplyr::filter(is.na(mas)==FALSE)  %>%
    dplyr::filter(is.na(gh_hobs)==FALSE)  %>%
    group_by(month_day, mas) %>%
    summarise_all(funs(mean))
  tz = "US/Central"
  
  

# KIOWA -------------------------------------------------------------------

  kiowa_acoustic = acoustic %>% dplyr::filter(site == "kiowa")
  
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/kiowa_mesonet.Rdata")
  wd = kiowa_mesonet%>%
    mutate(ghobs_scaled = scale(gh))
  wd$month_day = format(as.Date(wd$date_time), "%m-%d")
  labels = seq(-750,730,5) #creating bin labels, going from lowest value to the highest value-5
  wd$mas = cut(wd$mas, seq(-750,735,5),labels = labels, right = FALSE)
  
  #load historic data from 2005-2021
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/kiowa_wh.Rdata")
  hd = kiowa_wh %>% 
    dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    dplyr::filter(is.na(mas)==FALSE)  %>%
    dplyr::filter(is.na(gh_hobs)==FALSE)  %>%
    group_by(month_day, mas) %>%
    summarise_all(funs(mean))
  tz = "US/Mountain"
  
