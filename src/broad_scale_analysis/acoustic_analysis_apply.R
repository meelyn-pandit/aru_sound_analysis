library(soundecology) #obtain broad scale acoustic metrics
library(tuneR) #loading and reading sound files
library(seewave) #soundwave analysis
library(osfr) #downloading files from osf
library(dplyr) #data management and conversion
library(lubridate) #convert date types
library(lme4) #linear mixed models
library(lmerTest) #statistical tests for linear mixed models
library(ggplot2) #create good graphs
library(extrafont) #change fonts for ggplot2
library(lsmeans) #post-hoc tests for mixed models
library(zoo) #approximate missing values

#Set so you know which computer you are using; your personal computer or the OK Biosurvey computer
computer = 0
if(computer == 0){
  m = "meely"
} else if(computer == 1){
  m = "meelyn.pandit"
} else if(computer == 2){
  m = "obs"
}

# aru = c("aru03","aru04","aru05") #create list of directories that the sound analysis code will be applied to
aru = c("ws01","ws02","ws03","ws04","ws05","ws06","ws07","ws08","ws09","ws10", "ws11","ws12","ws13","ws14","ws15")
# aru = c("wg02","wg03","wg04","wg05")
# aru = c("aru01","aru04","aru05")

aru_list = as.list(aru) #make the directories a list
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/") #set working directory, this is the working directory on the harddrive with all the stored aru recordings.
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/cbma_arus_raw/")
# setwd("E:/aridity_project/sswma/sswma_audio_files/")
setwd("/Volumes/LaCie/aridity_project/sswma/sswma_audio_files")
# Load Weather data -------------------------------------------------------
# load("sitka_weather_data.Rdata")


# Run Apply function to obtain Acoustic Indices ---------------------------
lapply(aru_list, function(x){
  #Acoustic Complexity Index - measures overall acoustic distinctiveness in recordings, biophonies and geophonies based on variability and length of sound intensity
  results = paste0(x, "_aci_results.csv") #create a csv file to write results to
  #multiple_sounds function is used to analyze multiple sounds at once. you specify which index you want with "soundindex"
  multiple_sounds(directory = x, resultfile = results, soundindex = "acoustic_complexity",min_freq = 1, max_freq = 8000, no_cores = "max")  
  aci_results = read.csv(results, header = TRUE) #upload the results file into R so we can add more variables to the datafile
  names(aci_results) = tolower(names(aci_results)) #renaming columns
  aci_results$aru = substr(x, 1,5) #obtain aru number from the directory name, you may need to change the length of the substring based on your directory name
  aci_results$year = as.factor((substr(aci_results$filename, 1,4))) #obtain year from AudioMoth recording filename
  aci_results$month = as.factor((substr(aci_results$filename,5,6))) #obtain month from AudioMoth recording filename
  aci_results$day = as.factor((substr(aci_results$filename, 7,8))) #obtain day from AudioMoth recording filename
  aci_results$hour = as.factor(substr(aci_results$filename, 10,11)) #obtain hour from AudioMoth recording filename
  aci_results$min = as.factor(substr(aci_results$filename, 12,13)) #obtain minute from AudioMoth recording filename
  aci_results$second = as.factor(substr(aci_results$filename, 14,15)) #obtain seconds from AudioMoth recording filename
  aci_results$date = as.character(paste0(aci_results$year,"-",aci_results$month,"-",aci_results$day)) #combine year, month, and day to get date
  aci_results$time = as.character(paste0(aci_results$hour,":",aci_results$min,":",aci_results$second)) #combine hour, minute, and seconds to get time
  aci_results$date_time = as.POSIXct(as.character(paste0(aci_results$date," ", aci_results$time), format = "%Y-%m-%d %H:%M:%S"))#create datetime variable
  aci_results_df = as.data.frame(aci_results) #convert results into a dataframe
  # aci_results2 = merge(aci_results_df, weather_data, by.x = "date_time", by.y = "local_time") #merge your aci_dataset with the weather_data based on date_time in the aci_results dataframe and the local_time in the weather_data dataframe
  
  write.csv(aci_results_df, file = paste0("aci_results_raw_",aci_results_df$aru[1],".csv", sep = ""),row.names = FALSE) #save the dataframe as a new csv file
  gc(reset = TRUE)
  
  # #Acoustic Diversity Index
  results2 = paste0(x, "_adi_results.csv")
  multiple_sounds(directory = x, resultfile = results2, soundindex = "acoustic_diversity",max_freq = 8000, no_cores = "max")
  adi_results <- read.csv(results2, header = TRUE)
  names(adi_results) = tolower(names(adi_results))
  adi_results$aru = x
  adi_results$year = as.factor((substr(adi_results$filename, 1,4)))
  adi_results$month = as.factor((substr(adi_results$filename,5,6)))
  adi_results$day = as.factor((substr(adi_results$filename, 7,8)))
  adi_results$hour = as.factor(substr(adi_results$filename, 10,11))
  adi_results$min = as.factor(substr(adi_results$filename, 12,13))
  adi_results$second = as.factor(substr(adi_results$filename, 14,15))
  adi_results$date = as.character(paste0(adi_results$year,"-",adi_results$month,"-",adi_results$day))
  adi_results$time = as.character(paste0(adi_results$hour,":",adi_results$min,":",adi_results$second))
  adi_results$date_time = as.POSIXct(as.character(paste0(adi_results$date," ",adi_results$time),format = "%Y-%m-%d %H:%M:%S"))
  adi_results_df = as.data.frame(adi_results)
  # adi_results2 = merge(adi_results_df, sswma_data, by.x = "date_time", by.y = "local_time")
  write.csv(adi_results_df, file = paste0("adi_results_raw_",adi_results_df$aru[1],".csv", sep = ""),row.names = FALSE)
  gc(reset = TRUE)
  
  ##Acoustic Eveness Index
    results3 = paste0(x, "_aei_results.csv")
    multiple_sounds(directory = x, resultfile = results3, soundindex = "acoustic_evenness",max_freq = 8000, no_cores = "max")
    aei_results <- read.csv(results3, header = TRUE)
    names(aei_results) = tolower(names(aei_results))
    aei_results$aru = x
    aei_results$year = as.factor((substr(aei_results$filename, 1,4)))
    aei_results$month = as.factor((substr(aei_results$filename,5,6)))
    aei_results$day = as.factor((substr(aei_results$filename, 7,8)))
    aei_results$hour = as.factor(substr(aei_results$filename, 10,11))
    aei_results$min = as.factor(substr(aei_results$filename, 12,13))
    aei_results$second = as.factor(substr(aei_results$filename, 14,15))
    aei_results$date = as.character(paste0(aei_results$year,"-",aei_results$month,"-",aei_results$day))
    aei_results$time = as.character(paste0(aei_results$hour,":",aei_results$min,":",aei_results$second))
    aei_results$date_time = as.POSIXct(as.character(paste0(aei_results$date," ", aei_results$time), format = "%Y-%m-%d %H:%M:%S"))#
    aei_results_df = as.data.frame(aei_results)
    # aei_results2 = merge(aei_results_df, sswma_data, by.x = "date_time", by.y = "local_time")
    write.csv(aei_results_df,
              file = paste0("aei_results_raw_",aei_results_df$aru[1],".csv", sep = ""),
              row.names = FALSE)
    gc(reset = TRUE)
    
    # Biology Acoustic Dataset Labelling --------------------------------------
    results4 = paste0(x, "_bio_results.csv")
    multiple_sounds(directory = x, resultfile = results4, soundindex = "bioacoustic_index",min_freq = 1000, max_freq = 8000, no_cores = "max")
    bio_results = read.csv(results4, header = TRUE)
    names(bio_results) = tolower(names(bio_results))
    bio_results$aru = x
    bio_results$year = as.factor((substr(bio_results$filename, 1,4)))
    bio_results$month = as.factor((substr(bio_results$filename,5,6)))
    bio_results$day = as.factor((substr(bio_results$filename, 7,8)))
    bio_results$hour = as.factor(substr(bio_results$filename, 10,11))
    bio_results$min = as.factor(substr(bio_results$filename, 12,13))
    bio_results$second = as.factor(substr(bio_results$filename, 14,15))
    bio_results$date = as.character(paste0(bio_results$year,"-",bio_results$month,"-",bio_results$day))
    bio_results$time = as.character(paste0(bio_results$hour,":",bio_results$min,":",bio_results$second))
    bio_results$date_time = as.POSIXct(as.character(paste0(bio_results$date," ", bio_results$time), format = "%Y-%m-%d %H:%M:%S"))
    bio_results_df = as.data.frame(bio_results)
    # bio_results2 = merge(bio_results_df, sswma_data, by.x = "date_time", by.y = "local_time")
    write.csv(bio_results_df,
              file = paste0("bio_results_raw_",bio_results_df$aru[1],".csv", sep = ""),
              row.names = FALSE)
    gc(reset = TRUE)
    
  })


# Adding Weather data and Approximating missing values --------------------
sites = as.list(c("lwma","sswma","cbma","kiowa"))
aci = lapply(sites, function(x){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/")
  if(x == "lwma"){
    load("lwma_mesonet.Rdata")
    weather_data = lwma_mesonet
    tz = "US/Central"
  } else if(x == "sswma"){
    load("sswma_mesonet.Rdata")
    weather_data = sswma_mesonet
    tz = "US/Central"
    
    
  } else if(x == "cbma"){
    load("cbma_mesonet.Rdata")
    weather_data = cbma_mesonet
    tz = "US/Central"
    
    
  } else if(x == "kiowa"){
    load("kiowa_mesonet.Rdata")
    weather_data = kiowa_mesonet
    tz = "US/Mountain"
    
  }
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",x,"_arus_raw/"))
  # Load Weather data ------------------------------------------------------
  aci_full = read.csv(paste0("aci_",x,"_full.csv"), header = TRUE)
  names(aci_full) = tolower(names(aci_full))
  aci_full$year = as.factor((substr(aci_full$filename, 1,4))) #obtain year from AudioMoth recording filename
  aci_full$month = as.factor((substr(aci_full$filename,5,6))) #obtain month from AudioMoth recording filename
  aci_full$day = as.factor((substr(aci_full$filename, 7,8))) #obtain day from AudioMoth recording filename
  aci_full$hour = as.integer(substr(aci_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
  aci_full$min = as.factor(substr(aci_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
  aci_full$second = as.factor(substr(aci_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
  aci_full$date = as.character(paste0(aci_full$year,"-",aci_full$month,"-",aci_full$day)) #combine year, month, and day to get date
  aci_full$time = as.character(paste0(aci_full$hour,":",aci_full$min,":",aci_full$second)) #combine hour, minute, and seconds to get time
  aci_full$local_time = as.POSIXct(as.character(paste0(aci_full$date," ", aci_full$time), format = "%Y-%m-%d %H:%M:%S"), tz = tz)#create local datetime variable
  
  if(x == "kiowa"){
    kiowa_time_bad = aci_full %>% dplyr::filter(hour ==12)%>%
      mutate(local_time = local_time-3600
             # ,
             # time = as_hms(local_time),
             # hour = hour(local_time)
             )
    kiowa_time_good = aci_full %>% dplyr::filter(hour !=12)
    aci_full = rbind(kiowa_time_bad,kiowa_time_good) 
    
  } else {
    
  }
  aci_full$date_time = as_datetime(aci_full$local_time, tz = "UTC")
  aci_full_weather = left_join(aci_full, weather_data, by = c("date_time")) 
  aci_full_weather = aci_full_weather %>% dplyr::filter(is.na(temp)==FALSE)
  
  return(aci_full_weather)
  # return(aci_full)
})

lwma_aci = aci[[1]]
sswma_aci = aci[[2]]
cbma_aci = aci[[3]]
kiowa_aci = aci[[4]]

aci_full_weather = rbind(lwma_aci,sswma_aci,cbma_aci,kiowa_aci) %>%
  mutate(site.y = factor(site.y, levels=c("lwma","sswma","cbma","kiowa")))

aci_missing = aci_full_weather %>% dplyr::filter(is.na(temp)==TRUE)

aci_hour = aci_full_weather %>%
  # mutate(hour = hour(date_time),
  #        site = "lwma",
  #        dew = TAIR-((100-RELH)/5),
  #        arid = abs((1/dew)),
  #        mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site.y,hour.y) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))
ggplot(data = aci_hour, aes(x = hour.y, y = mean_relh, color = site.y))+
  geom_line()

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(aci_full_weather, file = "aci_full_weather.Rdata")

bio = lapply(sites, function(x){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/")
  if(x == "lwma"){
    load("lwma_mesonet.Rdata")
    weather_data = lwma_mesonet
    tz = "US/Central"
  } else if(x == "sswma"){
    load("sswma_mesonet.Rdata")
    weather_data = sswma_mesonet
    tz = "US/Central"
    
    
  } else if(x == "cbma"){
    load("cbma_mesonet.Rdata")
    weather_data = cbma_mesonet
    tz = "US/Central"
    
    
  } else if(x == "kiowa"){
    load("kiowa_mesonet.Rdata")
    weather_data = kiowa_mesonet
    tz = "US/Mountain"
    
    
  }
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",x,"_arus_raw/"))
  bio_full = read.csv(paste0("bio_",x,"_full.csv"), header = TRUE)
  names(bio_full) = tolower(names(bio_full))
  bio_full$year = as.factor((substr(bio_full$filename, 1,4))) #obtain year from AudioMoth recording filename
  bio_full$month = as.factor((substr(bio_full$filename,5,6))) #obtain month from AudioMoth recording filename
  bio_full$day = as.factor((substr(bio_full$filename, 7,8))) #obtain day from AudioMoth recording filename
  bio_full$hour = as.integer(substr(bio_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
  bio_full$min = as.factor(substr(bio_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
  bio_full$second = as.factor(substr(bio_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
  bio_full$date = as.character(paste0(bio_full$year,"-",bio_full$month,"-",bio_full$day)) #combine year, month, and day to get date
  bio_full$time = as.character(paste0(bio_full$hour,":",bio_full$min,":",bio_full$second)) #combine hour, minute, and seconds to get time
  bio_full$local_time = as.POSIXct(as.character(paste0(bio_full$date," ", bio_full$time), format = "%Y-%m-%d %H:%M:%S"), tz = tz)#create local datetime variable
  
  if(x == "kiowa"){
    kiowa_time_bad = bio_full %>% dplyr::filter(hour ==12)%>%
      mutate(local_time = local_time-3600
             # ,
             # time = as_hms(local_time),
             # hour = hour(local_time)
      )
    kiowa_time_good = bio_full %>% dplyr::filter(hour !=12)
    bio_full = rbind(kiowa_time_bad,kiowa_time_good) 
    
  } else {
    
  }
  bio_full$date_time = as_datetime(bio_full$local_time, tz = "UTC")
  bio_full_weather = left_join(bio_full, weather_data, by = c("date_time"))
  
  bio_full_weather = bio_full_weather %>% dplyr::filter(is.na(temp)==FALSE)
  
  return(bio_full_weather)
  # return(bio_full)
})

lwma_bio = bio[[1]]
sswma_bio = bio[[2]]
cbma_bio = bio[[3]]
kiowa_bio = bio[[4]]

bio_full_weather = rbind(lwma_bio,sswma_bio,cbma_bio,kiowa_bio)%>%
  mutate(site.y = factor(site.y, levels=c("lwma","sswma","cbma","kiowa")))

bio_missing = bio_full_weather %>% dplyr::filter(is.na(temp)==TRUE)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(bio_full_weather, file = "bio_full_weather.Rdata")


bio_hour = bio_full_weather %>%
  # mutate(hour = hour(date_time),
  #        site = "lwma",
  #        dew = TAIR-((100-RELH)/5),
  #        arid = abs((1/dew)),
  #        mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site.y,hour.y) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))
ggplot(data = bio_hour, aes(x = hour.y, y = mean_gh, color = site.y))+
  geom_line()


# Water Supplementation Experiment Acoustic Indices ----------------------------------------

# Adding Weather data and Approximating missing values --------------------
ws_sites = as.list(c("sswma","cbma"))
aci_ws = lapply(ws_sites, function(x){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/")
 if(x == "sswma"){
    load("sswma_mesonet.Rdata")
    weather_data = sswma_mesonet
    tz = "US/Central"
  } else if(x == "cbma"){
    load("cbma_mesonet.Rdata")
    weather_data = cbma_mesonet
    tz = "US/Central"
  } 
  
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",x,"_wlg_raw/"))
  # Load Weather data ------------------------------------------------------
  aci_full = read.csv(paste0("aci_wlg_",x,"_full.csv"), header = TRUE)
  names(aci_full) = tolower(names(aci_full))
  aci_full$year = as.factor((substr(aci_full$filename, 1,4))) #obtain year from AudioMoth recording filename
  aci_full$month = as.factor((substr(aci_full$filename,5,6))) #obtain month from AudioMoth recording filename
  aci_full$day = as.factor((substr(aci_full$filename, 7,8))) #obtain day from AudioMoth recording filename
  aci_full$hour = as.integer(substr(aci_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
  aci_full$min = as.factor(substr(aci_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
  aci_full$second = as.factor(substr(aci_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
  aci_full$date = as.character(paste0(aci_full$year,"-",aci_full$month,"-",aci_full$day)) #combine year, month, and day to get date
  aci_full$time = as.character(paste0(aci_full$hour,":",aci_full$min,":",aci_full$second)) #combine hour, minute, and seconds to get time
  aci_full$local_time = as.POSIXct(as.character(paste0(aci_full$date," ", aci_full$time), format = "%Y-%m-%d %H:%M:%S"), tz = tz)#create local datetime variable
  
  if(x == "kiowa"){
    kiowa_time_bad = aci_full %>% dplyr::filter(hour ==12)%>%
      mutate(local_time = local_time-3600
             # ,
             # time = as_hms(local_time),
             # hour = hour(local_time)
      )
    kiowa_time_good = aci_full %>% dplyr::filter(hour !=12)
    aci_full = rbind(kiowa_time_bad,kiowa_time_good) 
    
  } else {
    
  }
  
  
  aci_full$date_time = as_datetime(aci_full$local_time, tz = "UTC")
  aci_full_weather = left_join(aci_full, weather_data, by = c("date_time")) 
  aci_full_weather = aci_full_weather %>% dplyr::filter(is.na(temp)==FALSE)
  
  return(aci_full_weather)
  # return(aci_full)
})

sswma_aci = aci_ws[[1]]
sswma_aci$date = as_date(sswma_aci$date)

sswma_aci1 = sswma_aci %>%
  filter(aru == "ws01_"| aru == "ws02_"| aru == "ws03_"| aru == "ws04_"| aru == "ws05_")%>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0),
         ws_site = 1)

sswma_aci2 = sswma_aci %>%
  filter(aru == "ws06_"| aru == "ws07_"| aru == "ws08_"| aru == "ws09_"| aru == "ws10_") %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0),
         ws_site = 2)

sswma_aci3 = sswma_aci %>%
  filter(aru == "ws11_"| aru == "ws12_"| aru == "ws13_"| aru == "ws14_"| aru == "ws15_") %>%
  mutate(water = 0,
         ws_site = 3)

sswma_aci = rbind(sswma_aci1, sswma_aci2, sswma_aci3)

cbma_aci = aci_ws[[2]]
cbma_aci = cbma_aci%>%
  dplyr::filter(hour.x < 13) %>%
  dplyr::filter(hour.x > 4) %>%
  mutate(date = as_date(date))
cbma_aci1 = cbma_aci %>%
  filter(aru == "wg01_" | aru == "wg02_" | aru == "wg03_") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1)
cbma_aci2 = cbma_aci %>%
  filter(aru == "wg04_" | aru == "wg05_") %>%
  mutate(water = 1,
         ws_site = 2)
cbma_aci = rbind(cbma_aci1, cbma_aci2)

# #uncovered = access to water = 1. covered = no access to water = 0
# 
# 
# #uncomment below for Acoustic diversity index and acoustic eveness index
# cbma_water_aei = cbma_water_aei %>%
#   rename(db_threshold = min_freq) %>%
#   rename(min_freq = max_freq)
# cbma_water_aei = cbma_water_aei %>% filter(db_threshold == 50)
# cbma_water_aei$water = cbma_water_aei$water[,40] #change to 41 for acoustic complexity index, other indices use 40




aci_water = rbind(sswma_aci,cbma_aci) %>%
  mutate(site.y = factor(site.y, levels=c("sswma","cbma")))

aci_missing = aci_water %>% dplyr::filter(is.na(temp)==TRUE)

aci_hour = aci_water %>%
  # mutate(hour = hour(date_time),
  #        site = "lwma",
  #        dew = TAIR-((100-RELH)/5),
  #        arid = abs((1/dew)),
  #        mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site.y,hour.y) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))
ggplot(data = aci_hour, aes(x = hour.y, y = mean_relh, color = site.y))+
  geom_line()

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(aci_water, file = "aci_water.Rdata")

bio_ws = lapply(ws_sites, function(x){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/")
  if(x == "lwma"){
    load("lwma_mesonet.Rdata")
    weather_data = lwma_mesonet
    tz = "US/Central"
  } else if(x == "sswma"){
    load("sswma_mesonet.Rdata")
    weather_data = sswma_mesonet
    tz = "US/Central"
    
    
  } else if(x == "cbma"){
    load("cbma_mesonet.Rdata")
    weather_data = cbma_mesonet
    tz = "US/Central"
    
    
  } else if(x == "kiowa"){
    load("kiowa_mesonet.Rdata")
    weather_data = kiowa_mesonet
    tz = "US/Mountain"
    
    
  }
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",x,"_wlg_raw/"))
  bio_full = read.csv(paste0("bio_wlg_",x,"_full.csv"), header = TRUE)
  names(bio_full) = tolower(names(bio_full))
  bio_full$year = as.factor((substr(bio_full$filename, 1,4))) #obtain year from AudioMoth recording filename
  bio_full$month = as.factor((substr(bio_full$filename,5,6))) #obtain month from AudioMoth recording filename
  bio_full$day = as.factor((substr(bio_full$filename, 7,8))) #obtain day from AudioMoth recording filename
  bio_full$hour = as.integer(substr(bio_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
  bio_full$min = as.factor(substr(bio_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
  bio_full$second = as.factor(substr(bio_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
  bio_full$date = as.character(paste0(bio_full$year,"-",bio_full$month,"-",bio_full$day)) #combine year, month, and day to get date
  bio_full$time = as.character(paste0(bio_full$hour,":",bio_full$min,":",bio_full$second)) #combine hour, minute, and seconds to get time
  bio_full$local_time = as.POSIXct(as.character(paste0(bio_full$date," ", bio_full$time), format = "%Y-%m-%d %H:%M:%S"), tz = tz)#create local datetime variable
  
  if(x == "kiowa"){
    kiowa_time_bad = bio_full %>% dplyr::filter(hour ==12)%>%
      mutate(local_time = local_time-3600
             # ,
             # time = as_hms(local_time),
             # hour = hour(local_time)
      )
    kiowa_time_good = bio_full %>% dplyr::filter(hour !=12)
    bio_full = rbind(kiowa_time_bad,kiowa_time_good) 
    
  } else {
    
  }
  bio_full$date_time = as_datetime(bio_full$local_time, tz = "UTC")
  bio_full_weather = left_join(bio_full, weather_data, by = c("date_time"))
  
  bio_full_weather = bio_full_weather %>% dplyr::filter(is.na(temp)==FALSE)
  
  return(bio_full_weather)
  # return(bio_full)
})


sswma_bio = bio_ws[[1]]
sswma_bio$date = as_date(sswma_bio$date)

sswma_bio1 = sswma_bio %>%
  filter(aru == "ws01_"| aru == "ws02_"| aru == "ws03_"| aru == "ws04_"| aru == "ws05_")%>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0),
         ws_site = 1)

sswma_bio2 = sswma_bio %>%
  filter(aru == "ws06_"| aru == "ws07_"| aru == "ws08_"| aru == "ws09_"| aru == "ws10_") %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0),
         ws_site = 2)

sswma_bio3 = sswma_bio %>%
  filter(aru == "ws11_"| aru == "ws12_"| aru == "ws13_"| aru == "ws14_"| aru == "ws15_") %>%
  mutate(water = 0,
         ws_site = 3)

sswma_bio = rbind(sswma_bio1, sswma_bio2, sswma_bio3)

cbma_bio = bio_ws[[2]]
cbma_bio = cbma_bio%>%
  dplyr::filter(hour.x < 13) %>%
  dplyr::filter(hour.x > 4) %>%
  mutate(date = as_date(date))
cbma_bio1 = cbma_bio %>%
  filter(aru == "wg01_" | aru == "wg02_" | aru == "wg03_") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1)
cbma_bio2 = cbma_bio %>%
  filter(aru == "wg04_" | aru == "wg05_") %>%
  mutate(water = 1,
         ws_site = 2)
cbma_bio = rbind(cbma_bio1, cbma_bio2)


bio_water = rbind(sswma_bio,cbma_bio)%>%
  mutate(site.y = factor(site.y, levels=c("sswma","cbma")))

bio_missing = bio_water %>% dplyr::filter(is.na(temp)==TRUE)

bio_hour = bio_water %>%
  # mutate(hour = hour(date_time),
  #        site = "lwma",
  #        dew = TAIR-((100-RELH)/5),
  #        arid = abs((1/dew)),
  #        mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  group_by(site.y,hour.y) %>%
  summarize(mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(gh))
ggplot(data = bio_hour, aes(x = hour.y, y = mean_gh, color = site.y))+
  geom_line()

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(bio_water, file = "bio_water.Rdata")
# Joining Broad Acoustic Metrics to Validation Data -----------------------


setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data")
validation_data = read.csv("validation_acoustic_indices.csv", header = TRUE)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/sswma_water_station_acoustic_metrics/compiled_data/")
valid_aei = read.csv("aei_all_water_sswma.csv", header = TRUE)

aei_combo = validation_data %>%
  select(date_time,aru) %>%
  filter(aru != "aru01") %>%
  filter(aru != "aru04") %>%
  filter(aru != "aru05") 

aei_full = valid_aei %>%
  select(date_time,aru,left_channel) 



aei_joined = left_join(aei_combo, aei_full, by = c("date_time","aru"))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data")
write.csv(aei_joined, "valid_aei.csv", row.names = FALSE)

            


# Adding Weather data and Approximating missing values --------------------
sites = as.list(c("lwma","sswma","cbma","kiowa"))
sites = as.list(c("cbma"))
aci = lapply(sites, function(x){
  if(x == "lwma"){
    #load 2021 mesonet data
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/lwma_mesonet.Rdata")
    wd = lwma_mesonet
    
    #load historic data from 2005-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/lwma_wh.Rdata")
    hd = lwma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
  } else if(x == "sswma"){
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/sswma_mesonet.Rdata")
    wd = sswma_mesonet
    
    #load historic data from 2005-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/sswma_wh.Rdata")
    hd = sswma_wh %>%
    dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(x == "cbma"){
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/cbma_mesonet.Rdata")
    wd = cbma_mesonet
    
    #load historic data from 2005-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/cbma_wh.Rdata")
    hd = cbma_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(x == "kiowa"){
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/kiowa_mesonet.Rdata")
    wd = kiowa_mesonet
    
    #load historic data from 2005-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/kiowa_wh.Rdata")
    hd = kiowa_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Mountain"
    
  }
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",x,"_arus_raw/"))
  # Load Weather data ------------------------------------------------------
  aci_full = read.csv(paste0("aci_",x,"_full.csv"), header = TRUE)
  names(aci_full) = tolower(names(aci_full))
  aci_full$year = as.factor((substr(aci_full$filename, 1,4))) #obtain year from AudioMoth recording filename
  aci_full$month = as.factor((substr(aci_full$filename,5,6))) #obtain month from AudioMoth recording filename
  aci_full$day = as.factor((substr(aci_full$filename, 7,8))) #obtain day from AudioMoth recording filename
  aci_full$hour = as.integer(substr(aci_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
  aci_full$min = as.factor(substr(aci_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
  aci_full$second = as.factor(substr(aci_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
  aci_full$date = as.character(paste0(aci_full$year,"-",aci_full$month,"-",aci_full$day)) #combine year, month, and day to get date
  aci_full$time = as.character(paste0(aci_full$hour,":",aci_full$min,":",aci_full$second)) #combine hour, minute, and seconds to get time
  
  aci_full$local_time = as.POSIXct(as.character(paste0(aci_full$date," ", aci_full$time), format = "%Y-%m-%d %H:%M:%S"), tz = tz)#create local datetime variable

  
  #Normalize Acoustic data, using both min-max score and z score within sites
  aci_full = aci_full %>%
    dplyr::filter(is.na(left_channel)==FALSE)%>%
    mutate(aci_obs = left_channel,
           aci_scaled = scale(left_channel))
    # mutate(siteaci_minmax = (left_channel-min(left_channel))/(max(left_channel)-min(left_channel)),
    #        siteaci_zscore = (left_channel-(mean(left_channel))/sd(left_channel)))
  
  if(x == "kiowa"){
    kiowa_time_bad = aci_full %>% dplyr::filter(hour ==12)%>%
      mutate(local_time = local_time-3600
             # ,
             # time = as_hms(local_time),
             # hour = hour(local_time)
      )
    kiowa_time_good = aci_full %>% dplyr::filter(hour !=12)
    aci_full = rbind(kiowa_time_bad,kiowa_time_good) %>%
      dplyr::filter(year == 2021)
    
  } else {
    
  }
  aci_full$date_time = as_datetime(aci_full$local_time, tz = "UTC")
  aci_fullw = left_join(aci_full, wd, by=c("date_time"))%>%
    rename(gh_obs = "gh")
  aci_fullw$ghobs_scaledwin = scale(aci_fullw$gh_obs)
  
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  aci_fullw = aci_fullw %>% mutate(month_day = format(as.Date(date), "%m-%d"))%>%
    rename(hour_utc = "hour.y",
           min_as = "mas")%>%
    mutate(mas = cut(min_as, seq(-725,760,5),labels = labels, right = FALSE))
  
  # hd = hd %>% dplyr::filter(is.na(gh_obs)==FALSE)
  aci_fullwh = full_join(aci_fullw, hd, na_matches = "na") %>%
    arrange(month_day,mas)#need to do a full combine and then approximate missing aridity values, also need to extend the mesonet date to 09/01, utc time cuts out some of the 08/31/21 data?
  
  aci_fullwh$gh_hobs = na.approx(aci_fullwh$gh_hobs, na.rm = FALSE)
  aci_fullwh$ghhobs_scaled = na.approx(aci_fullwh$ghhobs_scaled, na.rm = FALSE)
  aci_fullwh$ghmean_time = na.approx(aci_fullwh$ghmean_time, na.rm = FALSE)
  aci_fullwh$ghsite_scaled = na.approx(aci_fullwh$ghsite_scaled, na.rm = FALSE)
  
  aci_missing = aci_fullwh %>% dplyr::filter(is.na(gh_hobs)==TRUE) #see which dates are missing aridity values
  aci_fullwh2 = aci_fullwh %>% 
    dplyr::filter(is.na(gh_hobs)==FALSE) %>%
    dplyr::filter(is.na(left_channel)==FALSE)
  
  aci_missing = aci_fullwh2 %>% dplyr::filter(is.na(left_channel)==TRUE) #see which dates are missing aridity values
  
  return(aci_fullwh2)
  # return(aci_full)
})

lwma_aci = aci[[1]]
sswma_aci = aci[[2]]
cbma_aci = aci[[3]]
kiowa_aci = aci[[4]]


aci_full = rbind(lwma_aci,sswma_aci,cbma_aci,kiowa_aci) %>%
  dplyr::select(-c(site.y))%>%
  rename(site = "site.x",
         hour = "hour.x")%>%
  mutate(site = factor(site, levels=c("lwma","sswma","cbma","kiowa")),
         hour_utc = hour(date_time))

aci_acrsite = aci_full %>%
  # mutate(month_day = format(as.Date(date), "%m-%d"))%>%
  mutate(stgh_across = scale(gh_obs))%>%
  group_by(month_day,mas)%>%
  summarise(n = n(),
            mean_ghacross = mean(gh_obs),
            sd_ghacross = sd(gh_obs),
            se_ghacross = (sd(gh_obs)/sqrt(n())),
            stmean_ghacross = mean(stgh_across),
            stsd_ghacross = sd(stgh_across),
            stse_ghacross = (sd(stgh_across)/sqrt(n())))

aci_full = left_join(aci_full,aci_acrsite, by = c("month_day","mas"))

aci_missing = aci_full %>% dplyr::filter(is.na(left_channel)==TRUE)

aci_hour = aci_full %>%
  dplyr::filter(is.na(left_channel)==FALSE) %>%
  # mutate(hour = hour(date_time),
  #        site = "lwma",
  #        dew = TAIR-((100-RELH)/5),
  #        arid = abs((1/dew)),
  #        mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  mutate(mas2 = as.numeric(as.character(mas)))%>%
  group_by(site,month_day,mas2) %>%
  summarize(mean_aci = mean(left_channel),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            # mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(ghsite_scaled))
ggplot(data = aci_hour, aes(x = mas2, y = mean_gh, color = site))+
  geom_line()

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(aci_full, file = "aci_full.Rdata")


# Combining Mesonet Data to BIO Acoustic Index Data -----------------------

sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("sswma"))
bio = lapply(sites, function(x){
  if(x == "lwma"){
    #load 2021 mesonet data
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/lwma_mesonet.Rdata")
    wd = lwma_mesonet
    
    #load historic data from 2012-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/lwma_wh.Rdata")
    hd = lwma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
  } else if(x == "sswma"){
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/sswma_mesonet.Rdata")
    wd = sswma_mesonet
    
    #load historic data from 2012-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/sswma_wh.Rdata")
    hd = sswma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(x == "cbma"){
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/cbma_mesonet.Rdata")
    wd = cbma_mesonet
    
    #load historic data from 2012-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/cbma_wh.Rdata")
    hd = cbma_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(x == "kiowa"){
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/kiowa_mesonet.Rdata")
    wd = kiowa_mesonet
    
    #load historic data from 2012-2021
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/historic_weather_data/kiowa_wh.Rdata")
    hd = kiowa_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Mountain"
    
  }
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",x,"_arus_raw/"))
  # Load Weather data ------------------------------------------------------
  bio_full = read.csv(paste0("bio_",x,"_full.csv"), header = TRUE)
  names(bio_full) = tolower(names(bio_full))
  bio_full$year = as.factor((substr(bio_full$filename, 1,4))) #obtain year from AudioMoth recording filename
  bio_full$month = as.factor((substr(bio_full$filename,5,6))) #obtain month from AudioMoth recording filename
  bio_full$day = as.factor((substr(bio_full$filename, 7,8))) #obtain day from AudioMoth recording filename
  bio_full$hour = as.integer(substr(bio_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
  bio_full$min = as.factor(substr(bio_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
  bio_full$second = as.factor(substr(bio_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
  bio_full$date = as.character(paste0(bio_full$year,"-",bio_full$month,"-",bio_full$day)) #combine year, month, and day to get date
  bio_full$time = as.character(paste0(bio_full$hour,":",bio_full$min,":",bio_full$second)) #combine hour, minute, and seconds to get time
  
  bio_full$local_time = as.POSIXct(as.character(paste0(bio_full$date," ", bio_full$time), format = "%Y-%m-%d %H:%M:%S"), tz = tz)#create local datetime variable
  
  
  #Normalize Acoustic data, using both min-max score and z score within sites
  bio_full = bio_full %>%
    dplyr::filter(is.na(left_channel)==FALSE)%>%
    mutate(bio_obs = left_channel,
           bio_scaled = scale(left_channel))
  # mutate(sitebio_minmax = (left_channel-min(left_channel))/(max(left_channel)-min(left_channel)),
  #        sitebio_zscore = (left_channel-(mean(left_channel))/sd(left_channel)))
  
  if(x == "kiowa"){
    kiowa_time_bad = bio_full %>% dplyr::filter(hour ==12)%>%
      mutate(local_time = local_time-3600
             # ,
             # time = as_hms(local_time),
             # hour = hour(local_time)
      )
    kiowa_time_good = bio_full %>% dplyr::filter(hour !=12)
    bio_full = rbind(kiowa_time_bad,kiowa_time_good) %>%
      dplyr::filter(year == 2021)
    
  } else {
    
  }
  bio_full$date_time = as_datetime(bio_full$local_time, tz = "UTC")
  bio_fullw = left_join(bio_full, wd, by=c("date_time"))%>%
    rename(gh_obs = "gh")
  
  bio_fullw$ghobs_scaledwin = scale(bio_fullw$gh_obs)
  
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  bio_fullw = bio_fullw %>% mutate(month_day = format(as.Date(date), "%m-%d"))%>%
    rename(hour_utc = "hour.y",
           min_as = "mas")%>%
    mutate(mas = cut(min_as, seq(-725,760,5),labels = labels, right = FALSE))
  
  # hd = hd %>% dplyr::filter(is.na(gh_obs)==FALSE)
  bio_fullwh = full_join(bio_fullw, hd, na_matches = "na") %>%
    arrange(month_day,mas)#need to do a full combine and then approximate missing aridity values, also need to extend the mesonet date to 09/01, utc time cuts out some of the 08/31/21 data?
  
  bio_fullwh$gh_hobs = na.approx(bio_fullwh$gh_hobs, na.rm = FALSE)
  bio_fullwh$ghhobs_scaled = na.approx(bio_fullwh$ghhobs_scaled, na.rm = FALSE)
  bio_fullwh$ghmean_time = na.approx(bio_fullwh$ghmean_time, na.rm = FALSE)
  bio_fullwh$ghsite_scaled = na.approx(bio_fullwh$ghsite_scaled, na.rm = FALSE)
  
  bio_missing = bio_fullwh %>% dplyr::filter(is.na(gh_hobs)==TRUE) #see which dates are missing aridity values
  bio_fullwh2 = bio_fullwh %>% 
    dplyr::filter(is.na(gh_hobs)==FALSE) %>%
    dplyr::filter(is.na(left_channel)==FALSE)
  
  bio_missing = bio_fullwh2 %>% dplyr::filter(is.na(left_channel)==TRUE) #see which dates are missing aridity values
  
  return(bio_fullwh2)
  # return(bio_full)
})

lwma_bio = bio[[1]]
sswma_bio = bio[[2]]
cbma_bio = bio[[3]]
kiowa_bio = bio[[4]]


bio_full = rbind(lwma_bio,sswma_bio,cbma_bio,kiowa_bio) %>%
  dplyr::select(-c(site.y))%>%
  rename(site = "site.x",
         hour = "hour.x")%>%
  mutate(site = factor(site, levels=c("lwma","sswma","cbma","kiowa")),
         hour_utc = hour(date_time))

bio_acrsite = bio_full %>%
  # mutate(month_day = format(as.Date(date), "%m-%d"))%>%
  mutate(stgh_across = scale(gh_obs))%>%
  group_by(month_day,mas)%>%
  summarise(n = n(),
            mean_ghacross = mean(gh_obs),
            sd_ghacross = sd(gh_obs),
            se_ghacross = (sd(gh_obs)/sqrt(n())),
            stmean_ghacross = mean(stgh_across),
            stsd_ghacross = sd(stgh_across),
            stse_ghacross = (sd(stgh_across)/sqrt(n())))

bio_full = left_join(bio_full,bio_acrsite, by = c("month_day","mas"))

bio_missing = bio_full %>% dplyr::filter(is.na(mas)==TRUE)

bio_full = bio_full %>%
  dplyr::filter(is.na(mas) == FALSE)

bio_hour = bio_full %>%
  dplyr::filter(is.na(left_channel)==FALSE) %>%
  # mutate(hour = hour(date_time),
  #        site = "lwma",
  #        dew = TAIR-((100-RELH)/5),
  #        arid = abs((1/dew)),
  #        mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))))%>%
  mutate(mas2 = as.numeric(as.character(mas)))%>%
  group_by(site,month_day,mas2) %>%
  summarize(mean_bio = mean(left_channel),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(abs(arid)),
            # mean_mas = mean(mas),
            mean_sunalt = mean(altitude),
            mean_gh = mean(ghsite_scaled))
ggplot(data = bio_hour, aes(x = mas2, y = mean_gh, color = site))+
  geom_line()

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(bio_full, file = "bio_full.Rdata")
