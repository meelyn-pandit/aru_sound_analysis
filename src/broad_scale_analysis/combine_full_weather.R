library(soundecology) #obtain broad scale acoustic metrics
library(tuneR) #loading and reading sound files
library(seewave) #soundwave analysis
library(osfr) #downloading files from osf
library(dplyr) #data management and conversion
library(lubridate) #convert date types
library(lme4) #linear mixed models
library(lmerTest)
library(okmesonet)
library(ggplot2) #create good graphs
library(extrafont)
library(lsmeans)
library(pbapply)
library(zoo)


setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/cbma_arus_raw/")
# setwd("/Volumes/LaCie/Aridity Project/cbma/")
setwd("/Volumes/LaCie/aridity_project/sswma/sswma_audio_files/")

# Load Weather data -------------------------------------------------------
# weather_data = cbma_data
# weather_data = kiowa_data
# weather_data = sswma_data

# site = c("cbma", "kiowa","lwma","sswma")
site = "sswma"


metric = c("aci","adi","aei","bio")


# aru = c("aru01","aru02","aru03","aru04","aru05")
# aru = c("wg01","wg02","wg03","wg04","wg05")
aru = c("ws01","ws02","ws03","ws04","ws05","ws06","ws07","ws08","ws09","ws10","ws11","ws12","ws13","ws14","ws15")
for(m in metric){
  metric_weather_full2 = NULL
  
  for(a in aru){
  #   if(s == "kiowa"){
  #     weather_data = kiowa_data
  #   } else if(s == "lwma"){
  #     weather_data = lwma_data
  #   } else if(s == "sswma"){
  #     weather_data = sswma_data
  #   } else if(s == "cbma"){
  #     weather_data == cbma_data
  #   }
  weather_data = sswma_data
    # setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/cbma_arus_raw/"))
    
    # setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",s,"_arus_raw/"))
    metric_file = read.csv(paste0(m,"_results_raw_",a,".csv"),header = TRUE)
    names(metric_file) = tolower(names(metric_file))
    metric_file = metric_file %>%
      mutate(year = as.factor((substr(metric_file$filename, 1,4))), #obtain year from AudioMoth recording filename
             month = as.factor((substr(metric_file$filename,5,6))),
             day = as.factor((substr(metric_file$filename, 7,8))),
             hour = as.factor(substr(metric_file$filename, 10,11)),
             min = as.factor(substr(metric_file$filename, 12,13)),
             second = as.factor(substr(metric_file$filename, 14,15)),
             date = as.character(paste0(year,"-",month,"-",day)),
             time = as.character(paste0(hour,":",min,":",second)))
    metric_file$local_time = as.POSIXct(as.character(paste0(metric_file$date," ", metric_file$time), format = "%Y-%m-%d %H:%M:%S"))#create datetime variable
    
    metric_weather = left_join(metric_file, weather_data, by = "local_time")
    
    metric_weather_full = metric_weather %>% arrange(aru, local_time) %>%
      mutate(temperature = na.approx(temperature, na.rm = FALSE),
             humidity = na.approx(humidity, na.rm = FALSE),
             pressure = na.approx(pressure, na.rm = FALSE),
             windspeed = na.approx(windspeed, na.rm = FALSE),
             winddir = na.approx(winddir, na.rm = FALSE),
             emissivity = na.approx(emissivity, na.rm = FALSE),
             cloudcover = na.approx(cloudcover, na.rm = FALSE),
             netlong = na.approx(netlong, na.rm = FALSE),
             uplong = na.approx(uplong, na.rm = FALSE),
             downlong = na.approx(downlong, na.rm = FALSE),
             rad_dni = na.approx(rad_dni, na.rm = FALSE),
             rad_dif = na.approx(rad_dif, na.rm = FALSE),
             szenith = na.approx(szenith, na.rm = FALSE),
             relh = na.approx(relh, na.rm = FALSE))
    
    metric_weather_full2 = rbind(metric_weather_full,metric_weather_full2)
    write.csv(metric_weather_full2, paste0("sswma_ws_",m,"_full_weather.csv"), row.names = FALSE)
    save(metric_weather_full2, file = paste0("sswma_ws_",m,"_full_weather.Rdata"))
  }

}
  






aci_cbma_full = read.csv("aci_cbma_full.csv", header = TRUE)
names(aci_cbma_full) = tolower(names(aci_cbma_full))
# names(aci_cbma_full) = c("filename","sampling_rate","bit","duration","channels","index","fft_w","min_freq","max_freq", "j", "left_channel","right_channel","aru","site") #renaming columns
aci_cbma_full$year = as.factor((substr(aci_cbma_full$filename, 1,4))) #obtain year from AudioMoth recording filename
aci_cbma_full$month = as.factor((substr(aci_cbma_full$filename,5,6))) #obtain month from AudioMoth recording filename
aci_cbma_full$day = as.factor((substr(aci_cbma_full$filename, 7,8))) #obtain day from AudioMoth recording filename
aci_cbma_full$hour = as.factor(substr(aci_cbma_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
aci_cbma_full$min = as.factor(substr(aci_cbma_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
aci_cbma_full$second = as.factor(substr(aci_cbma_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
aci_cbma_full$date = as.character(paste0(aci_cbma_full$year,"-",aci_cbma_full$month,"-",aci_cbma_full$day)) #combine year, month, and day to get date
aci_cbma_full$time = as.character(paste0(aci_cbma_full$hour,":",aci_cbma_full$min,":",aci_cbma_full$second)) #combine hour, minute, and seconds to get time
aci_cbma_full$local_time = as.POSIXct(as.character(paste0(aci_cbma_full$date," ", aci_cbma_full$time), format = "%Y-%m-%d %H:%M:%S"))#create datetime variable

aci_cbma_weather = left_join(aci_cbma_full, weather_data, by = "local_time")
# missing = anti_join(aci_cbma_full,cbma_data, by = "local_time")
# 
# combined = bind_rows(aci_cbma_weather,missing) %>%
#   arrange(aru, local_time)
aci_cbma_weather = aci_cbma_weather %>% arrange(aru, local_time) %>%
  mutate(temperature = na.approx(temperature, na.rm = FALSE),
         humidity = na.approx(humidity, na.rm = FALSE),
         pressure = na.approx(pressure, na.rm = FALSE),
         windspeed = na.approx(windspeed, na.rm = FALSE),
         winddir = na.approx(winddir, na.rm = FALSE),
         emissivity = na.approx(emissivity, na.rm = FALSE),
         cloudcover = na.approx(cloudcover, na.rm = FALSE),
         netlong = na.approx(netlong, na.rm = FALSE),
         uplong = na.approx(uplong, na.rm = FALSE),
         downlong = na.approx(downlong, na.rm = FALSE),
         rad_dni = na.approx(rad_dni, na.rm = FALSE),
         rad_dif = na.approx(rad_dif, na.rm = FALSE),
         szenith = na.approx(szenith, na.rm = FALSE),
         relh = na.approx(relh, na.rm = FALSE))

write.csv(aci_cbma_weather, "cbma_wlg_aci_full_weather.csv", row.names = FALSE)
save(aci_cbma_weather, file = "cbma_wlg_aci_full_weather.Rdata")


# Acoustic Diversity Index ------------------------------------------------

adi_cbma_full = read.csv("adi_cbma_full.csv", header = TRUE)
names(adi_cbma_full) = tolower(names(adi_cbma_full))
# names(adi_cbma_full) = c("filename","sampling_rate","bit","duration","channels","index","max_freq","db_threshold","freq_steps","left_channel","right_channel","aru","site") #renaming columns
adi_cbma_full$year = as.factor((substr(adi_cbma_full$filename, 1,4))) #obtain year from AudioMoth recording filename
adi_cbma_full$month = as.factor((substr(adi_cbma_full$filename,5,6))) #obtain month from AudioMoth recording filename
adi_cbma_full$day = as.factor((substr(adi_cbma_full$filename, 7,8))) #obtain day from AudioMoth recording filename
adi_cbma_full$hour = as.factor(substr(adi_cbma_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
adi_cbma_full$min = as.factor(substr(adi_cbma_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
adi_cbma_full$second = as.factor(substr(adi_cbma_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
adi_cbma_full$date = as.character(paste0(adi_cbma_full$year,"-",adi_cbma_full$month,"-",adi_cbma_full$day)) #combine year, month, and day to get date
adi_cbma_full$time = as.character(paste0(adi_cbma_full$hour,":",adi_cbma_full$min,":",adi_cbma_full$second)) #combine hour, minute, and seconds to get time
adi_cbma_full$local_time = as.POSIXct(as.character(paste0(adi_cbma_full$date," ", adi_cbma_full$time), format = "%Y-%m-%d %H:%M:%S"))#create datetime variable

adi_cbma_weather = left_join(adi_cbma_full, weather_data, by = "local_time")
# missing = anti_join(adi_cbma_full,cbma_data, by = "local_time")
# 
# combined = bind_rows(adi_cbma_weather,missing) %>%
#   arrange(aru, local_time)
adi_cbma_weather = adi_cbma_weather %>% arrange(aru, local_time) %>%
  mutate(temperature = na.approx(temperature, na.rm = FALSE),
         humidity = na.approx(humidity, na.rm = FALSE),
         pressure = na.approx(pressure, na.rm = FALSE),
         windspeed = na.approx(windspeed, na.rm = FALSE),
         winddir = na.approx(winddir, na.rm = FALSE),
         emissivity = na.approx(emissivity, na.rm = FALSE),
         cloudcover = na.approx(cloudcover, na.rm = FALSE),
         netlong = na.approx(netlong, na.rm = FALSE),
         uplong = na.approx(uplong, na.rm = FALSE),
         downlong = na.approx(downlong, na.rm = FALSE),
         rad_dni = na.approx(rad_dni, na.rm = FALSE),
         rad_dif = na.approx(rad_dif, na.rm = FALSE),
         szenith = na.approx(szenith, na.rm = FALSE),
         relh = na.approx(relh, na.rm = FALSE))

write.csv(adi_cbma_weather, "cbma_adi_full_weather.csv", row.names = FALSE)
save(adi_cbma_weather, file = "cbma_adi_full_weather.Rdata")


# Acoustic Evenness Index -------------------------------------------------

aei_cbma_full = read.csv("aei_cbma_full.csv", header = TRUE)
names(aei_cbma_full) = tolower(names(aei_cbma_full))
# names(aei_cbma_full) = c("filename","sampling_rate","bit","duration","channels","index","max_freq","db_threshold","freq_steps","left_channel","right_channel","aru","site") #renaming columns
aei_cbma_full$year = as.factor((substr(aei_cbma_full$filename, 1,4))) #obtain year from AudioMoth recording filename
aei_cbma_full$month = as.factor((substr(aei_cbma_full$filename,5,6))) #obtain month from AudioMoth recording filename
aei_cbma_full$day = as.factor((substr(aei_cbma_full$filename, 7,8))) #obtain day from AudioMoth recording filename
aei_cbma_full$hour = as.factor(substr(aei_cbma_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
aei_cbma_full$min = as.factor(substr(aei_cbma_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
aei_cbma_full$second = as.factor(substr(aei_cbma_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
aei_cbma_full$date = as.character(paste0(aei_cbma_full$year,"-",aei_cbma_full$month,"-",aei_cbma_full$day)) #combine year, month, and day to get date
aei_cbma_full$time = as.character(paste0(aei_cbma_full$hour,":",aei_cbma_full$min,":",aei_cbma_full$second)) #combine hour, minute, and seconds to get time
aei_cbma_full$local_time = as.POSIXct(as.character(paste0(aei_cbma_full$date," ", aei_cbma_full$time), format = "%Y-%m-%d %H:%M:%S"))#create datetime variable

aei_cbma_weather = left_join(aei_cbma_full, weather_data, by = "local_time")
# missing = anti_join(aei_cbma_full,cbma_data, by = "local_time")
# 
# combined = bind_rows(aei_cbma_weather,missing) %>%
#   arrange(aru, local_time)
aei_cbma_weather = aei_cbma_weather %>% arrange(aru, local_time) %>%
  mutate(temperature = na.approx(temperature, na.rm = FALSE),
         humidity = na.approx(humidity, na.rm = FALSE),
         pressure = na.approx(pressure, na.rm = FALSE),
         windspeed = na.approx(windspeed, na.rm = FALSE),
         winddir = na.approx(winddir, na.rm = FALSE),
         emissivity = na.approx(emissivity, na.rm = FALSE),
         cloudcover = na.approx(cloudcover, na.rm = FALSE),
         netlong = na.approx(netlong, na.rm = FALSE),
         uplong = na.approx(uplong, na.rm = FALSE),
         downlong = na.approx(downlong, na.rm = FALSE),
         rad_dni = na.approx(rad_dni, na.rm = FALSE),
         rad_dif = na.approx(rad_dif, na.rm = FALSE),
         szenith = na.approx(szenith, na.rm = FALSE),
         relh = na.approx(relh, na.rm = FALSE))

write.csv(aei_cbma_weather, "cbma_aei_full_weather.csv", row.names = FALSE)
save(aei_cbma_weather, file = "cbma_aei_full_weather.Rdata")

# Biodiversity Indicie ----------------------------------------------------

bio_cbma_full = read.csv("bio_cbma_full.csv", header = TRUE)
# names(bio_cbma_full) = c("filename","sampling_rate","bit","duration","channels","index","fft_w","min_freq","max_freq", "left_channel","right_channel","aru","site") #renaming columns
names(bio_cbma_full) = tolower(names(bio_cbma_full))
bio_cbma_full$year = as.factor((substr(bio_cbma_full$filename, 1,4))) #obtain year from AudioMoth recording filename
bio_cbma_full$month = as.factor((substr(bio_cbma_full$filename,5,6))) #obtain month from AudioMoth recording filename
bio_cbma_full$day = as.factor((substr(bio_cbma_full$filename, 7,8))) #obtain day from AudioMoth recording filename
bio_cbma_full$hour = as.factor(substr(bio_cbma_full$filename, 10,11)) #obtain hour from AudioMoth recording filename
bio_cbma_full$min = as.factor(substr(bio_cbma_full$filename, 12,13)) #obtain minute from AudioMoth recording filename
bio_cbma_full$second = as.factor(substr(bio_cbma_full$filename, 14,15)) #obtain seconds from AudioMoth recording filename
bio_cbma_full$date = as.character(paste0(bio_cbma_full$year,"-",bio_cbma_full$month,"-",bio_cbma_full$day)) #combine year, month, and day to get date
bio_cbma_full$time = as.character(paste0(bio_cbma_full$hour,":",bio_cbma_full$min,":",bio_cbma_full$second)) #combine hour, minute, and seconds to get time
bio_cbma_full$local_time = as.POSIXct(as.character(paste0(bio_cbma_full$date," ", bio_cbma_full$time), format = "%Y-%m-%d %H:%M:%S"))#create datetime variable

bio_cbma_weather = left_join(bio_cbma_full, weather_data, by = "local_time")
# missing = anti_join(bio_cbma_full,cbma_data, by = "local_time")
# 
# combined = bind_rows(bio_cbma_weather,missing) %>%
#   arrange(aru, local_time)
bio_cbma_weather = bio_cbma_weather %>% arrange(aru, local_time) %>%
  mutate(temperature = na.approx(temperature, na.rm = FALSE),
         humidity = na.approx(humidity, na.rm = FALSE),
         pressure = na.approx(pressure, na.rm = FALSE),
         windspeed = na.approx(windspeed, na.rm = FALSE),
         winddir = na.approx(winddir, na.rm = FALSE),
         emissivity = na.approx(emissivity, na.rm = FALSE),
         cloudcover = na.approx(cloudcover, na.rm = FALSE),
         netlong = na.approx(netlong, na.rm = FALSE),
         uplong = na.approx(uplong, na.rm = FALSE),
         downlong = na.approx(downlong, na.rm = FALSE),
         rad_dni = na.approx(rad_dni, na.rm = FALSE),
         rad_dif = na.approx(rad_dif, na.rm = FALSE),
         szenith = na.approx(szenith, na.rm = FALSE),
         relh = na.approx(relh, na.rm = FALSE))

write.csv(bio_cbma_weather, "cbma_bio_full_weather.csv", row.names = FALSE)
save(bio_cbma_weather, file = "cbma_bio_full_weather.Rdata")
