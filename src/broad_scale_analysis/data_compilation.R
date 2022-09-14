library(dplyr) #data manipulation
library(tibble) #data manipulation
library(ggplot2) #graphs
library(lme4) #lmm and glmm
library(lmerTest) #p-values for lmms
library(bbmle) #AIC tests
library(lubridate) #date and time manipulation
library(car) #Anova() function
library(multcomp) #posthoc tests for ANOVA type III effects
library(zoo) #used to interpolate missing data

#change list to names of your directories
# dir = as.list(c("cbma_acoustic_metrics","kiowa_acoustic_metrics","lwma_acoustic_metrics","sswma_aru_acoustic_metrics"))
# dir = as.list(c("b1","b2","b3","b4","b5","g1","g2","o1","o2","o3","o4","p1","p2","p3","p4"))

# Compile SSWMA and CBMA Water Station Data -------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/water_supplementation_data_clean/")
metric_list = as.list(c("aci","bio"))

lapply(metric_list, function(x){
  metric = list.files(pattern = paste0(x,"_full_weather.Rdata"))
  load(metric[1])
  cbma_wg = metric_weather_full2
  
  cbma_wg$date = as_date(cbma_wg$date)
  
  cbma_wg1 = cbma_wg %>%
    filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
    mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
           ws_site = 1,
           site = "cbma")
  
  cbma_wg2 = cbma_wg %>%
    filter(aru == "wg04" | aru == "wg05") %>%
    mutate(water = 1,
           ws_site = 2,
           site = "cbma")

  cbma_wg_full = rbind(cbma_wg1, cbma_wg2)
  
  # #uncovered = access to water = 1. covered = no access to water = 0
  # 
  # 
  # #uncomment below for Acoustic diversity index and acoustic eveness index
  # cbma_water_aei = cbma_water_aei %>%
  #   rename(db_threshold = min_freq) %>%
  #   rename(min_freq = max_freq)
  # cbma_water_aei = cbma_water_aei %>% filter(db_threshold == 50)
  # cbma_water_aei$water = cbma_water_aei$water[,40] #change to 41 for acoustic complexity index, other indices use 40
  load(metric[2])
  sswma_ws = metric_weather_full2
  sswma_ws$date = as_date(sswma_ws$date)
  
  sswma_ws1 = sswma_ws %>%
    filter(aru == "ws01"| aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05")%>%
    mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0),
           ws_site = 1,
           site = "sswma")

  sswma_ws2 = sswma_ws %>%
    filter(aru == "ws06"| aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10") %>%
    mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0),
           ws_site = 2,
           site = "sswma")

  sswma_ws3 = sswma_ws %>%
    filter(aru == "ws11"| aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15") %>%
    mutate(water = 0,
           ws_site = 3,
           site = "sswma")

  sswma_ws_full = rbind(sswma_ws1, sswma_ws2, sswma_ws3)
  water_supp = rbind(cbma_wg_full,sswma_ws_full)
   
 
  
  
  write.csv(water_supp, paste0("water_sup_",x,".csv"), row.names = FALSE)
  save(water_supp, file = paste0("water_sup_",x,".Rdata"))
  
  
})


# Adding Water Schedule to Water Supplementation datasets -----------------------------------


# Upload Aridity Gradient Datasets ----------------------------------------

# Use this to compile data that is organized in multiple directori --------

aei = lapply(dir, function(x){
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/sitka_acoustic_metrics/",x,sep=""))
  aru = substr(x,1,2) #change substring length to what your site names are
  aei_files = list.files(pattern = "aei") %>%
    lapply(read.csv) %>%
    bind_rows() %>%
    add_column(aru = aru,
               site = "sitka")
})
aei_results = bind_rows(aei)
write.csv(aei_results, paste0("aei_",aei_results$site[1],"_raw.csv", sep = ""), row.names = FALSE)

# #Use this to compile data that is in one directory ----------------------
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/kiowa_arus/")
# setwd("D:/Aridity Project/lwma/lwma_acoustic_metrics_raw/")
# setwd("D:/Aridity Project/sswma/sswma_acoustic_metrics_aridity_gradient_raw/")
# site = c("kiowa","lwma","sswma")
site = "sswma"
metric = c("aci","adi","aei","bio")
for(s in site){
  for(m in metric){
    setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",s,"_arus_raw/"))
    metric_list = as.list(list.files(pattern = paste0(m,"_results_raw")))
    for(i in metric_list){
      metric_results = bind_rows(metric_combined)
      write.csv(metric_results, paste0(m,"_",metric_results$site[1],"_full.csv", sep = ""), row.names = FALSE)
    }
      # read.csv(x,header = TRUE) %>%
      #   add_column( #aru = substr(x,1,5),
      #              site = s) 

    
  }
}    
# setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",site,"_arus_raw/",sep = ""))
# setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",site,"_ws_raw/",sep = ""))
# setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/broad_acoustic_data/",site,"_wlg_raw/",sep = ""))

metric = c("aci","adi","aei","bio")
setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/",m,"_full_all_sites/"))
aci_files = as.list(list.files(pattern = "aci"))
aci = lapply(aci_files, function(x){
  read.csv(x,header = TRUE) %>%
    add_column(aru = substr(x,1,5),
               site = site) 
}) 
aci_results = bind_rows(aci)
write.csv(aci_results, paste0("aci_all_sites_combined",aci_results$site[1],".csv", sep = ""), row.names = FALSE)

adi_files = as.list(list.files(pattern = "adi"))
adi = lapply(adi_files, function(x){
  read.csv(x,header = TRUE) %>%
    add_column(aru = substr(x,1,5),
               site = site) 
}) 

adi_results = bind_rows(adi)
write.csv(adi_results, paste0("adi_wlg",adi_results$site[1],".csv", sep = ""), row.names = FALSE)

aei_files = as.list(list.files(pattern = "aei"))
aei = lapply(aei_files, function(x){
  read.csv(x,header = TRUE) %>%
    add_column(aru = substr(x,1,5),
               site = site) 
}) 

aei_results = bind_rows(aei)
write.csv(aei_results, paste0("aei_wlg",aei_results$site[1],".csv", sep = ""), row.names = FALSE)

bio_files = as.list(list.files(pattern = "bio"))
bio = lapply(bio_files, function(x){
  read.csv(x,header = TRUE) %>%
  add_column(aru = substr(x,1,5),
  site = site) 
}) 

bio_results = bind_rows(bio)
write.csv(bio_results, paste0("bio_wlg",bio_results$site[1],".csv", sep = ""), row.names = FALSE)


# Combining all sites per metric together ---------------------------------

metric = c("aci","adi","aei","bio")
setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/bio_full_all_sites"))
metric_list = as.list(list.files(pattern = ".Rdata"))

load(metric_list[[1]])
cbma = metric_weather_full 
cbma = cbma %>% dplyr::select(-date_time)

load(metric_list[[2]])
kiowa = metric_weather_full

load(metric_list[[3]])
lwma = metric_weather_full

load(metric_list[[4]])
sswma = metric_weather_full


bio_all_sites_full_weather = rbind(cbma,kiowa,lwma,sswma)
save(bio_all_sites_full_weather, file = "bio_all_sites_full_weather.Rdata")















bio_all_sites = rbind(bio[[1]],bio[[2]],bio[[3]],bio[[4]],bio[[5]],
                      bio[[6]],bio[[7]],bio[[8]],bio[[9]],bio[[10]],
                      bio[[11]],bio[[12]],bio[[13]],bio[[14]],bio[[15]])
# bio_water = bio[[1]]
# bio_water = bio_water[,-c(23:38)]
# write.csv(bio_all_sites,"bio_all_sites.csv", row.names = FALSE)

bio_all_sites$date_time = as_datetime(bio_all_sites$date_time, tz = "US/Pacific")
bio_all_sites$date_time_utc = as_datetime(bio_all_sites$date_time, tz = "UTC")
bio_all_sites = merge(bio_all_sites, weather_data2, by.x = "date_time", by.y = "local_time")
bio_all_sites$dew_temp = bio_all_sites$temperature-((100-bio_all_sites$relh)/5)
#uncomment below for bio and bio
bio_all_sites = bio_all_sites %>% filter(db_threshold == 50)
bio_all_sites = bio_all_sites %>%
  rename(db_threshold = min_freq) %>%
  rename(min_freq = max_freq)
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/sitka_acoustic_metrics/")
write.csv(bio_all_sites, "bio_all_sites.csv", row.names = FALSE)

save(bio_all_sites, file = "bio_all_sites.Rdata")


# Upload CBMA Wildlife Guzzler Sites --------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/cbma_wildlife_guzzlers/")
dir = as.list(c("wg01","wg02","wg03","wg04","wg05"))
aei = lapply(dir, function(x){
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/cbma_wildlife_guzzlers/",x,sep=""))
  site = substr(x,1,4) #change substring length to what your site names are
  aei_files = list.files(pattern = "aei") %>%
    lapply(read.csv) %>%
    bind_rows()
})

cbma_water_aei = rbind(aei[[1]],aei[[2]],aei[[3]],aei[[4]],aei[[5]])
cbma_water_aei$date_time = as_datetime(cbma_water_aei$date_time, tz = "US/Central")
cbma_water_aei$date_time_utc = as_datetime(cbma_water_aei$date_time, tz = "UTC")
cbma_water_aei$dew_temp = cbma_water_aei$temperature-((100-cbma_water_aei$relh)/5)

#uncovered = access to water = 1. covered = no access to water = 0
cbma_water_aei1 = cbma_water_aei %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03")
cbma_water_aei1$water = ifelse(cbma_water_aei1$date >= "2021-06-04" & cbma_water_aei1$date <"2021-06-25"| cbma_water_aei1$date >= "2021-07-19" & cbma_water_aei1 < "2021-08-02", 0,1)

cbma_water_aei2 = cbma_water_aei %>%
  filter(aru == "wg04" | aru == "wg05")
cbma_water_aei2$water =  1


cbma_water_aei = rbind(cbma_water_aei1, cbma_water_aei2)
cbma_water_aei$water = cbma_water_aei$water[,40] #change to 41 for acoustic complexity index, other indices use 40

#uncomment below for Acoustic diversity index and acoustic eveness index
cbma_water_aei = cbma_water_aei %>%
  rename(db_threshold = min_freq) %>%
  rename(min_freq = max_freq)
cbma_water_aei = cbma_water_aei %>% filter(db_threshold == 50)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/cbma_wildlife_guzzlers/")
write.csv(cbma_water_aei, "aei_water_cbma.csv", row.names = FALSE)
save(cbma_water_aei, file = "aei_water_cbma.Rdata")



# Dataframe Editing -------------------------------------------------------
adi_all_sites = read.csv("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/adi_all_sites.csv", header = TRUE)

adi_all_sites$site = recode_factor(adi_all_sites$site, lwma = "lwma", kiow = "kiowa", cbma = "cbma", sswm = "sswma")

adi_all_sites$site = factor(adi_all_sites$site, levels=c("lwma","sswma","cbma","kiowa"))
adi_all_sites$dew_temp = adi_all_sites$temperature-((100-adi_all_sites$relh)/5)
adi_all_sites$date_time_utc = as_datetime(adi_all_sites$obs_time, tz = "UTC")

# Correct Kiowa Times on 06/17 and 06/18 --------------------------------------------
kiowa_bad = aci_all_sites %>%
  filter(site == "kiowa") %>%
  filter(date == "2021-06-17"| date == "2021-06-18") %>%
  filter(aru == "aru03")

aci_minus = anti_join(aci_all_sites,kiowa_bad)

kiowa_bad$date_time_utc = kiowa_bad$date_time_utc-3600 #minus 1 hour or 3600s
aci_all_sites = rbind(aci_minus,kiowa_bad)

aci_all_sites$time_utc = hms::as_hms(substr(aci_all_sites$date_time_utc,12,19))
aci_all_sites$week = week(aci_all_sites$date_time_utc)


# Adding Water Station Data and Switching Dates ---------------------------
sswma_water_aci = read.csv("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/sswma_water_station_acoustic_metrics/compiled_data/aci_all_water_sswma.csv", header = TRUE)

sswma_water_aci$date = ymd(sswma_water_aci$date)
sswma_water_aci$time = hms(sswma_water_aci$time)

sswma_water_aci1 = sswma_water_aci %>%
  filter(site == 1)
sswma_water_aci1$water = ifelse(sswma_water_aci1$date >= "2021-05-17" & sswma_water_aci1$date <"2021-05-30"| sswma_water_aci1$date >= "2021-06-13" & sswma_water_aci1 < "2021-07-02", 1,0)

sswma_water_aci2 = sswma_water_aci %>%
  filter(site == 2)
sswma_water_aci2$water = ifelse(sswma_water_aci2$date >= "2021-05-30" & sswma_water_aci2$date <"2021-06-12"| sswma_water_aci2$date >= "2021-07-03" & sswma_water_aci2 < "2021-08-07", 1,0)

sswma_water_aci3 = sswma_water_aci %>%
  filter(site == 3)
sswma_water_aci3$water = 0

sswma_water_aci = rbind(sswma_water_aci1, sswma_water_aci2, sswma_water_aci3)
sswma_water_aci$water = sswma_water_aci$water[,40]

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/sswma_water_station_acoustic_metrics/compiled_data")
write.csv(sswma_water_bio, "aci_all_water_sswma.csv", row.names = FALSE)
# aci_all_sites$date_time3 = ifelse(aci_all_sites$site == "kiowa", as_datetime(aci_all_sites$date_time, tz="America/Denver"), as_datetime(aci_all_sites$date_time, tz="America/Chicago"))

# aci_all_sites$date_time_utc = as_datetime(aci_all_sites$date_time3, tz = "UTC")
# aci_all_sites$time_utc = hms::as_hms(substr(aci_all_sites$date_time_utc,12,19))


# Creating aridity variable - Residuals from Temp x Relh ------------------

plot(sswma_water_aci$temperature, sswma_water_aci$relh)
regLine(lm(relh ~ temperature, data = sswma_water_aci))

sswma_water_aci$arid = residuals(lm(relh ~ temperature, data = sswma_water_aci))
sswma_water_aci$SHD = interaction(scale(sswma_water_aci$date),sswma_water_aci$arid, sswma_water_aci$water)


m1 = lmer(left_channel ~ arid*time@hour*water + (1|site), data = sswma_water_aci, REML = TRUE)
summary(m1)

sswma_water_aci$SHD = as.numeric(interaction(sswma_water_aci$temperature,sswma_water_aci$relh))
m2 = lmer(left_channel ~ scale(SHD)*time@hour*water + (1|site), data = sswma_water_aci, REML = TRUE)
summary(m2)

m3 = lmer(left_channel ~ dew_temp*time@hour*water + (1|site), data = sswma_water_aci, REML = TRUE)

m4 = lmer(left_channel ~ relh*time@hour*water + (1|site), data = sswma_water_aci, REML = TRUE)
summary(m4)

m5 = lmer(left_channel ~ relh*time@hour + temperature*time@hour + water*time@hour + (1|site), data = sswma_water_aci, REML = FALSE)
summary(m5)
AICctab(m1,m2,m3,m4, nobs = 5743, weights = TRUE, delta = TRUE)

m1 = lmer(left_channel ~ arid*time@hour*water + (1|site), data = sswma_water_aci, REML = FALSE)
summary(m1)
Anova(m1, type ="III")
contrasts = glht(m1, linfct = mcp(site = "Tukey"))
summary(contrasts)

AICctab(m0,m1,m2,m3,m4,m5, nobs = 9207, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m5)
Anova(m1, type = "III")
# m5 = lm(left_channel ~ SHD, data = aci_all_sites)
# aci_all_sites$SHD = interaction(scale(aci_all_sites$time_utc),aci_all_sites$relh,aci_all_sites$site)

aci_means = aci_all_sites %>%
  filter(is.na(left_channel) == FALSE) %>%
  group_by(site,week) %>%
  summarize(aciMean = mean(left_channel),
            mean_temp = mean(temperature),
            mean_relh = mean(relh),
            mean_dew = mean(dew_temp))


# Means across time -------------------------------------------------------
###Relative Humidity
ggplot(data = aci_means,aes(x =week, y = aciMean, color=mean_relh)) +
  geom_point(size = 5, aes(shape = site))+
  scale_shape_manual(name = "Site", labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"), values = c(15,16,17,18))+
  geom_line(aes(group = site))+
  scale_color_viridis_c(name = "Relative\nHumidity %",direction = -1)+
  # scale_x_time(name = "Time (CDT)", labels = c("05:00:00", "07:00:00", "09:00:00", "11:00:00", "13:00:00")) +
  scale_x_continuous(name = "Week", labels = c("May", "June", "July", "August"))+
  scale_y_continuous(name = "acidiversity Index"
                     # , limits = c(0,0.5)
                     )+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results")
ggsave("aci_acoustic_metric_time.jpg", dpi=300, height=6, width=12, units="in")

###Dewpoint Temperature
ggplot(data = aci_means,aes(x =week, y = aciMean, color=mean_dew)) +
  geom_point(size = 5, aes(shape = site))+
  scale_shape_manual(name = "Site", labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"), values = c(15,16,17,18))+
  geom_line(aes(group = site))+
  scale_color_viridis_c(name = "Dewpoint\nTemperature (°C)",direction = -1)+
  # scale_x_time(name = "Time (CDT)", labels = c("05:00:00", "07:00:00", "09:00:00", "11:00:00", "13:00:00")) +
  scale_x_continuous(name = "Month", labels = c("May", "June", "July", "August"))+
  scale_y_continuous(name = "Acoustic Complexity Index"
                     # , limits = c(0,0.5)
  )+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results")
ggsave("aci_acoustic_metric_week_dew.jpg", dpi=300, height=6, width=12, units="in")
# Facet Wrap --------------------------------------------------------------
ggplot(data = aci_means,aes(x =mean_relh, y = aciMean)) +
  geom_point(size = 5, aes(shape = site))+
  scale_shape_manual(name = "Site", labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"), values = c(15,16,17,18))+
  # scale_color_viridis_c(name = "Dewpoint\nTemperature(C) %",direction = -1)+
  # scale_x_time(name = "Time (UTC)", labels = c("05:00:00", "07:00:00", "09:00:00", "11:00:00", "13:00:00")) +
  scale_y_continuous(name = "acidiversity Index")+
  facet_grid(rows = vars(site)) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results")
ggsave("aci_acoustic_metric_time.jpg", dpi=300, height=6, width=12, units="in")


# Mean bar graph ----------------------------------------------------------
ggplot(data = aci_means,
       aes(x = time_utc, y = aciMean, fill = site))+
  geom_bar(stat = "identity", position=position_dodge())


# Acoustic Indices across Full 2 months -----------------------------------
aci_means = aci_all_sites %>%
#   filter(is.na(left_channel) == FALSE) %>%
  group_by(site,date_time_utc) %>%
  summarize(aciMean = mean(left_channel),
            mean_temp = mean(temperature),
            mean_relh = mean(relh),
            mean_dew = mean(dew_temp))

ggplot(data = aci_means,
       aes(x = date_time_utc, y = aciMean, color=mean_relh)) +
  geom_point(size = 2, aes(shape = site))+
  geom_line(aes(group = site))+
  # facet_wrap(~site) +
  # scale_colour_continuous(labels = c(60,70,80,90))
  scale_color_viridis_c(name = "Relh",direction = -1)+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))


# Weather Graphs ----------------------------------------------------------

ggplot(data = aci_means,
       aes(x = date_time_utc, y = mean_temp, color= site)) +
  geom_line()

ggplot(data = aci_means,
       aes(x = date_time_utc, y = mean_relh, color= site)) +
  geom_line()
# # Uploading Data ----------------------------------------------------------
# aci_files = list.files(pattern = "aci") %>%
#   lapply(read.csv) %>%
#   bind_rows
# 
# aci_means = aci_files %>%
#   group_by(temperature,obs_time) %>%
#   summarize(aciMean = mean(left_channel))
# 
# adi_files = list.files(pattern = "adi") %>%
#   lapply(read.csv) %>%
#   bind_rows
# 
# adi_means = adi_files %>%
#   group_by(temperature,obs_time) %>%
#   summarize(adiMean = mean(left_channel))
# 
# aei_files = list.files(pattern = "aei") %>%
#   lapply(read.csv) %>%
#   bind_rows
# 
# aei_means = aei_files %>%
#   group_by(temperature,obs_time) %>%
#   summarize(aeiMean = mean(left_channel))
# 
# bio_files = list.files(pattern = "bio") %>%
#   lapply(read.csv) %>%
#   bind_rows
# 

# Compiling Filtered Data -------------------------------------------------

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data")
validation_data = read.csv("validation_acoustic_indices.csv", header = TRUE)
validation_data$date_time = as.Date((validation_data$date_time))

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/pabu_filtered/")
aru = c("aru01","aru04","aru05","ws01","ws02","ws03","ws04","ws05","ws06","ws11","ws12","ws13","ws14","ws15")
file_list = list.files(pattern = "adi")

adi_final = NULL
for(i in file_list){
  adi_temp = read.csv(i, header = TRUE) %>%
   filter(db_threshold == 50)%>% #comment out for aci and bio indicies
    rename(pabu_adi = left_channel) %>%
    dplyr::select(date_time,aru,pabu_adi)
  adi_final = rbind(adi_final,adi_temp)
}
write.csv(adi_final,"compiled_pabu_adi.csv", row.names = FALSE)

