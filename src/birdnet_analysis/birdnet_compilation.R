library(dplyr) #data manipulation
library(tibble) #data manipulation
library(lubridate) #manipulating date and time
library(hms) #manipulate time
library(zoo) #for na.approx to approximate missing values in weather dataset
library(ggplot2) #graphs
library(gridExtra) #ggplot multi panels
library(cowplot)
library(lme4) #lmm and glmm analysis
library(lmerTest) #get p-values for lmm tests
library(reshape2) #???
library(suncalc) #calculate sunrise time and sun altitude 
library(zoo)#approximating rows with NA in the weather data
library(car)#ANOVAs
library(multcomp) #posthoc tests for ANOVA type III effects
library(bbmle) #AIC comparisons
library(performance) #performance

# Load CSV Files ----------------------------------------------------------
sites = as.list(c("lwma","sswma","cbma","kiowa"))

results_final = NULL
for(s in sites){
  if(s == "lwma"){
    arus = as.list(c("aru01","aru02","aru03","aru04","aru05"))
  } else if(s == "sswma"){
    arus = as.list(c("aru01","aru02","aru03","aru04","aru05","ws01","ws02","ws03","ws04","ws05","ws06","ws11","ws12","ws13","ws14","ws15"))
  } else if(s == "cbma"){
    arus = as.list(c("aru01","aru02","aru03","aru04","aru05","wg01", "wg02", "wg03", "wg04", "wg05"))
  } else if(s == "kiowa") {
    arus = as.list(c("aru01","aru02","aru03","aru04","aru05"))
  }
  for(i in arus){
    setwd(paste0("/media/meelyn/LaCie/aridity_project/",s,"/",s,"_audio_files/",i))
    
    # setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/pabu_filtered/",
    # i,"_pabu_filtered"))
    birdnet = list.files(pattern = "BirdNET.results.csv")
    for(j in 1:length(birdnet)){
      
      results_temp = read.csv2(birdnet[[j]], header = TRUE, sep = ",", fill = TRUE)
      if(nrow(results_temp) == 0){
        results_temp = rbind(results_temp, data.frame("filepath"= birdnet[[j]], "start" = NA, "end" = NA, "scientific_name" = NA, "common_name" = NA, "confidence" = NA, "lat" = NA, "lon" = NA, "week" = NA, "overlap" = NA, "sensitivity" = NA, "min_conf" = NA, "species_list" = NA, "model" = NA))
      }
      results_temp$date_time = ymd_hms(substr(birdnet[[j]],1,15)) #change to 15,30 if you are using bird filtered data
      results_temp$date = date(results_temp$date_time)
      results_temp$time = as_hms(results_temp$date_time)
      results_temp$aru = i   
      results_temp$site = s
      results_final = rbind(results_final,results_temp)
    }
    
  }
  
}

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")

  lwma_aru_results = results_final %>%
    dplyr::filter(site == "lwma")
  save(lwma_aru_results, file = "lwma_aru_results.Rdata")

  sswma_aru_results = results_final %>%
    dplyr::filter(site == "sswma")
  save(sswma_aru_results, file = "sswma_aru_results.Rdata")

  cbma_aru_results = results_final %>%
    dplyr::filter(site == "cbma")
  save(cbma_aru_results, file = "cbma_aru_results.Rdata")
  
  kiowa_aru_results = results_final %>%
    dplyr::filter(site == "kiowa")
  save(kiowa_aru_results, file = "kiowa_aru_results.Rdata")
  
birdnet_data = results_final
save(birdnet_data, file = "birdnet_data.Rdata")


# Combining Site Datasets, adding sunlight and sunaltitude variables, and adding weather data from Mesonet Sites--------------------------------------

sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("lwma"))
arid_full = NULL
water_full = NULL
for(s in sites){
  setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
  if(s == "lwma"){
    # load lwma bird data
    load("birdnet_data/lwma_aru_results.Rdata")
    data = lwma_aru_results
    #load 2021 mesonet data
    load("mesonet_data/lwma_mesonet.Rdata")
    wd = lwma_mesonet %>%
      mutate(ghobs_scaled = scale(gh))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    #load historic data from 2005-2021
    load("historic_weather_data/lwma_wh.Rdata")
    hd = lwma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
  } else if(s == "sswma"){
    # load sswma bird data
    load("birdnet_data/sswma_aru_results.Rdata")
    data = sswma_aru_results
    load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/sswma_mesonet.Rdata")
    wd = sswma_mesonet%>%
      mutate(ghobs_scaled = scale(gh))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    
    #load historic data from 2005-2021
    load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/sswma_wh.Rdata")
    hd = sswma_wh %>%
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(s == "cbma"){
    # load cbma bird data
    load("birdnet_data/cbma_aru_results.Rdata")
    data = cbma_aru_results
    load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/cbma_mesonet.Rdata")
    wd = cbma_mesonet%>%
      mutate(ghobs_scaled = scale(gh))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/cbma_wh.Rdata")
    hd = cbma_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(s == "kiowa"){
    # load kiowa bird data
    load("birdnet_data/kiowa_aru_results.Rdata")
    data = kiowa_aru_results
    load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/kiowa_mesonet.Rdata")
    wd = kiowa_mesonet%>%
      mutate(ghobs_scaled = scale(gh))
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/kiowa_wh.Rdata")
    hd = kiowa_wh %>% 
      dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
      dplyr::filter(is.na(mas)==FALSE)  %>%
      group_by(month_day, mas) %>%
      summarise_all(funs(mean))
    tz = "US/Mountain"
    
  }
  
  # summarize birdnet data into number of vocalizations and number of species
  data_temp = data %>%
    mutate(date = as_date(date),
           local_time = force_tz(date_time, tz = tz),
           date_time = as_datetime(local_time, tz = "UTC"),
           time = as_hms(date_time),
           hour_local = hour(local_time),
           hour_utc = hour(date_time)
           ) %>%
    # filter(date > "2021-04-30 CDT")%>%
    # dplyr::filter(is.na(common_name) == FALSE) %>% 
    group_by(date,time,local_time,date_time,hour_local,hour_utc,lat,lon,aru,site) %>%
    summarise(num_vocals = n(),
              species_diversity = n_distinct(common_name))%>%
    arrange(date_time) %>%
    mutate(num_vocals = replace(num_vocals, is.na(lat)==TRUE && num_vocals == 1, 0),
           species_diversity = replace(species_diversity, is.na(lat)==TRUE && species_diversity == 1, 0))
  
  
  
    if(s == "kiowa"){
      kiowa_time_bad = data_temp %>% 
        dplyr::filter(hour_local ==12)%>%
        mutate(date_time = date_time-3600,
               local_time = local_time-3600,
               time = as_hms(date_time),
               hour_local = hour(local_time),
               hour_utc = hour(date_time))
      kiowa_time_good = data_temp %>% dplyr::filter(hour_local !=12)
      data_temp = rbind(kiowa_time_bad,kiowa_time_good) %>%
        arrange(date_time)

    } else {
    
  }
  
  data_temp = left_join(data_temp, wd, by = "date_time")
  data_missing = data_temp %>%
    dplyr::filter(is.na(mas)==TRUE)
  
  data_temp = data_temp %>% filter(is.na(mas) == FALSE) %>%
    mutate(month_day = format(as.Date(date_time), "%m-%d"))
  
  data_temp2 = full_join(data_temp, hd, by = c("month_day", "mas")) %>%
    arrange(month_day,mas)
  data_temp2$gh_hobs = na.approx(data_temp2$gh_hobs, na.rm = FALSE) 
  data_temp2$ghhobs_scaled = na.approx(data_temp2$ghhobs_scaled, na.rm = FALSE) 
  data_temp2$ghmean_time = na.approx(data_temp2$ghmean_time, na.rm = FALSE) 
  data_temp2$ghsite_scaled = na.approx(data_temp2$ghsite_scaled, na.rm = FALSE) 
  
  data_temp3 = data_temp2 %>%
    mutate(site = factor(site.x, levels=c("lwma","sswma","cbma","kiowa")))%>%
    # dplyr::select(-site.x, -site.y,-hour.x,-hour.y)
    dplyr::select(-hour, -site.y, -hour_utc.y) %>%
    rename(hour_utc = "hour_utc.x") 
  
  data_temp_arid = data_temp3 %>%
    filter(aru == "aru01" | aru == "aru02"| aru == "aru03"| aru == "aru04"| aru == "aru05")
  
  data_temp_water = data_temp3 %>%
  filter(aru == "ws01" | aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05" |
         aru == "ws06" | aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10" | 
         aru == "ws11" | aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15" |
         aru == "wg01" | aru == "wg02"| aru == "wg03"| aru == "wg04"| aru == "wg05")
  
  arid_full = rbind(data_temp_arid,arid_full) %>%
    arrange(site,aru,hour_utc)
  
  water_full = rbind(data_temp_water,water_full) %>%
    arrange(site,aru,hour_utc)
  
}

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data")
save(arid_full, file = "birdnet_data/aridity_gradient_ml_raw.Rdata")
save(water_full, file = "birdnet_data/water_supp_ml_raw.Rdata")


# Saving Aridity Gradient Data Results ----------------------------------------------
#Aridity Gradient Data for statistical analyses
full_arid = arid_full %>% #saving it a different name so you don't overwrite it
  dplyr::filter(is.na(mas)==FALSE) %>%
  dplyr::filter(is.na(gh_hobs) == FALSE) %>%
  dplyr::filter(hour_local <13) %>%
  dplyr::filter(year(date) != 2106) %>%
  dplyr::filter(date < "2021-08-16")
full_arid$ghacross_sites = scale(full_arid$gh)


max(as.numeric(full_arid$mas))
which.max(full_arid$mas)
mas_check = full_arid[which.max(full_arid$mas),];mas_check
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/birdnet_data/")
save(full_arid, file = "aridity_gradient_ml.Rdata")
load("aridity_gradient_ml.Rdata")


# Water Saving Supplementation Data Compilation ----------------------------------

full_water = water_full %>% #saving it a different name so you don't overwrite it
  dplyr::filter(is.na(mas)==FALSE) %>%
  dplyr::filter(is.na(gh_hobs) == FALSE) %>%
  dplyr::filter(hour_local <13) %>%
  dplyr::filter(year(date) != 2106) %>%
  dplyr::filter(is.na(mas)==FALSE) %>%
  dplyr::filter(date < "2021-08-16")
full_water$ghacross_sites = scale(full_water$gh)

#Separating out CBMA water sites
full_water1 = full_water %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = full_water %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)

cbma_full_water = rbind(full_water1, full_water2)

#Separating out SSWMA water sites
sswma_full_water1 = full_water %>%
  filter(aru == "ws01"| aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05")%>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0),
         ws_site = 1)

sswma_full_water2 = full_water %>%
  filter(aru == "ws06"| aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10") %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0),
         ws_site = 2)

sswma_full_water3 = full_water %>%
  filter(aru == "ws11"| aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15") %>%
  mutate(water = 0,
         ws_site = 3)

sswma_full_water = rbind(sswma_full_water1, sswma_full_water2, sswma_full_water3)
water_compiled = rbind(cbma_full_water,sswma_full_water)


#Checking to see if there are any outliers in the minutes after sunrise
max(as.numeric(as.character(water_compiled$mas)))
which.max(as.numeric(as.character(water_compiled$mas)))
mas_check = water_compiled[which.max(water_compiled$mas),];mas_check
  
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/birdnet_data/")
save(water_compiled, file = "water_supp_ml.Rdata")
load("water_supp_ml.Rdata")

#Plotting mean num_vocals across sun altitude per hour
ggplot(data = full_arid, aes(x = relh, y = gh, color = site))+ #sun altitude does not overlap between sites, so good metric for time AND location
  geom_line()



# Aridity Graident Date bin ----------------------------------
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/birdnet_data/")
load("aridity_gradient_ml.Rdata")

arid_date = full_arid %>%
  # dplyr::filter(is.na(arid) == FALSE) %>%
  mutate(week = week(date_time))%>%
  group_by(site,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            # se_vocals = sd(num_vocals)/sqrt(n()),
            mean_species = mean(species_diversity),
            # se_species = sd(species_diversity)/sqrt(n()),
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites
  )
arid_date$arid_within = cut(arid_date$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
arid_date$arid_across = cut(arid_date$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
arid_date$hist_within = cut(arid_date$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
arid_date$hist_across = cut(arid_date$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/birdnet_data")
save(arid_date, file = "birdnet_totals_arid_date.Rdata")


# Aridity Gradient Date-Bin - Plots ---------------------------------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")

graph_arid_date = arid_date %>%
  dplyr::filter(is.na(date) == FALSE)%>%
  group_by(site,arid_within) %>%
  summarise(n = n(),
            vocals_mean = mean(mean_vocals),
            vocals_se = (sd(mean_vocals))/sqrt(n()),
            species_mean = mean(mean_species),
            species_se = sd(mean_species)/sqrt(n()))

### Boxplot Graph of Number of Vocalizations - Date bin
ggplot(data = arid_date %>% 
         dplyr::filter(is.na(arid_within)==FALSE), 
       aes(x=arid_within, y= mean_vocals, color = as.factor(site))) +
  # geom_point(aes(color = site), shape = 20, size = 3)+
  geom_boxplot()+
  stat_summary(fun = "mean",
               geom = "point",
               size = 5,
               aes(group=as.factor(site)),
               position = position_dodge(0.75)) +
  # geom_jitter()+
  # geom_point(size = 2, position = position_dodge(0.2))+
  # geom_errorbar(aes(ymin = vocals_mean-vocals_se, 
  #                   ymax = vocals_mean+vocals_se), width = 0.2,
  #               position = position_dodge())+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/arid_date_bin_vocals_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

graph_arid_date = arid_date %>%
  dplyr::filter(is.na(date) == FALSE)%>%
  group_by(site,arid_within) %>%
  summarise(n = n(),
            vocals_mean = mean(mean_vocals),
            vocals_se = (sd(mean_vocals))/sqrt(n()),
            species_mean = mean(mean_species),
            species_se = sd(mean_species)/sqrt(n()))

### Aridity Gradient - Day Bin - Dot Plot - Number of Vocals
ggplot(data = graph_arid_date,
       aes(x=arid_within, y= vocals_mean, color = as.factor(site))) +
  geom_point(size = 2, position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = vocals_mean-vocals_se, 
                    ymax = vocals_mean+vocals_se), width = 0.2,
                position = position_dodge(0.5))+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_x_discrete(name = "Aridity", labels = c("Extremely\nHumid","Humid","Normal","Arid","Extremely\nArid"))+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aridity_gradient_datebin_vocals_dotplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

### Boxplot Graph of Species Diversity - Date bin
ggplot(data = arid_date %>% 
         dplyr::filter(is.na(arid_within)==FALSE), 
       aes(x=arid_within, y= mean_species, color = as.factor(site))) +
  # geom_point(aes(color = site), shape = 20, size = 3)+
  geom_boxplot()+
  stat_summary(fun = "mean",
               geom = "point",
               size = 5,
               aes(group=as.factor(site)),
               position = position_dodge(0.75)) +
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Species Diversity")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/arid_date_bin_species_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

### Aridity Gradient - Day Bin - Dot Plot - Species Diversity
ggplot(data = graph_arid_date,
       aes(x=arid_within, y= species_mean, color = as.factor(site))) +
  geom_point(size = 2, position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = species_mean-species_se, 
                    ymax = species_mean+species_se), width = 0.2,
                position = position_dodge(0.5))+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_x_discrete(name = "Aridity", labels = c("Extremely\nHumid","Humid","Normal","Arid","Extremely\nArid"))+
  scale_y_continuous(name = "Mean\nNumber of Species")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aridity_gradient_datebin_species_dotplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

# Aridity Gradient Date-bin - Statistical Analyses ------------------------------------------
library(emmeans)

### Date-bin Number of Vocalizations

ggplot(arid_date,
       aes(x=arid_within,y=mean_vocals,color=site)) + 
  geom_jitter() + 
  geom_boxplot(alpha=0.2) + 
  facet_wrap(~site)

# arid_date$aridx = interaction(arid_date$arid_within, arid_date$site)
date_vocals = lm(mean_vocals ~ arid_within*site, data = arid_date)
summary(date_vocals)
assump(date_vocals)
aov(date_vocals)
fit_date = Anova(date_vocals,
         contrasts=list(factorA='arid_within', FactorB ='site'), 
         data = arid_date,
         type='III')
TukeyHSD(aov(date_vocals))
# date_vocals_emms = emmeans(date_vocals, "aridx", lmerTest.limit = 32094)
# pairs(date_vocals_emms)

### Date-bin Species Diversity

ggplot(arid_date,
       aes(x=arid_within,y=mean_species,color=site)) + 
  geom_jitter() + 
  geom_boxplot(alpha=0.2) + 
  facet_wrap(~site)

arid_date$aridx = interaction(arid_date$arid_within, arid_date$site)
date_species = lm(mean_species ~ arid_within*site, data = arid_date)
summary(date_species)
assump(date_species)
aov(date_species)
fit_date = Anova(date_species,
                 contrasts=list(factorA='arid_within', FactorB ='site'), 
                 data = arid_date,
                 type='III')
TukeyHSD(aov(date_species))
# date_species_emms = emmeans(date_species, "aridx", lmerTest.limit = 317)
# pairs(date_species_emms)




# Aridity Gradient - MAS bin - Num Vocals and Species Diversity -----------
### MAS Bin - Num Vocals and Species Diversity
arid_mas = full_arid %>%
  dplyr::filter(is.na(mas) == FALSE) %>%
  group_by(site,mas)%>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            mean_species = mean(species_diversity),
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites
  )

arid_mas$mas_num = as.numeric(as.character(arid_mas$mas))
arid_mas$arid_within = cut(arid_mas$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
arid_mas$arid_across = cut(arid_mas$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
arid_mas$hist_within = cut(arid_mas$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
arid_mas$hist_across = cut(arid_mas$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
arid_mas$mas_bin = cut(arid_mas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/birdnet_data")
save(arid_mas, file = "birdnet_totals_arid_mas.Rdata")


# Aridity Gradient - MAS Bin - Plots --------------------------------------

graph_arid_mas = arid_mas %>%
  dplyr::filter(is.na(mas_bin) == FALSE)%>%
  group_by(site,mas_bin) %>%
  summarise(n = n(),
            vocals_mean = mean(mean_vocals),
            vocals_se = (sd(mean_vocals))/sqrt(n()),
            species_mean = mean(mean_species),
            species_se = sd(mean_species)/sqrt(n()))

### Aridity Gradient - Mas Bin - Dot Plot - Number of Vocals
ggplot(data = graph_arid_mas %>% 
         dplyr::filter(is.na(mas_bin)==FALSE), 
       aes(x=mas_bin, y= vocals_mean, color = as.factor(site))) +
  geom_point(size = 2, position = position_dodge(0.2))+
  geom_errorbar(aes(ymin = vocals_mean-vocals_se, 
                    ymax = vocals_mean+vocals_se), width = 0.2,
                position = position_dodge())+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aridity_gradient_masbin_vocals_dotplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

### Aridity Gradient - Mas Bin - Dot Plot - Number of Vocals
ggplot(data = graph_arid_mas %>% 
         dplyr::filter(is.na(mas_bin)==FALSE), 
       aes(x=mas_bin, y= species_mean, color = as.factor(site))) +
  geom_point(size = 2, position = position_dodge(0.2))+
  geom_errorbar(aes(ymin = species_mean-species_se, 
                    ymax = species_mean+species_se), width = 0.2,
                position = position_dodge())+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Species Diversity")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aridity_gradient_masbin_species_dotplot_.jpg", width = 8, height = 6, units = "in", dpi = 600)
# Aridity Gradient MAS-bin - Statistical Analyses ------------------------------------------
library(emmeans)

### MAS-bin Number of Vocalizations
arid_mas$aridx = interaction(arid_mas$mas_bin, arid_mas$site)

# stripchart(mean_vocals ~ site, vertical = TRUE, pch = 1, xlab = "site", 
#            data = arid_mas)
ggplot(arid_mas,
       aes(x=mas_bin,y=mean_vocals,color=site)) + 
  geom_boxplot()
  # geom_boxplot(alpha=0.2) + 
  # facet_wrap(~site)

mas_vocals = lm(mean_vocals ~ mas_bin*site, data = arid_mas)
summary(mas_vocals)
aov(mas_vocals)
Anova(mas_vocals, type = "III")
TukeyHSD(aov(mas_vocals))
# anova(mas_vocals)
# ranef(mas_vocals)
# mas_vocals_emms = emmeans(mas_vocals, "mas_bin", lmerTest.limit = 418)
# pairs(mas_vocals_emms)

### MAS-bin Species Diversity
# arid_mas$aridx = interaction(arid_mas$mas_bin, arid_mas$site)
mas_species = lm(mean_species ~ mas_bin*site, data = arid_mas)
summary(mas_species)
aov(mas_species)
Anova(mas_species, type = "III")
TukeyHSD(aov(mas_species))
# mas_species_emms = emmeans(mas_species, "aridx", lmerTest.limit = 418)
# pairs(mas_species_emms)


# Water Supplementation Data - Date Bin - Data Organization ----------
water_compiled$water_int = interaction(as.factor(water_compiled$ws_site),as.factor(water_compiled$water))

water_date = water_compiled %>%
  mutate(date = date(date_time))%>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  dplyr::filter(date < "2021-08-16") %>%
  arrange(site,date) %>%
  group_by(site,ws_site,water,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            # se_vocals = sd(num_vocals)/sqrt(n()),
            mean_species = mean(species_diversity),
            # se_species = sd(species_diversity)/sqrt(n()),
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled)) #%>% #historic aridity scaled across sites

# water_date$mas_num = as.numeric(as.character(water_date$mas))
water_date$arid_within = cut(water_date$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
water_date$arid_across = cut(water_date$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
water_date$hist_within = cut(water_date$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
water_date$hist_across = cut(water_date$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
# water_date$mas_bin = as.factor(ifelse(water_date$mas_num < 0, 0, cut(water_date$mas_num, breaks = 3)))

sswater_date = water_date %>%
  dplyr::filter(site == "sswma")
cbwater_date = water_date %>%
  dplyr::filter(site == "cbma")


# SSWMA Water Supplementation - Date Bin - Plots ---------------------------------------

sswma1_rec1 <- data.frame (xmin=as_date("2021-05-17"), 
                           xmax=as_date("2021-05-30"), 
                           ymin=-Inf, ymax=Inf) #start of water site 1 with water
sswma2_rec1 <- data.frame (xmin=as_date("2021-05-30"), 
                           xmax=as_date("2021-06-13"), 
                           ymin=-Inf, ymax=Inf) #start of water site 2 with water
sswma1_rec2 = data.frame (xmin=as_date("2021-06-13"), 
                          xmax=as_date("2021-07-02"), 
                          ymin=-Inf, ymax=Inf) #start of water site 1 with water
sswma2_rec2 = data.frame (xmin=as_date("2021-07-03"), 
                          xmax=as_date("2021-08-07"), 
                          ymin=-Inf, ymax=Inf) #start of water at water site 2

#SSWMA Vocals Graph
ggplot(data = sswater_date,
                      # wsvocals_day = ggplot(data = water_date %>%dplyr::filter(site == "sswma"), #uncomment to summarize by date only
                      aes(x=date, y=log(mean_vocals), 
                          color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=sswma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#E69F00", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.1, inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean Num. Vocals)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_vocals_rectangle_plots.jpg", width = 8, height = 6, units = "in")

#Boxplot for Water SSWMA vocals Diversity
ggplot(data = sswater_date,
       aes(x=as.factor(ws_site), y=log(mean_vocals), 
                                   color = as.factor(ws_site),
                                   fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean\nNum.\nVocals)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# sswmaw_vocals_plots = plot_grid(wsvocals_day, boxplot_sswma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_vocals_plots
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_vocals_boxplots.jpg", width = 8, height = 6, units = "in")

ggplot(data = sswater_date,
# wsspecies_day = ggplot(data = water_date %>%dplyr::filter(site == "sswma"), #uncomment to summarize by date only
                       aes(x=date, y=log(mean_species), 
                           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=sswma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#E69F00", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#E69F00", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),
                     name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean Species Diversity")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_species_rectangle_plots.jpg", width = 8, height = 6, units = "in")

#Boxplot for Water SSWMA Species Diversity
ggplot(data = sswater_date,
                           aes(x=as.factor(ws_site), y=log(mean_species), 
                               color = as.factor(ws_site),
                               fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean Species Diversity)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# sswmaw_species_plots = plot_grid(wsspecies_day, boxplot_sswma_species,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_species_plots
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_species_boxplots.jpg", width = 8, height = 7.5, units = "in")

# CBMA Water Supplementation - Date Bin - Plots --------------------------------------------------------
full_water1 = full_water %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = full_water %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)


cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), xmax=as_date("2021-06-04"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), xmax=as_date("2021-07-19"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2

#CBMA Vocals Graph
ggplot(data = cbwater_date,
                      # wsvocals_day = ggplot(data = water_date %>%dplyr::filter(site == "cbma"), #uncomment to summarize by date only
                      aes(x=date, y=mean_species, 
                          color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=cbma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=cbma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Mean Species Diversity)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("cbma_water_vocals_rectangle_plots.jpg", width = 8, height = 6, units = "in")

#Boxplot for Water cbma vocals Diversity
ggplot(data = cbwater_date,
       aes(x=as.factor(ws_site), y=mean_species, 
                                  color = as.factor(ws_site),
                                  fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 2,
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Mean Species Diversity")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# cbmaw_vocals_plots = plot_grid(wcvocals_day, boxplot_cbma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));cbmaw_vocals_plots
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("cbmaw_vocals_boxplots.jpg",width = 8, height = 6, units = "in")

ggplot(data = cbwater_date,
                       # wsspecies_day = ggplot(data = water_date %>%dplyr::filter(site == "cbma"), #uncomment to summarize by date only
                       aes(x=date, y=log(mean_species), 
                           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=cbma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=cbma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  # geom_smooth(method = "lm")+ #date not significant so no trendlines
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean\nSpecies\nDiversity")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("cbma_water_species_rectangle_plots.jpg", width = 8, height = 6, units = "in")


#Boxplot for Water cbma Species Diversity
ggplot(data = cbwater_date,
                               aes(x=as.factor(ws_site), y=log(mean_species), 
                                   color = as.factor(ws_site),
                                   fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 5,
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean Species Diversity)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# cbmaw_species_plots = plot_grid(wcspecies_day, boxplot_cbma_species,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));cbmaw_species_plots
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results")
ggsave("cbma_water_species_boxplots.jpg",width = 8, height = 7.5, units = "in")




# SSWMA Water Supplementation - Date Bin - Statistical Analysis -----------
sswater_date$aridx_date = interaction(sswater_date$arid_within,as.factor(sswater_date$ws_site),as.factor(sswater_date$water))
sswater_vocals = lm(mean_vocals ~ aridx_date, data = sswater_date)
summary(sswater_vocals)
assump(sswater_vocals)
aov(sswater_vocals)
fit_date = Anova(sswater_vocals,
                 contrasts=list(factorA='arid_within', FactorB ='site'), 
                 data = arid_date,
                 type='III')
TukeyHSD(aov(sswater_vocals))
# date_vocals_emms = emmeans(date_vocals, "aridx", lmerTest.limit = 32094)
# pairs(date_vocals_emms)

### Date-bin Species Diversity

ggplot(arid_date,
       aes(x=arid_within,y=mean_species,color=site)) + 
  geom_jitter() + 
  geom_boxplot(alpha=0.2) + 
  facet_wrap(~site)

sswater_date$aridx_date = interaction(sswater_date$arid_within,as.factor(sswater_date$ws_site),as.factor(sswater_date$water))
sswater_species = lm(mean_species ~ aridx_date, data = sswater_date)
summary(sswater_species)
assump(sswater_species)
aov(sswater_species)
fit_date = Anova(sswater_species,
                 contrasts=list(factorA='arid_within', FactorB ='site'), 
                 data = arid_date,
                 type='III')
TukeyHSD(aov(sswater_species))
# date_species_emms = emmeans(date_species, "aridx", lmerTest.limit = 317)
# pairs(date_species_emms)


# CBMA Water Supplementation - Day Bin - Statistical Analysis -------------
cbwater_date$aridx_date = interaction(cbwater_date$arid_within,as.factor(cbwater_date$ws_site),as.factor(cbwater_date$water))
cbwater_vocals = lm(mean_vocals ~ aridx_date, data = cbwater_date)
summary(cbwater_vocals)
assump(cbwater_vocals)
aov(cbwater_vocals)
fit_date = Anova(cbwater_vocals,
                 contrasts=list(factorA='arid_within', FactorB ='site'), 
                 data = arid_date,
                 type='III')
TukeyHSD(aov(cbwater_vocals))
# date_vocals_emms = emmeans(date_vocals, "aridx", lmerTest.limit = 32094)
# pairs(date_vocals_emms)

### Date-bin Species Diversity

ggplot(arid_date,
       aes(x=arid_within,y=mean_species,color=site)) + 
  geom_jitter() + 
  geom_boxplot(alpha=0.2) + 
  facet_wrap(~site)

cbwater_date$aridx_date = interaction(cbwater_date$arid_within,as.factor(cbwater_date$ws_site),as.factor(cbwater_date$water))
cbwater_species = lm(mean_species ~ aridx_date, data = cbwater_date)
summary(cbwater_species)
assump(cbwater_species)
aov(cbwater_species)
fit_date = Anova(cbwater_species,
                 contrasts=list(factorA='arid_within', FactorB ='site'), 
                 data = arid_date,
                 type='III')
TukeyHSD(aov(cbwater_species))
# date_species_emms = emmeans(date_species, "aridx", lmerTest.limit = 317)
# pairs(date_species_emms)

# Water Supplementation - MAS Bin - data organization-----------------------------------------
water_mas = water_compiled %>%
  mutate(date = date(date_time))%>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  dplyr::filter(date < "2021-08-16") %>%
  arrange(site,date) %>%
  group_by(site,ws_site,water,mas) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            # se_vocals = sd(num_vocals)/sqrt(n()),
            mean_species = mean(species_diversity),
            # se_species = sd(species_diversity)/sqrt(n()),
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled)) #%>% #historic aridity scaled across sites

water_mas$mas_num = as.numeric(as.character(water_mas$mas))
water_mas$arid_within = cut(water_mas$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
water_mas$arid_across = cut(water_mas$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
water_mas$hist_within = cut(water_mas$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
water_mas$hist_across = cut(water_mas$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
water_mas$mas_bin = cut(water_mas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

sswater_mas = water_mas %>%
  dplyr::filter(site == "sswma")
cbwater_mas = water_mas %>%
  dplyr::filter(site == "cbma")


# SSWMA Water Supplementation - MAS Bin - Plots ---------------------------------------

#Boxplot for Water SSWMA vocals Diversity
ggplot(data = sswater_mas,
       aes(x=mas_bin, y=log(mean_vocals), 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  # stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(ws_site)),
               size = 2,
               position = position_dodge(0.5), preserve = "single") +
  scale_y_continuous(name = "Log\n(Mean Num. Vocals)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# sswmaw_vocals_plots = plot_grid(wsvocals_day, boxplot_sswma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_vocals_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("sswma_water_vocals_boxplots.jpg",width = 8, height = 6, units = "in")


#Boxplot for Water SSWMA Species Diversity
ggplot(data = sswater_mas,
       aes(x=as.factor(ws_site), y=log(mean_species), 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean\nSpecies\nDiversity)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
sswmaw_species_plots = plot_grid(wsspecies_day, boxplot_sswma_species,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_species_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("sswmaw_species_boxplots.jpg",plot = boxplot_sswma_species, width = 8, height = 7.5, units = "in")

# CBMA Water Supplementation - MAS Bin - Plots --------------------------------------------------------
full_water1 = full_water %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = full_water %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)


cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), xmax=as_date("2021-06-04"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), xmax=as_date("2021-07-19"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2

#CBMA Vocals Graph
ggplot(data = cbwater_mas,
       # wsvocals_day = ggplot(data = water_date %>%dplyr::filter(site == "cbma"), #uncomment to summarize by date only
       aes(x=date, y=log(mean_vocals), 
           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=cbma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=cbma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean\nNum.\nVocals)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")

#Boxplot for Water cbma vocals Diversity
ggplot(data = cbwater_mas,
       aes(x=as.factor(ws_site), y=log(mean_vocals), 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean\nNum.\nVocals)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
cbmaw_vocals_plots = plot_grid(wcvocals_day, boxplot_cbma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));cbmaw_vocals_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbmaw_vocals_boxplots.jpg",plot = boxplot_cbma_vocals, width = 8, height = 7.5, units = "in")

ggplot(data = cbwater_mas,
       # wsspecies_day = ggplot(data = water_date %>%dplyr::filter(site == "cbma"), #uncomment to summarize by date only
       aes(x=date, y=log(mean_species), 
           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=cbma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=cbma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  # geom_smooth(method = "lm")+ #date not significant so no trendlines
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean\nSpecies\nDiversity")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none")

#Boxplot for Water cbma Species Diversity
ggplot(data = cbwater_mas,
       aes(x=as.factor(ws_site), y=log(mean_species), 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean\nSpecies\nDiversity)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20));boxplot_cbma_species
cbmaw_species_plots = plot_grid(wcspecies_day, boxplot_cbma_species,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));cbmaw_species_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbmaw_species_boxplots.jpg",plot = boxplot_cbma_species, width = 8, height = 7.5, units = "in")




# SSWMA Water Supplementation - MAS Bin - Statistical Analyses ------------

sswater_vocals = lm(mean_vocals ~ mas_bin*as.factor(ws_site)*as.factor(water), data = sswater_mas)
summary(sswater_vocals)
aov(sswater_vocals)
Anova(sswater_vocals, type = "III")
TukeyHSD(aov(sswater_vocals))
# anova(mas_vocals)
# ranef(mas_vocals)
# mas_vocals_emms = emmeans(mas_vocals, "mas_bin", lmerTest.limit = 418)
# pairs(mas_vocals_emms)

### MAS-bin Species Diversity
# arid_mas$aridx = interaction(arid_mas$mas_bin, arid_mas$site)
sswater_species = lm(mean_species ~ mas_bin*as.factor(ws_site)*as.factor(water), data = sswater_mas)
summary(sswater_species)
aov(sswater_species)
Anova(sswater_species, type = "III")
TukeyHSD(aov(sswater_species))
# mas_species_emms = emmeans(mas_species, "aridx", lmerTest.limit = 418)
# pairs(mas_species_emms)


# CBMA Water Supplementation - MAS Bin - Statistical Analysis -------------
cbwater_mas$aridx = interaction(cbwater_mas$mas_bin,as.factor(cbwater_mas$ws_site),as.factor(cbwater_mas$water))
cbwater_vocals = lm(mean_vocals ~ aridx, data = cbwater_mas)
summary(cbwater_vocals)
aov(cbwater_vocals)
Anova(cbwater_vocals, type = "III")
TukeyHSD(aov(cbwater_vocals))
# anova(mas_vocals)
# ranef(mas_vocals)
# mas_vocals_emms = emmeans(mas_vocals, "mas_bin", lmerTest.limit = 418)
# pairs(mas_vocals_emms)

### MAS-bin Species Diversity
cbwater_mas$aridx = interaction(cbwater_mas$mas_bin,as.factor(cbwater_mas$ws_site),as.factor(cbwater_mas$water))
cbwater_species = lm(mean_species ~ aridx, data = cbwater_mas)
summary(cbwater_species)
aov(cbwater_species)
Anova(cbwater_species, type = "III")
TukeyHSD(aov(cbwater_species))
# cbwater_species_emms = emmeans(cbwater_species, "aridx", lmerTest.limit = 306)
# pairs(mas_species_emms)

# Running Water Supplementation SSWMA and CBMA Separately LMMs!!! Data Summarized by ARU-----------------
water_aru$water_int = interaction(as.factor(water_aru$ws_site),as.factor(water_aru$water))


sswma_water = water_aru %>%
  dplyr::filter(site == "sswma")
cbma_water = water_aru %>%
  dplyr::filter(site == "cbma")
wre1 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date) + (1|ws_site), REML = TRUE, data = sswma_water) #best fit
wre2 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date) + (1|aru), REML = TRUE, data = sswma_water)
wre3 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date) + (1|ws_site/aru), REML = TRUE, data = sswma_water)
AICctab(wre1,wre2,wre3, nobs = 845, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
assump(wre2)

#SSWMA Water Supplementation LMMs - Number of Vocalizations

swv1 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = sswma_water)
swv2 = lmer(log(mean_vocals) ~ max_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = sswma_water)
AICctab(swv1,swv2,nobs = 4374, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(swv1) #water interaction and altitude are most significant factors, num vocals decreases with increasing altitude, and ws_site 1 increases with water, but ws_site 2 decreases with water
assump(swv1)
Anova(swv1, type = "III")
contrasts = glht(swv1, linfct = mcp(water_int = "Tukey"))
summary(contrasts)

##CBMA Water Supplementation LMMs - Number of Vocalizations

cwv1 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = cbma_water)
cwv2 = lmer(log(mean_vocals) ~ max_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = cbma_water)
AICctab(cwv1,cwv2,nobs = 2097, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(cwv1) #num vocals decrease with altitude, increase with access to water in site 1
Anova(cwv1, type = "III")
contrasts = glht(cwv1, linfct = mcp(water_int = "Tukey"))
summary(contrasts)

#SSWMA Water Supplementation LMMs Species Diversity

sws1 = lmer(log(mean_species)~mean_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = sswma_water)
sws2 = lmer(log(mean_species) ~ max_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = sswma_water)
AICctab(sws1,sws2,nobs = 4374, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(sws1) #water interaction and altitude are most significant factors, num vocals decreases with increasing altitude, and ws_site 1 increases with water, but ws_site 2 decreases with water
assump(sws1)
Anova(sws1, type = "III")
contrasts = glht(sws1, linfct = mcp(water_int = "Tukey"))
summary(contrasts)

##CBMA Water Supplementation LMMs Species Diversity

cws1 = lmer(log(mean_species) ~ mean_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = cbma_water)
cws2 = lmer(log(mean_species) ~ max_gh*water_int*scale(date)*hour_utc + (1|aru), REML = FALSE, data = cbma_water)
AICctab(cws1,cws2,nobs = 2097, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(cws1) #num vocals decrease with altitude, increase with access to water in site 1
Anova(cws1, type = "III")
contrasts = glht(cws1, linfct = mcp(water_int = "Tukey"))
summary(contrasts)




# Water Supplementation Experiment Sites Together - Raw Data----------------------------------------

water_compiled$water_int = interaction(water_compiled$ws_site,water_compiled$water)
wre1 = glmer(num_vocals ~ abs((1/gh))*water_int+ (1|site), family = "poisson", data = water_compiled)
wre2 = glmer(num_vocals ~ abs((1/gh))*water_int + (1|aru), family = "poisson", data = water_compiled)
wre3 = glmer(num_vocals ~ abs((1/gh))*water_int + (1|site/aru), family = "poisson", data = water_compiled)
wre4 = glmer(num_vocals ~ abs((1/gh))*water_int + (1|ws_site/aru), family = "poisson", data = water_compiled)
AICctab(wre1,wre2,wre3,wre4, nobs = 35106, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
#if site is included in the water_int interaction variable, then aru is the best random effect
#would be better to analyze water sswma and cbma separatley?

#Water Supplementation - Number of Vocalizations
wv1 = glmer(num_vocals ~ 1 + (1|aru), family = "poisson", data = water_compiled)
wv2 = glmer(num_vocals ~ abs((1/gh)) + (1|aru), family = "poisson", data = water_compiled)
wv3 = glmer(num_vocals ~ altitude + (1|aru), family = "poisson", data = water_compiled)
wv4 = glmer(num_vocals ~ water_int + (1|aru), family = "poisson", data = water_compiled)
wv5 = glmer(num_vocals ~ site + (1|aru), family = "poisson", data = water_compiled)
wv5 = glmer(num_vocals ~ water_int*abs((1/gh)) + (1|aru), family = "poisson", data = water_compiled)
wv6 = glmer(num_vocals ~ water_int*abs((1/gh))*site + (1|aru), family = "poisson", data = water_compiled)
wv7 = glmer(num_vocals ~ water_int+altitude + (1|aru), family = "poisson", data = water_compiled)
AICctab(wv1,wv2,wv3,wv4,wv5,wv6, nobs = 35106, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(wv6)
# Running Water Supplementation SSWMA and CBMA Separately -----------------

wre1 = glmer(num_vocals ~ abs((1/gh))*water_int + (1|aru), family = "poisson", data = sswma_water) #best fit
wre2 = glmer(num_vocals ~ abs((1/gh))*water_int + (1|ws_site), family = "poisson", data = sswma_water)
wre3 = glmer(num_vocals ~ abs((1/gh))*water_int + (1|ws_site/aru), family = "poisson", data = sswma_water)
AICctab(wre1,wre2,wre3, nobs = 22745, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

#SSWMA Water Supplementation GLMMs
swv1 = glmer(num_vocals ~ 1 + (1|aru), family = "poisson", data = sswma_water)
swv2 = glmer(num_vocals ~ abs((1/gh)) + (1|aru), family = "poisson", data = sswma_water)
swv3 = glmer(num_vocals ~ altitude + (1|aru), family = "poisson", data = sswma_water)
swv4 = glmer(num_vocals ~ water_int + (1|aru), family = "poisson", data = sswma_water)
swv5 = glmer(num_vocals ~ water_int*abs((1/gh)) + (1|aru), family = "poisson", data = sswma_water)
swv6 = glmer(num_vocals ~ water_int*altitude + (1|aru), family = "poisson", data = sswma_water)
swv7 = glmer(num_vocals ~ water_int*scale(mas) + (1|aru), family = "poisson", data = sswma_water)
AICctab(swv1,swv2,swv3,swv4,swv5,swv6,swv7,nobs = 22745, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(swv5) #water interaction and altitude are most significant factors, num vocals decreases with increasing altitude, and ws_site 1 increases with water, but ws_site 2 decreases with water
assump(swv5)
aswv5 = Anova(swv5, type = "III")
aswv5 = aov(swv5)
contrasts = glht(aswv5, linfct = mcp(water_int = "Tukey"))
summary(contrasts)

##CBMA Water Supplementation GLMMs
cwv1 = glmer(num_vocals ~ 1 + (1|aru), family = "poisson", data = cbma_water)
cwv2 = glmer(num_vocals ~ abs((1/gh)) + (1|aru), family = "poisson", data = cbma_water)
cwv3 = glmer(num_vocals ~ altitude + (1|aru), family = "poisson", data = cbma_water)
cwv4 = glmer(num_vocals ~ water_int + (1|aru), family = "poisson", data = cbma_water)
cwv5 = glmer(num_vocals ~ water_int*abs((1/gh)) + (1|aru), family = "poisson", data = cbma_water)
cwv6 = glmer(num_vocals ~ water_int*altitude + (1|aru), family = "poisson", data = cbma_water)
cwv7 = glmer(num_vocals ~ water_int*scale(mas) + (1|aru), family = "poisson", data = cbma_water)
AICctab(cwv1,cwv2,cwv3,cwv4,cwv5,cwv6,cwv7, nobs = 22745, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(cwv5) #num vocals decrease with altitude, increase with access to water in site 1
Anova(cwv5, type = "III")
contrasts = glht(cwv6, linfct = mcp(water_int = "Tukey"))
summary(contrasts)


# Water Supplementation Species Diversity GLMMs --------------------------------------------------
#SSWMA Water Supplementation GLMMs
swsd1 = glmer(species_diversity ~ 1 + (1|aru), family = "poisson", data = sswma_water)
swsd2 = glmer(species_diversity ~ abs((1/gh)) + (1|aru), family = "poisson", data = sswma_water)
swsd3 = glmer(species_diversity ~ altitude + (1|aru), family = "poisson", data = sswma_water)
swsd4 = glmer(species_diversity ~ water_int + (1|aru), family = "poisson", data = sswma_water)
swsd5 = glmer(species_diversity ~ water_int*abs((1/gh)) + (1|aru), family = "poisson", data = sswma_water)
swsd6 = glmer(species_diversity ~ water_int*altitude + (1|aru), family = "poisson", data = sswma_water)
swsd7 = glmer(species_diversity ~ water_int*scale(mas) + (1|aru), family = "poisson", data = sswma_water)
AICctab(swsd1,swsd2,swsd3,swsd4,swsd5,swsd6,swsd7, nobs = 12361, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(swsd5) #species diversity increases with aridity
Anova(swsd5, type = "III")
contrasts = glht(swsd5, linfct = mcp(water_int = "Tukey"))
summary(contrasts) #water increases species_diversity at ws_site 1, but decreases at ws_site 2

##CBMA Water Supplementation GLMMs
cwsd1 = glmer(species_diversity ~ 1 + (1|aru), family = "poisson", data = cbma_water)
cwsd2 = glmer(species_diversity ~ abs((1/gh)) + (1|aru), family = "poisson", data = cbma_water)
cwsd3 = glmer(species_diversity ~ altitude + (1|aru), family = "poisson", data = cbma_water)
cwsd4 = glmer(species_diversity ~ water_int + (1|aru), family = "poisson", data = cbma_water)
cwsd5 = glmer(species_diversity ~ water_int*abs((1/gh)) + (1|aru), family = "poisson", data = cbma_water)
cwsd6 = glmer(species_diversity ~ water_int*altitude + (1|aru), family = "poisson", data = cbma_water)
cwsd7 = glmer(species_diversity ~ water_int*scale(mas) + (1|aru), family = "poisson", data = sswma_water)
AICctab(cwsd1,cwsd2,cwsd3,cwsd4,cwsd5,cwsd6,cwsd7, nobs = 12361, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(cwsd5) #as aridity increases so does species diversity
assump(cwsd5)
Anova(cwsd5, type = "III")
contrasts = glht(cwsd5, linfct = mcp(water_int = "Tukey"))
summary(contrasts) #presence of water increases species diversity across ws_site 1, and ws_site 2 has higher species diversity than ws_site 1 with restrected access, but is not significantly different (p = 0.0698) from ws_site 1 with access to water

