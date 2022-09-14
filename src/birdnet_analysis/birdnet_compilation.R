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
arus = as.list(c("aru01","aru02","aru03","aru04","aru05","ws01","ws02","ws03","ws04","ws05","ws06","ws11","ws12","ws13","ws14","ws15"))
# arus= as.list(c("aru01","aru02","aru03","aru04","aru05","wg01", "wg02", "wg03", "wg04", "wg05"))

results_final = NULL
for(i in arus){
  setwd(paste0("/Volumes/LaCie/aridity_project/sswma/sswma_audio_files/",i))
  
  # setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/pabu_filtered/",
        # i,"_pabu_filtered"))
  birdnet = list.files(pattern = "BirdNET.results.csv")
  for(j in 1:length(birdnet)){
    
      results_temp = read.csv(birdnet[[j]], header = TRUE, sep = ",", fill = TRUE)
      if(nrow(results_temp) == 0){
        results_temp = rbind(results_temp, data.frame("filepath"= NA, "start" = NA, "end" = NA, "scientific_name" = NA, "common_name" = NA, "confidence" = NA, "lat" = NA, "lon" = NA, "week" = NA, "overlap" = NA, "sensitivity" = NA, "min_conf" = NA, "species_list" = NA, "model" = NA))
      }
      results_temp$date_time = as_datetime(substr(birdnet[[j]],1,15)) #change to 15,30 if you are using bird filtered data
      results_temp$date = date(results_temp$date_time)
      results_temp$time = hms(substr(results_temp$date_time,10,15))
      results_temp$aru = i   
      results_temp$site = "sswma"
    results_final = rbind(results_final,results_temp)
  }

}

sswma_aru_results = results_final %>% dplyr::filter(is.na(confidence) != TRUE)
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
setwd("/Volumes/LaCie/aridity_project/sswma/sswma_audio_files/")
save(sswma_aru_results, file = "sswma_aru_results.Rdata")
gc(reset = TRUE)
setwd("/Volumes/LaCie/aridity_project/")
birdnet_data = rbind(lwma_aru_results,sswma_aru_results,cbma_aru_results,kiowa_aru_results)
save(birdnet_data, file = "birdnet_data.Rdata")

# Combining Site Datasets, adding sunlight and sunaltitude variables, and adding weather data from NiceMapR --------------------------------------
sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("sswma"))
arid_full = NULL
water_full = NULL
for(s in sites){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
  
  if(s == "lwma"){
    load("lwma_aru_results.Rdata")
    data = lwma_aru_results
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/lwma_mesonet.Rdata")
    weather_data = lwma_mesonet
  } else if(s == "sswma"){
    load("sswma_aru_results.Rdata")
    data = sswma_aru_results
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/sswma_mesonet.Rdata")
    weather_data = sswma_mesonet
  } else if(s == "cbma"){
    load("cbma_aru_results.Rdata")
    data = cbma_aru_results
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/cbma_mesonet.Rdata")
    weather_data = cbma_mesonet
  } else if(s == "kiowa"){
    load("kiowa_aru_results.Rdata")
    data = kiowa_aru_results
    load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/mesonet_data/kiowa_mesonet.Rdata")
    weather_data = kiowa_mesonet
  }
  
  
  if(s == "kiowa"){
    tz = "US/Mountain"
    #i want to only subtract 3600s from 06/17/21 and 06/18/21
  } else {
    tz = "US/Central"
  }
  
  data_temp = data %>%
    mutate(date = as_date(date),
           local_time = force_tz(date_time, tz = tz),
           date_time = as_datetime(local_time, tz = "UTC"),
           time = as_hms(date_time),
           hour_local = hour(local_time),
           hour_utc = hour(date_time)) %>%
    # filter(date > "2021-04-30 CDT")%>%
    dplyr::filter(is.na(common_name) == FALSE) %>% 
    group_by(date,time,local_time,date_time,hour_local,hour_utc,lat,lon,aru,site) %>%
    summarise(num_vocals = n(),
              species_diversity = n_distinct(common_name))%>%
    arrange(date_time) 
  
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
  
  data_temp = left_join(data_temp, weather_data, by = "date_time")
  data_missing = data_temp %>%
    dplyr::filter(is.na(mas)==TRUE)
  
  data_temp = data_temp %>%
    mutate(site = factor(site.x, levels=c("lwma","sswma","cbma","kiowa")))%>%
    # dplyr::select(-site.x, -site.y,-hour.x,-hour.y)
    dplyr::select(-hour, -site.y)
  
  data_temp_arid = data_temp%>%
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

# Saving Compilation Results ----------------------------------------------
#Aridity Gradient Data for statistical analyses
full_arid = arid_full %>% #saving it a different name so you don't overwrite it
  dplyr::filter(is.na(mas)==FALSE) %>%
  dplyr::filter(hour_local <13) %>%
  dplyr::filter(year(date) != 2106) %>%
  mutate(arid_bin = cut(arid, 3, labels = c("low","med","high")))

max(full_arid$mas)
which.max(full_arid$mas)
mas_check = full_arid[which.max(full_arid$mas),];mas_check
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
save(full_arid, file = "aridity_gradient_ml.Rdata")
load("aridity_gradient_ml.Rdata")


# Water Supplementation Data Compilation ----------------------------------

full_water = water_full %>% #saving it a different name so you don't overwrite it
  dplyr::filter(is.na(mas)==FALSE) %>%
  dplyr::filter(hour_local <13) %>%
  dplyr::filter(year(date) != 2106) %>%
  dplyr::filter(is.na(mas)==FALSE)


full_water1 = full_water %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = full_water %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)

cbma_full_water = rbind(full_water1, full_water2)


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
max(water_compiled$mas)
which.max(water_compiled$mas)
mas_check = water_compiled[which.max(water_compiled$mas),];mas_check
  
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
save(water_compiled, file = "water_supp_ml.Rdata")
load("water_supp_ml.Rdata")

#Plotting mean num_vocals across sun altitude per hour
ggplot(data = full_arid, aes(x = relh, y = gh, color = site))+ #sun altitude does not overlap between sites, so good metric for time AND location
  geom_line()


# Averaging Data Across Hours and Months ----------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
load("aridity_gradient_ml.Rdata")

#Data is averaged by site, aru, date, and hour across aru and breeding season
arid_dh = full_arid %>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  group_by(site,aru,date,hour_utc) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            mean_species = mean(species_diversity),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh)) %>%
  mutate(gh_bin = cut(mean_gh, 3, labels = c("low","med","high")),
         maxgh_bin = cut(max_gh, 3, labels = c("low","med","high")))

#Summarizing Datat by site, aru, hour for Graphs
arid_hour = full_arid %>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  group_by(site,hour_utc) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            se_vocals = sd(num_vocals)/sqrt(n()),
            mean_species = mean(species_diversity),
            se_species = sd(species_diversity)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            se_gh = sd(gh)/sqrt(n()),
            max_gh = max(gh)) %>%
  mutate(gh_bin = cut(mean_gh, 3, labels = c("low","med","high")),
         maxgh_bin = cut(max_gh, 3, labels = c("low","med","high")))

arid_date = full_arid %>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  mutate(week = week(date_time))%>%
  group_by(site,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            se_vocals = sd(num_vocals)/sqrt(n()),
            mean_species = mean(species_diversity),
            se_species = sd(species_diversity)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            se_gh = sd(gh)/sqrt(n()),
            max_gh = max(gh)) %>%
  mutate(gh_bin = cut(mean_gh, 3, labels = c("low","med","high")),
         maxgh_bin = cut(max_gh, 3, labels = c("low","med","high")))


# Statistical Analyses - Aridity Graident LMMs ----------------------------
#Testing for Collinearity

cor(arid_date[,c(4:13)])
cor.test(cor_arid$num_vocals,cor_arid$species_diversity)
#altitude and mas are highly positively correlated so cannot use them in same model
# Testing for Best Random effect -------------------------------------------
re1 = lmer(log(mean_vocals) ~ mean_gh*scale(date)*hour_utc+(1|site), REML = TRUE, data = arid_dh)
re2 = lmer(log(mean_vocals) ~ mean_gh*scale(date)*hour_utc+(1|aru), REML = TRUE, data = arid_dh)
re3 = lmer(log(mean_vocals) ~ mean_gh*scale(date)*hour_utc+(1|site/aru), REML = TRUE, data = arid_dh)
AICctab(re1,re2,re3, nobs = 9521, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
#best random effect is aru nested in site


#arid_dh dataset
v1 = lmer(log(mean_vocals) ~ mean_gh*scale(date)*hour_utc+(1|site/aru), REML = TRUE, data = arid_dh)
v2 = lmer(log(mean_vocals) ~ max_gh*scale(date)*hour_utc+(1|site/aru), REML = TRUE, data = arid_dh)

AICctab(v1,v2,nobs = 9521, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(v1) #ardity increases, species diversity decreases, no significant effect of scale(mas)
assump(v1)

#Species Diversity LMMs

s1 = lmer(log(mean_species) ~ mean_gh*scale(date)*hour_utc+(1|site/aru), REML = TRUE, data = arid_dh)
s2 = lmer(log(mean_species) ~ max_gh*scale(date)*hour_utc+(1|site/aru), REML = TRUE, data = arid_dh)

AICctab(s1,s2, nobs = 9521, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(s1) #ardity increases, species diversity decreases, no significant effect of scale(mas)
assump(s1)



#Plotting mean num_vocals across sun altitude per hour
ggplot(data = arid_dh, aes(x = mean_gh, y = mean_vocals, color = site))+ #sun altitude does not overlap between sites, so good metric for time AND location
  geom_line()

cbpalette <- c("#56B4E9", "#E69F00", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#creating two panel graph for dew temperature fluctuation and 
hour_vocals = ggplot(data = arid_hour,aes(x=hour_utc, y=log(mean_vocals), color = site)) +
  geom_point(size = 3)+
  # geom_line(size = 1) +
  # geom_errorbar(aes(ymin=mean_vocals-se_vocals, ymax=mean_vocals+se_vocals), width=1,
  #               position=position_dodge(0.0))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log (Mean\nNum.\nVocals)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");hour_vocals

hour_species = ggplot(data = arid_hour,aes(x=hour_utc, y=log(mean_species), color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 3)+
  # geom_line(size = 1) +
  # geom_errorbar(aes(ymin=mean_species-se_species, ymax=mean_species+se_species), width=1,
  #               position=position_dodge(0.0))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log(Mean\nSpecies\nDiversity)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");hour_species

hour_arid = ggplot(data = arid_hour,
                   aes(x=hour_utc, y=mean_gh, color = site)) +
  geom_point(size = 3)+
  # geom_line(size = 1) +
  # geom_errorbar(aes(ymin=mean_gh-se_gh, ymax=mean_gh+se_gh), width=1,
  #               position=position_dodge(0.0))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site",labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_continuous(name = "Hour (UTC)")+
  scale_y_continuous(name = "Mean\nEvaporation\nRate\n(kg/hr)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x= element_text("Mean Sun Altitude (Radians)")
        )+ #change angle to 0 for presentations, 90 for papers
  theme(legend.position = "bottom");hour_arid

hour_out <- plot_grid(hour_vocals, hour_species,
                      hour_arid, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.3));hour_out
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("ml_hour.jpg",plot = last_plot(), width = 8, height = 7.5, units = "in")



hour_species = ggplot(data = arid_hour) +
  geom_line(aes(x = mean_sunalt, y = mean_species, color = site))
hour_out2 = plot_grid(hour_species, hour_arid, align = "v", ncol = 1, rel_heights = c(0.5,0.5));hour_out2


# Graphing results across date --------------------------------------------
cbpalette <- c("#56B4E9", "#E69F00", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Graphs showing vocal trends across date
date_vocals = ggplot(data = arid_date,aes(x=date, y=log(mean_vocals), color = site)) +
  geom_point(size = 3)+
  # geom_line(size = 1) +
  geom_smooth(method = "lm")+
  # geom_errorbar(aes(ymin=mean_vocals-se_vocals, ymax=mean_vocals+se_vocals), width=1,
  #               position=position_dodge(0.0))+
  # geom_smooth(method = "loess")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log (Mean\nNum.\nVocals)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");date_vocals

date_species = ggplot(data = arid_date,aes(x=date, y=mean_species, color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 3)+
  # geom_line(size = 1) +
  geom_smooth(method = "lm")+
  # geom_errorbar(aes(ymin=mean_species-se_species, ymax=mean_species+se_species), width=1,
  #               position=position_dodge(0.0))+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log (Mean\nSpecies\nDiversity")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");date_species

date_arid = ggplot(data = arid_date,aes(x=date, y=mean_gh, color = site)) +
  geom_point(size = 3)+
  # geom_line(size = 1) +
  geom_smooth(method = "lm")+
  # geom_errorbar(aes(ymin=mean_gh-se_gh, ymax=mean_gh+se_gh), width=1,
  #               position=position_dodge(0.0))+
  scale_color_manual(values = cbpalette,name = "Site",labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Mean\nEvaporation\nRate\n(kg/hr)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x= element_text("Mean Sun Altitude (Radians)")
  )+ #change angle to 0 for presentations, 90 for papers
  theme(legend.position = "bottom");date_arid

date_out <- plot_grid(date_vocals,date_species,date_arid, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.3));date_out
  

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("ml_date.jpg",plot = date_out, width = 8, height = 7.5, units = "in")

# Testing for Collinearity ------------------------------------------------
cor_arid = full_arid %>%
  dplyr::select(site,aru,arid,mas,altitude,num_vocals,species_diversity,gh)
cor(cor_arid[,c(11:16)])
cor.test(cor_arid$num_vocals,cor_arid$species_diversity)
#altitude and mas are highly positively correlated so cannot use them in same model
# Testing for Best Random effect -------------------------------------------
re1 = glmer(num_vocals ~ abs((1/gh))*altitude + (1|site), family = "poisson",data = full_arid)
re2 = glmer(num_vocals ~ abs((1/gh))*altitude + (1|aru), family = "poisson",data = full_arid)
re3 = glmer(num_vocals ~ abs((1/gh))*altitude + (1|site/aru), family = "poisson",data = full_arid)
AICctab(re1,re2,re3, nobs = 48128, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
#best random effect is aru nested in site
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
load("aridity_gradient_ml.Rdata")

v1 = glmer(num_vocals ~ 1 + (1|site/aru), family = "poisson", data = full_arid)
v2 = glmer(num_vocals ~ arid + (1|site/aru), family = "poisson", data = full_arid)
v3 = glmer(num_vocals ~ altitude + (1|site/aru), family = "poisson", data = full_arid)
v4 = glmer(num_vocals ~ abs((1/gh)) + (1|site/aru), family = "poisson", data = full_arid)
v5 = glmer(num_vocals ~ relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
v6 = glmer(num_vocals ~ scale(mas) + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
v7 = glmer(num_vocals ~ altitude+abs((1/gh)) + (1|site/aru), family = "poisson", data = full_arid) 
v8 = glmer(num_vocals ~ altitude*abs((1/gh)) + (1|site/aru),  family = "poisson", data = full_arid)
v9 = glmer(num_vocals ~ altitude+relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
v10 = glmer(num_vocals ~ altitude*relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
v11 = glmer(num_vocals ~ scale(mas)+relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
v12 = glmer(num_vocals ~ scale(mas)*relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
AICctab(v1,v2,v3,v4,v7,v8, nobs = 48128, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

install.packages("pscl")
library(pscl)

f1 = formula(num_vocals ~ altitude*gh + (1|site/aru))
zip1 = zeroinfl(f1, dist = "poisson", link = "logit", data = full_arid)
summary(v8) #as altitude increases (further west/increasing time of day) number of vocalizations decrease, and as evaporation rate increases number of vocalizations decrease, interaction between the two has num_vocals decreasing as evaporation rate*altitude increases
assump(v8)


#Mean Species Diversity
sd1 = glmer(species_diversity ~ 1 + (1|site/aru), family = "poisson", data = full_arid)
sd2 = glmer(species_diversity ~ arid + (1|site/aru), family = "poisson", data = full_arid)
sd3 = glmer(species_diversity ~ altitude + (1|site/aru), family = "poisson", data = full_arid)
sd4 = glmer(species_diversity ~ abs((1/gh)) + (1|site/aru), family = "poisson", data = full_arid)
sd5 = glmer(species_diversity ~ relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
sd6 = glmer(species_diversity ~ scale(mas) + (1|site/aru), family = "poisson", data = full_arid) 
sd7 = glmer(species_diversity ~ altitude+abs((1/gh)) + (1|site/aru), family = "poisson", data = full_arid) 
sd8 = glmer(species_diversity ~ altitude*abs((1/gh)) + (1|site/aru), family = "poisson", data = full_arid)
sd9 = glmer(species_diversity ~ altitude+relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
sd10 = glmer(species_diversity ~ altitude*relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
sd11 = glmer(species_diversity ~ scale(mas)+relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
sd12 = glmer(species_diversity ~ scale(mas)*relh + (1|site/aru), family = "poisson", data = full_arid) # large eigenvalue ratio
AICctab(sd1,sd2,sd3,sd4,sd6,sd7,sd8, nobs = 48128, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(sd8) #ardity increases, species diversity decreases, no significant effect of scale(mas)
assump(sd8)


# Aridity Gradient Dataset WITHOUT OUTLIER!!!! ----------------------------
# which.max(full_arid$arid)
# full_arid$arid[34191,]
# out_df = full_arid %>% dplyr::filter(site == "kiowa" && date == "2021-05-19")
# ag_noout = full_arid %>% dplyr::filter(dew > 0.65)
# 
# # No Outlier - Testing for Best Random effect -------------------------------------------
# cor_arid = ag_noout %>%
#   dplyr::select(site,aru,arid,mas,altitude,num_vocals,species_diversity)
# cor(cor_arid[,c(6,9:13)]) #no significant correlations between covariates
# cor.test(cor_arid$num_vocals,cor_arid$species_diversity)
# 
# re1out = glmer(num_vocals ~ altitude + arid + (1|site), family = "poisson",data = ag_noout)
# re2out = glmer(num_vocals ~ altitude + arid + (1|aru), family = "poisson",data = ag_noout)
# re3out = glmer(num_vocals ~ altitude + arid + (1|site/aru), family = "poisson",data = ag_noout)
# AICctab(re1out,re2out,re3out, nobs = 44594, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
# #best random effect is aru nested in site
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
# 
# v1out = glmer(num_vocals ~ 1 + (1|site/aru), family = "poisson", data = ag_noout)
# v2out = glmer(num_vocals ~ arid + (1|site/aru), family = "poisson", data = ag_noout)
# v3out = glmer(num_vocals ~ altitude + (1|site/aru), family = "poisson", data = ag_noout)
# v4out = glmer(num_vocals ~ altitude+arid + (1|site/aru), family = "poisson", data = ag_noout)
# v5out = glmer(num_vocals ~ scale(mas) + (1|site/aru), family = "poisson", data = ag_noout)
# v6out = glmer(num_vocals ~ scale(mas)+arid + (1|site/aru), family = "poisson", data = ag_noout)
# 
# AICctab(v1out,v2out,v3out,v4out,v5out,v6out, nobs = 44594, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
# 
# 
# summary(v4out) #as altitude increases (further west/later in the day) number of vocalizations decrease, and as aridity decreases number of vocalizations increase
# # Anova(v4, type = "III")
# # contrasts = glht(v4, linfct = mcp(site = "Tukey"))
# # summary(contrasts)
# 
# # Mean Species Diversity = No Aridity Outlier! ----------------------------
# #Mean Species Diversity - No Outlier
# s1out = glmer(species_diversity ~ 1 + (1|site/aru), family = "poisson", data = ag_noout)
# s2out = glmer(species_diversity ~ arid + (1|site/aru), family = "poisson", data = ag_noout)
# s3out = glmer(species_diversity ~ altitude + (1|site/aru), family = "poisson", data = ag_noout)
# s4out = glmer(species_diversity ~ altitude+arid + (1|site/aru), family = "poisson", data = ag_noout)
# s5out = glmer(species_diversity ~ scale(mas) + (1|site/aru), family = "poisson", data = ag_noout)
# s6out = glmer(species_diversity ~ scale(mas)+arid + (1|site/aru), family = "poisson", data = ag_noout)
# 
# AICctab(s1out,s2out,s3out,s4out,s5out,s6out, nobs = 44594, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
# 
# 
# 
# 
# summary(s2out) #as aridity increases, species diversity decreases
# summary(s6out) #as aridity increases, species diversity decreases, no effect of scale(mas)
# summary(s4out) #as aridity increases, species diversity decreases, no effect of altitude


# Water Supplementation Graphs --------------------------------------------
water_compiled$water_int = interaction(as.factor(water_compiled$ws_site),as.factor(water_compiled$water))


# Water Supplementation Data - Summarized by Date use for Graphs ----------


water_date = water_compiled %>%
  mutate(date = date(date_time))%>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  dplyr::filter(date < "2021-08-07") %>%
  arrange(site,date) %>%
  group_by(site,ws_site,water,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            se_vocals = sd(num_vocals)/sqrt(n()),
            mean_species = mean(species_diversity),
            se_species = sd(species_diversity)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(abs(1/gh)))

# SSWMA Water Supplementation Plots ---------------------------------------

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
wsvocals_day = ggplot(data = water_date %>%dplyr::filter(site == "sswma"),
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
  scale_y_continuous(name = "Log\n(Mean\nNum.\nVocals)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none");wsvocals_day

#Boxplot for Water SSWMA vocals Diversity
boxplot_sswma_vocals = ggplot(data = water_aru %>%
                                 dplyr::filter(site == "sswma"),
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
        legend.margin=margin(t=-20));boxplot_sswma_vocals
sswmaw_vocals_plots = plot_grid(wsvocals_day, boxplot_sswma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_vocals_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("sswmaw_vocals_boxplots.jpg",plot = boxplot_sswma_vocals, width = 8, height = 7.5, units = "in")

wsspecies_day = ggplot(data = water_date %>%dplyr::filter(site == "sswma"),
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
  geom_rect(data=sswma2_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.1, inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean\nSpecies\nDiversity")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none");wsspecies_day

#Boxplot for Water SSWMA Species Diversity
boxplot_sswma_species = ggplot(data = water_aru %>%
                                 dplyr::filter(site == "sswma"),
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
        legend.margin=margin(t=-20));boxplot_sswma_species
sswmaw_species_plots = plot_grid(wsspecies_day, boxplot_sswma_species,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_species_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("sswmaw_species_boxplots.jpg",plot = boxplot_sswma_species, width = 8, height = 7.5, units = "in")

# CBMA Water Plots --------------------------------------------------------
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
wcvocals_day = ggplot(data = water_date %>%dplyr::filter(site == "cbma"),
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
        legend.position = "none");wcvocals_day

#Boxplot for Water cbma vocals Diversity
boxplot_cbma_vocals = ggplot(data = water_aru %>%
                                dplyr::filter(site == "cbma"),
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
        legend.margin=margin(t=-20));boxplot_cbma_vocals
cbmaw_vocals_plots = plot_grid(wcvocals_day, boxplot_cbma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));cbmaw_vocals_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbmaw_vocals_boxplots.jpg",plot = boxplot_cbma_vocals, width = 8, height = 7.5, units = "in")

wcspecies_day = ggplot(data = water_date %>%dplyr::filter(site == "cbma"),
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
        legend.position = "none");wcspecies_day

#Boxplot for Water cbma Species Diversity
boxplot_cbma_species = ggplot(data = water_aru %>%
                                 dplyr::filter(site == "cbma"),
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

# Water Supplementation Data Summarized by ARU use for Statistics ----------------------------

water_aru = water_compiled %>%
  mutate(date = date(date_time))%>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  dplyr::filter(date < "2021-08-07") %>%
  group_by(site,aru,date,hour_utc,ws_site,water,water_int) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_vocals = mean(num_vocals),
            se_vocals = sd(num_vocals)/sqrt(n()),
            mean_species = mean(species_diversity),
            se_species = sd(species_diversity)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh), #don't need to transform because this is summary data
            max_gh = max(gh)
            # mean_gh = mean(abs(1/gh)),
            # max_gh = max(abs(1/gh))
            )

# Water Supplementation - Best Random Effect ------------------------------
hist(log(water_aru$mean_vocals))
wre1 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date)+ (1|ws_site), REML = TRUE, data = water_aru)
wre2 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date) + (1|aru), REML = TRUE, data = water_aru)
wre3 = lmer(log(mean_vocals) ~ mean_gh*water_int*scale(date) + (1|ws_site/aru), REML = TRUE, data = water_aru)
AICctab(wre1,wre2,wre3, nobs = 845, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

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
water_compiled$water_int = interaction(as.factor(water_compiled$ws_site),as.factor(water_compiled$water))


sswma_water = water_compiled %>%
  dplyr::filter(site == "sswma")
cbma_water = water_compiled %>%
  dplyr::filter(site == "cbma")
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






# Checking Unique Species at each Site ------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
load("birdnet_data.Rdata")
lwma_ag = birdnet_data %>%
  dplyr::filter(site == "lwma")%>%
  dplyr::filter(common_name == "Northern Shoveler")%>%
  summarize(species = unique(common_name))


# Comparing against Manual Detections in Validation Files -----------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
arus = as.list(c("aru01","aru04","aru05","ws01","ws02","ws03","ws04","ws05","ws06","ws11","ws12","ws13","ws14","ws15"))
valid_final = NULL
for(i in arus){
   setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/",
                i))
   birdnet = list.files(pattern = ".txt")
   for(j in 1:length(birdnet)){
      
      valid_temp = read.table(birdnet[[j]], header = FALSE, sep = "", fill = TRUE)
      names(valid_temp) = c("start","end","id") 
      valid_temp$id = substr(valid_temp$id,1,4)
      valid_temp$date_time = as_datetime(substr(birdnet[[j]],1,15))
      valid_temp$date = date(valid_temp$date_time)
      valid_temp$time = hms(substr(valid_temp$date_time,12,19))
      valid_temp$aru = i   
      valid_temp$site = "sswma"
      valid_final= rbind(valid_final,valid_temp)
   }
   
}

#Saving validation data
sswma_valid = valid_final
valid_species_hour = sswma_valid %>%
   filter(time@hour <=13) %>% 
   group_by(date_time,aru,site) %>%
   summarise(n = n(),
species_diversity = n_distinct(id))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
write.csv(valid_species_hour, "validation_diversity.csv", row.names = FALSE)
# rename(time = "time@hour") %>%

mmp_valid = full_join(valid_species_hour,species_hour, by = c("date_time","aru")) %>%
   rename("mmp_num_vocals" = n,
          "mmp_species_diversity" = species_diversity.x,
          "birdnet_num_vocals" = num_vocals,
          "birdnet_species_diversity" = species_diversity.y) %>%
   select(mmp_num_vocals,
          mmp_species_diversity,
          birdnet_num_vocals,
          birdnet_species_diversity) %>%
   mutate(mmp_num_vocals = as.numeric(mmp_num_vocals),
          mmp_species_diversity = as.numeric(mmp_species_diversity),
          birdnet_num_vocals = as.numeric(birdnet_num_vocals),
          birdnet_species_diversity = as.numeric(birdnet_species_diversity)) %>%
   na.omit()
mmp_cor = cor(mmp_valid[, 3:6], use ="everything")
cor.test(mmp_valid$mmp_species_diversity,mmp_valid$birdnet_species_diversity)
cor.test(mmp_valid$mmp_num_vocals, mmp_valid$birdnet_num_vocals)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
write.csv(mmp_valid, "mmp_valid_data.csv", row.names = FALSE)

# Correlating Validation Data ---------------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")

valid_data = read.csv("validation_acoustic_indices.csv", header = TRUE) %>%
   na.omit()


valid_cor = cor(valid_data[, 4:17], use ="everything")
cor.test(valid_data$user_num_vocals,valid_data$birdnet_unfiltered_num_vocals) #r = 0.244 (0.091-0.386), p = 0.002, 
cor.test(valid_data$num_vocals,valid_data$pabu_aei) #r =-0.463, p = 0.035

melted_valid <- melt(valid_cor)
head(melted_valid)
# lower_valid <- valid_cor
# 
# # Make lower triangular matrix by setting NA to upper triangular part:
# lower_valid[upper.tri(lower_valid)] <- NA
# lower_m_valid <- melt(lower_valid, na.rm = TRUE)


# Ggplot lower triangular correlation matrix:

ggplot(data = melted_valid, aes(x = Var1, y = Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient2(midpoint = 0.5, mid ="grey70", 
                        limits = c(-1, +1)) +
   labs(title = "Correlation Matrix of Validation Data, Acoustic Indices, and BirdNet-Lite Analysis", 
        x = "", y = "", fill = "Correlation \n Measure") +
   theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
         axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
         axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
         legend.title = element_text(face="bold", colour="brown", size = 10)) +
   geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
             fontface = "bold", size = 5)+
   theme_classic(base_size = 20) +
   theme(axis.text.x = element_text(angle = 45, hjust=1))



cbma_data$date_time = cbma_data$local_time
miss_weather = anti_join(results_final, cbma_data, by = "date_time")
combined = full_join(results_final, cbma_data, by = "date_time")
combined2 = combined %>%
  filter(year(date_time) != 1970)
combined2$temperature = na.approx(combined2$temperature) #approximate dewpoint temperatuer
combined2$relh = na.approx(combined2$relh) #approximate air temperature
combined2$pressure = na.approx(combined2$pressure) #approximate relative humidity


setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean")
 # save(combined, file = "cbma_aru_species_results.Rdata")
save(combined2, file = "cbma_wg_spcecies_results.Rdata")
write.csv(combined2, file = "cbma_wg_spcecies_results.csv", row.names = FALSE)

cbma_wg_aci = read.csv("aci_water_cbma.csv", header = TRUE)
cbma_comb_data = full_join(combined2%>%dplyr::filter(aru == "wg01"),cbma_wg_aci%>%dplyr::filter(aru == "wgo1"), by = "aru")


 combined %>%
   group_by(Common.name) %>%
   filter(date_time == max(date_time)) %>%
   ungroup()

 species_hour = combined2 %>%
   group_by(date, time@hour, Common.name) %>%
   summarise(n = n(),
             temp = mean(temperature),
             relh = mean(relh)) %>%
   rename(time = "time@hour") %>%
   filter(time <=13) %>% filter(is.na(Common.name) == FALSE)
 
 ggplot(data = species_hour, aes(x = time, y = n, color = Common.name))+
   geom_line()+
   # geom_bar(stat = "identity", position = position_dodge(0.2))+
   scale_y_continuous(limits = c(0,1250)) +
   facet_wrap(~Common.name)
   
m1 = lmer(n ~ scale(date) * time + (1|Common.name), data = species_hour, REML= FALSE)
summary(m1)
m2 = lmer(n ~ temp*relh*scale(date) + (1|Common.name), data = species_hour, REML= FALSE)
summary(m2)
