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
library(performance)
library(generics) #find rows with the same values across dataframes
library(lintr)

sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("lwma"))
arid_species = NULL
water_species = NULL

for(s in sites){
  setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  if(s == "lwma"){
    # load lwma bird data
    load("birdnet_data/lwma_aru_results.Rdata")
    data = lwma_aru_results %>%
      dplyr::filter(common_name == "Northern Cardinal" | common_name == "House Finch" | common_name == "Cassin's Sparrow" | common_name == "Eastern Meadowlark" | common_name == "Western Meadowlark" | common_name == "Lark Sparrow")
    #load 2021 mesonet data
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
    
  } else if(s == "sswma"){
    # load sswma bird data
    load("birdnet_data/sswma_aru_results.Rdata")
    data = sswma_aru_results%>%
      dplyr::filter(common_name == "Northern Cardinal" | common_name == "House Finch" | common_name == "Cassin's Sparrow" | common_name == "Eastern Meadowlark" | common_name == "Western Meadowlark" | common_name == "Lark Sparrow")
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
    
    
  } else if(s == "cbma"){
    # load cbma bird data
    load("birdnet_data/cbma_aru_results.Rdata")
    data = cbma_aru_results%>%
      dplyr::filter(common_name == "Northern Cardinal" | common_name == "House Finch" | common_name == "Cassin's Sparrow" | common_name == "Eastern Meadowlark" | common_name == "Western Meadowlark" | common_name == "Lark Sparrow")
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
    
    
  } else if(s == "kiowa"){
    # load kiowa bird data
    load("birdnet_data/kiowa_aru_results.Rdata")
    data = kiowa_aru_results%>%
      dplyr::filter(common_name == "Northern Cardinal" | common_name == "House Finch" | common_name == "Cassin's Sparrow" | common_name == "Eastern Meadowlark" | common_name == "Western Meadowlark" | common_name == "Lark Sparrow")
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
    
  }
  
  # if(s == "kiowa"){
  #   tz = "US/Mountain"
  #   #i want to only subtract 3600s from 06/17/21 and 06/18/21
  # } else {
  #   tz = "US/Central"
  # }
  
  data_temp = data %>%
    mutate(date = as_date(date),
           local_time = force_tz(date_time, tz = tz),
           date_time = as_datetime(local_time, tz = "UTC"),
           time = as_hms(date_time),
           hour_local = hour(local_time),
           hour_utc = hour(date_time)) %>%
    # filter(date > "2021-04-30 CDT")%>%
    dplyr::filter(is.na(common_name) == FALSE) %>% 
    group_by(site) 
  
  if(s == "kiowa"){
    kiowa_time_bad = data_temp %>% 
      dplyr::filter(hour_local ==12)%>%
      mutate(date_time = date_time-3600,
             local_time = local_time-3600,
             time = as_hms(date_time),
             hour_local = hour(local_time),
             hour_utc = hour(date_time))
    kiowa_time_good = data_temp %>% dplyr::filter(hour_local !=12)
    data_temp = rbind(kiowa_time_bad,kiowa_time_good)
    
  } else {
    
  }
  
  # data_temp = left_join(data_temp, wd, by = "date_time")
  # data_missing = data_temp %>%
  #   dplyr::filter(is.na(mas)==TRUE)
  
  data_temp = data_temp %>%
    mutate(month_day = format(as.Date(date_time), "%m-%d"))
  
  hd2 = full_join(wd,hd, by = c("month_day","mas")) %>%
    arrange(month_day, mas)
  hd2$hour_utc = hour(hd2$date_time)
  hd2$gh_hobs = na.approx(hd2$gh_hobs, na.rm = FALSE)
  hd2$ghhobs_scaled = na.approx(hd2$ghhobs_scaled, na.rm = FALSE)
  hd2$ghmean_time = na.approx(hd2$ghmean_time, na.rm = FALSE)
  hd2$ghsite_scaled = na.approx(hd2$ghsite_scaled, na.rm = FALSE)
  
  data_temp2 = left_join(data_temp, hd2, by = c("date_time")) %>%
    arrange(date_time)
  
  data_temp3 = data_temp2 %>%
    mutate(site = factor(site.x, levels=c("lwma","sswma","cbma","kiowa")))%>%
    # dplyr::select(-site.x, -site.y,-hour.x,-hour.y)
    dplyr::select(-hour, -site.y, -hour_utc.y, -month_day.y) %>%
    rename(hour_utc = "hour_utc.x",
           month_day = "month_day.x")
  
  data_temp_arid = data_temp3%>%
    filter(aru == "aru01" | aru == "aru02"| aru == "aru03"| aru == "aru04"| aru == "aru05")
  
  data_temp_water = data_temp3 %>%
    filter(aru == "ws01" | aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05" |
             aru == "ws06" | aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10" | 
             aru == "ws11" | aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15" |
             aru == "wg01" | aru == "wg02"| aru == "wg03"| aru == "wg04"| aru == "wg05")
  
  arid_species = rbind(data_temp_arid,arid_species) %>%
    filter(is.na(gh_hobs)==FALSE)

  water_species = rbind(data_temp_water,water_species) %>%
    filter(is.na(gh_hobs)==FALSE)
}

arid_species$ghacross_sites = scale(arid_species$gh)
arid_species2 = arid_species %>%
  group_by(common_name, site,date_time, mas)%>%
  summarise(num_vocals = n(),
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            ghwithin_scaled = mean(ghobs_scaled), # observed aridity scaled within sites
            ghacross_sites = mean(ghacross_sites), # observed aridity scaled across sites
            ghhobs_scaled = mean(ghhobs_scaled), #historic aridity scaled within sites
            ghsite_scaled = mean(ghsite_scaled)) %>% #historic aridity scaled across sites
  mutate(mas = as.numeric(as.character(mas)))

water_species$ghacross_sites = scale(water_species$gh)
water_species2 = water_species %>%
  group_by(common_name, site,date_time, mas)%>%
  summarise(num_vocals = n(),
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            ghwithin_scaled = mean(ghobs_scaled), # observed aridity scaled within sites
            ghacross_sites = mean(ghacross_sites), # observed aridity scaled across sites
            ghhobs_scaled = mean(ghhobs_scaled), #historic aridity scaled within sites
            ghsite_scaled = mean(ghsite_scaled)) %>% #historic aridity scaled across sites
  mutate(mas = as.numeric(as.character(mas)))

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
save(arid_species2, file = "general_species_ag.Rdata")
save(water_species2, file = "general_species_water.Rdata")

# Plotting Cardinal Vocalizations across time and aridity -----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#
ggplot(data = arid_species2 %>% filter(common_name == "Eastern Meadowlark" | common_name == "Western Meadowlark"),
       aes(x=ghwithin_scaled, y=num_vocals, color = site)) +
  # geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle("Species: Meadowlarks") +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
# for observed aridity the trend for cardinals in cbma is positive, while for historic aridity it is negative
# 2021 aridity was drier than historic aridity levels
# relationship could be driven by a few outliers

# Creating cbma, cardinal dataset
card_cbma = arid_species2 %>% filter(common_name == "Northern Cardinal") %>% filter(site == "cbma")

# Plotting observed aridity against historic aridity ----------------------
ggplot(data = arid_species2,aes(x=gh_obs, y=gh_hist, color = site)) +
  # geom_point()+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_x_continuous(name = "observed aridity")+
  scale_y_continuous(name = "historic aridity")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")

# Statistical Analyses ----------------------------------------------------
#Number of cardinal vocalizations across the aridity gradient
noca1 = lmer(num_vocals ~ gh_obs*scale(mas) + (1|site), REML = FALSE, data = arid_species2)
summary(noca1)

noca2 = lmer(num_vocals ~ ghhobs_scaled*scale(mas) + (1|site), REML = FALSE, data = arid_species2)
summary(noca2)

noca3 = lmer(num_vocals ~ ghsite_scaled*scale(mas) + (1|site), REML = FALSE, data = arid_species2)
summary(noca3)

# Number of species per aru and site --------------------------------------

lwma_species = arid_species %>%
  dplyr::filter(site == "lwma") %>%
  group_by(common_name) %>%
  summarise(num_species = n())

sswma_species = arid_species %>%
  dplyr::filter(site == "sswma") %>%
  group_by(common_name) %>%
  summarise(num_species = n())
  # group_by(site,aru)%>%
  # summarize(n_distinct(common_name))

cbma_species = arid_species %>%
  dplyr::filter(site == "cbma") %>%
  group_by(common_name) %>%
  summarise(num_species = n())
  # group_by(site,aru)%>%
  # summarize(n_distinct(common_name))

kiowa_species = arid_species %>%
  dplyr::filter(site == "kiowa") %>%
  group_by(common_name) %>%
  summarise(num_species = n())
  # group_by(site,aru)%>%
  # summarize(n_distinct(common_name))

# Number of species per aru and site, water supplementation--------------------------------------

sswma_species_water = water_species %>%
  dplyr::filter(site == "sswma") %>%
  group_by(site,aru)%>%
  summarize(n_distinct(common_name))

cbma_species_water = water_species %>%
  dplyr::filter(site == "cbma") %>%
  group_by(site,aru)%>%
  summarize(n_distinct(common_name))

diff1 = inner_join(lwma_species,sswma_species, by = "common_name")
diff2 = inner_join(diff1,cbma_species, by = "common_name")
diff3 = inner_join(diff2,kiowa_species, by = "common_name")



sswma_lwma_diff = setdiff(sswma_species,lwma_species)
sswma_lwma_same = intersect(sswma_species,lwma_species)

cbma_sswma_diff = setdiff(cbma_species,sswma_species)
cbma_sswma_same = intersect(cbma_species,sswma_species)

kiowa_cbma_diff = setdiff(kiowa_species,cbma_species)
kiowa_cbma_same = intersect(kiowa_species,cbma_species)
