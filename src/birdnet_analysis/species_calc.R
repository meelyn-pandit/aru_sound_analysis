library(dplyr) #data manipulation
library(tidyverse)
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
library(emmeans)
library(glmmTMB) # zero-inflated analyses for mixed models
library(pscl) # zero-inflated analyses for non-mixed models
library(performance) #test to see if models are zero-inflated

# library(lintr)

sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("lwma"))
arid_species = NULL
water_species = NULL


# Data Cleaning and Organization ------------------------------------------
for(s in sites){
  # setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  if(s == "lwma"){
    # load lwma bird data
    load("data_clean/birdnet_data/lwma_aru_results.Rdata")
    data <- lwma_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      dplyr::summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
    #load 2021 mesonet data
    load("data_clean/mesonet_data/lwma_mesonet.Rdata")
    wd = lwma_mesonet %>%
      mutate(ghobs_scaled = scale(gh))
    wd$month_day = format(as.Date(wd$date_time), "%m-%d")
    labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    # load("historic_weather_data/lwma_wh.Rdata")
    # hd = lwma_wh %>%
    #   dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    #   dplyr::filter(is.na(mas)==FALSE)%>%
    #   dplyr::filter(is.na(gh_hobs)==FALSE)%>%
    #   group_by(month_day, mas) %>%
    #   summarise_all(funs(mean))
    tz = "US/Central"
    
  } else if(s == "sswma"){
    # load sswma bird data
    load("data_clean/birdnet_data/sswma_aru_results.Rdata")
    data = sswma_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      dplyr::summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
    load("data_clean/mesonet_data/sswma_mesonet.Rdata")
    wd = sswma_mesonet%>%
      mutate(ghobs_scaled = scale(gh))
    wd$month_day = format(as.Date(wd$date_time), "%m-%d")
    labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    
    #load historic data from 2005-2021
    # load("data_clean/historic_weather_data/sswma_wh.Rdata")
    # hd = sswma_wh %>%
    #   dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    #   dplyr::filter(is.na(mas)==FALSE)  %>%
    #   dplyr::filter(is.na(gh_hobs)==FALSE)  %>%
    #   group_by(month_day, mas) %>%
    #   summarise_all(funs(mean))
    tz = "US/Central"
    
    
  } else if(s == "cbma"){
    # load cbma bird data
    load("data_clean/birdnet_data/cbma_aru_results.Rdata")
    data = cbma_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      dplyr::summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
    load("data_clean/mesonet_data/cbma_mesonet.Rdata")
    wd = cbma_mesonet%>%
      mutate(ghobs_scaled = scale(gh))
    wd$month_day = format(as.Date(wd$date_time), "%m-%d")
    labels = seq(-725,755,5) #creating bin labels, going from lowest value to the highest value-5
    wd$mas = cut(wd$mas, seq(-725,760,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    # load("data_clean/historic_weather_data/cbma_wh.Rdata")
    # hd = cbma_wh %>% 
    #   dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    #   dplyr::filter(is.na(mas)==FALSE)  %>%
    #   dplyr::filter(is.na(gh_hobs)==FALSE)  %>%
    #   group_by(month_day, mas) %>%
      # summarise_all(funs(mean))
    tz = "US/Central"

    
  } else if(s == "kiowa"){
    # load kiowa bird data
    load("data_clean/birdnet_data/kiowa_aru_results.Rdata")
    data = kiowa_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      dplyr::summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
    load("data_clean/mesonet_data/kiowa_mesonet.Rdata")
    wd = kiowa_mesonet%>%
      mutate(ghobs_scaled = scale(gh))
    wd$month_day = format(as.Date(wd$date_time), "%m-%d")
    labels = seq(-750,730,5) #creating bin labels, going from lowest value to the highest value-5
    wd$mas = cut(wd$mas, seq(-750,735,5),labels = labels, right = FALSE)
    
    #load historic data from 2005-2021
    # load("data_clean/historic_weather_data/kiowa_wh.Rdata")
    # hd = kiowa_wh %>% 
    #   dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    #   dplyr::filter(is.na(mas)==FALSE)  %>%
    #   dplyr::filter(is.na(gh_hobs)==FALSE)  %>%
    #   group_by(month_day, mas) %>%
    #   summarise_all(funs(mean))
    tz = "US/Mountain"
    
  }
  
  # if(s == "kiowa"){
  #   tz = "US/Mountain"
  #   #i want to only subtract 3600s from 06/17/21 and 06/18/21
  # } else {
  #   tz = "US/Central"
  # }
  
  data_temp = data %>%
    mutate(date = as_date(date_time),
           local_time = force_tz(date_time, tz = tz),
           date_time = as_datetime(local_time, tz = "UTC"),
           time = as_hms(date_time),
           hour_local = hour(local_time),
           hour_utc = hour(date_time),
           month_day = format(as.Date(date_time), "%m-%d")) %>%
    # filter(date > "2021-04-30 CDT")%>%
    # dplyr::filter(is.na(common_name) == FALSE) %>% 
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
  
  
  # hd2 = full_join(wd,hd, by = c("month_day","mas")) %>%
  #   arrange(month_day, mas)
  # hd2$hour_utc = hour(hd2$date_time)
  # hd2$gh_hobs = na.approx(hd2$gh_hobs, na.rm = FALSE)
  # hd2$ghhobs_scaled = na.approx(hd2$ghhobs_scaled, na.rm = FALSE)
  # hd2$ghmean_time = na.approx(hd2$ghmean_time, na.rm = FALSE)
  # hd2$ghsite_scaled = na.approx(hd2$ghsite_scaled, na.rm = FALSE)
  
  data_temp2 = left_join(data_temp, wd, by = c("date_time")) %>%
    arrange(date_time)
  
  data_temp3 = data_temp2 %>%
    dplyr::select(-site.y,-month_day.y) %>%
    dplyr::rename(site = "site.x",
                  month_day = "month_day.x")
    
  #   mutate(site = factor(site.x, levels=c("lwma","sswma","cbma","kiowa")))%>%
  #   dplyr::select(-hour, -site.y, -hour_utc.y, -month_day.y) %>%
  #   rename(hour_utc = "hour_utc.x",
  #          month_day = "month_day.x")
  
  data_temp_arid = data_temp3 %>%
    filter(aru == "aru01" | aru == "aru02"| aru == "aru03"| aru == "aru04"| aru == "aru05")
  
  data_temp_water = data_temp3 %>%
    filter(aru == "ws01" | aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05" |
             aru == "ws06" | aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10" | 
             aru == "ws11" | aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15" |
             aru == "wg01" | aru == "wg02"| aru == "wg03"| aru == "wg04"| aru == "wg05")
  
  arid_species = rbind(data_temp_arid,arid_species)
  # %>%
  #   filter(is.na(gh_hobs)==FALSE)

  water_species = rbind(data_temp_water,water_species) 
  # %>%
  #   filter(is.na(gh_hobs)==FALSE)
}

# arid_species$arid_within = na.approx(aw$arid_within, na.rm = FALSE)

### Adding variable calculations
arid_species2 = arid_species %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  dplyr::filter(is.na(temp) == FALSE) %>%
  mutate(date = date(date_time),
         date_time = round_date(date_time, unit = "minute"),
         pres = na.approx(pres, na.rm = FALSE), # approximating missing pressure data
         # gh = ((25+(19*ws2m))* 1 *(max_sat(temp)-(relh/100))),
         evap_wind = (evap_rate(u2 = ws2m, # evaporation rate in mm/day
                                p = pres, 
                                t = temp, 
                                rh = (relh/100), 
                                z0 = 0.03)), 
         evap_1 = (evap_rate(u2 = 1, # evaporation rate with windspeed set at 1m/s
                             p = pres, 
                             t = temp, 
                             rh = (relh/100), 
                             z0 = 0.03))) %>%
  # dplyr::mutate(ewlwvp = ewl/vpd) %>% # in g/h/kpA
  dplyr::mutate(ew_vol = evap_wind*0.1, # volume of water (mL) being evaporated per day from a circular pan with a radius of 10cm, units are mL/cm^2/day
                e1_vol = evap_1*0.1) %>% # volume of water (mL) being evaporated per day from a circular pan with a radius of 10cm, units are mL/cm^2/day
  dplyr::mutate(atten_alpha04 = att_coef(4000, temp, relh, Pa = (pres/1000)),
                atten_alpha08 = att_coef(8000, temp, relh, Pa = (pres/1000)),
                atten_alpha12 = att_coef(12000, temp, relh, Pa = (pres/1000))) %>%
  dplyr::mutate(atten_dist04 = aud_range(f = 4000, 
                                         T_cel = temp, 
                                         h_rel = relh, 
                                         Pa = (pres/1000)),
                atten_dist08 = aud_range(f = 8000, 
                                         T_cel = temp, 
                                         h_rel = relh, 
                                         Pa = (pres/1000)),
                atten_dist12 = aud_range(f = 12000, 
                                         T_cel = temp, 
                                         h_rel = relh, 
                                         Pa = (pres/1000)),
                mas_num = as.numeric(as.character(mas)),
                site_labels = factor(site, levels = c("lwma","sswma","cbma","kiowa"),
                                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))) %>%
  dplyr::mutate(mas_bin = cut(mas_num, 
                              include.lowest = TRUE, 
                              breaks = c(-400,-5,125,255,400), 
                              labels = c("0","1","2","3")))
save(arid_species2, file = "data_clean/species_ag_all.Rdata")

# Day bin - Aridity Gradient - Data Organization  --------
arid_species3 = arid_species2 %>%
  group_by(site,site_labels,date,mas_bin) %>%
  dplyr::summarise(num_noca = sum(`Northern Cardinal`),
            num_hofi = sum(`House Finch`),
            num_casp = sum(`Cassin's Sparrow`),
            num_eame = sum(`Eastern Meadowlark`),
            num_weme = sum(`Western Meadowlark`),
            num_lasp = sum(`Lark Sparrow`),
            num_stfl = sum(`Scissor-tailed Flycatcher`),
            num_blgr = sum(`Blue Grosbeak`),
            num_nomo = sum(`Northern Mockingbird`),
            num_dick = sum(`Dickcissel`),
            num_mela = num_eame+num_weme,
            ew_volmean = mean(ew_vol, na.rm = TRUE)
            # gh_obs = mean(gh), #observed aridity in 2021
            # gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            # arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            # arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            # hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            # hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites sites
  )

save(arid_species3, file = "data_clean/species_ag_mas_bin.Rdata")

# Day bin - Aridity Gradient - Data Plots -----------------

### color-blind friendly color palette
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

load('data_clean/species_ag_day_bin.Rdata')
#Aridity gradient sites plotted against day-binned aridity
ggplot(data = arid_species3,
       aes(x=ew_volmean, y=num_nomo, color = site_labels)) +
  # geom_boxplot()+
  # geom_point() +
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Northern Mockingbird")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom") +
  geom_smooth(method = lm) +
  facet_grid(~mas_bin)
ggsave("results/casp_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)


ag_contrasts_convar_time(arid_species3,
                         arid_species3$num_noca,
                         arid_species3$ew_volmean)

ag_contrasts_convar_site(arid_species3,
                         arid_species3$num_noca,
                         arid_species3$ew_volmean)


# Day bin - Aridity Gradient - Statistical Analyses ----------------------------------------------------
### Date-bin Number of Vocalizations
ggplot(a3,
       aes(x=arid_within,y=num_hofi,color=site)) + 
  geom_jitter() + 
  geom_boxplot(alpha=0.2) + 
  facet_wrap(~site)

hist(a3$num_hofi)
# Number of individual species vocalizations across the aridity gradient
a3$aridx = interaction(a3$arid_within,a3$site)
zip_hofi1 = glm.nb(num_hofi ~ arid_within*site, data = a3)
summary(zip_hofi1)
check_zeroinflation(zip_hofi1)
aov(zip_hofi1)
Anova(zip_hofi1,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = a3,
      type='III')
TukeyHSD(aov(zip_hofi1))

# historic aridity scaled within sites
zip_hofi4 = glmmTMB(num_hofi ~ hist_within*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_hofi4)

# historic aridity scaled across sites
hofi5 = glmer(num_vocals ~ ghsite_scaled*scale(mas) + (1|site), family = "poisson", data = arid_species2)
summary(hofi5)

aridx = interaction(a3$hist_across,a3$site)
zip_hofi5 = glmmTMB(num_hofi ~ hist_across*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_hofi5)

zip_hofi5.5 = glmmTMB(num_hofi ~ aridx + (1|site), ziformula=~1, family=poisson, data = a3)
contrasts = glht(zip_hofi5.5, linfct = mcp(aridx = "Tukey"))
summary(contrasts)


# Day bin - Water supplementation - Data Organization -----------------------------
### Separating SSWMA and CBMA water supplementation sites, will need to analyze separately
## SSWMA water supplementation

water_species$ghacross_sites = scale(water_species$gh)
ws_sswma = water_species %>%
  dplyr::filter(site == "sswma") %>% 
  group_by(aru,date,hour_utc)%>%
  dplyr::summarise(num_noca = sum(`Northern Cardinal`),
            num_hofi = sum(`House Finch`),
            num_casp = sum(`Cassin's Sparrow`),
            num_eame = sum(`Eastern Meadowlark`),
            num_weme = sum(`Western Meadowlark`),
            num_lasp = sum(`Lark Sparrow`),
            num_stfl = sum(`Scissor-tailed Flycatcher`),
            num_blgr = sum(`Blue Grosbeak`),
            num_nomo = sum(`Northern Mockingbird`),
            num_dick = sum(`Dickcissel`),
            num_mela = num_eame+num_weme,
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites
  ) %>%
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
         arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5)),
         hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
         hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
  # creating water site (ws_site) 1,2,3
  ws_site = if_else(aru == "ws01" | aru == "ws02" | aru == "ws03" | aru == "ws04" | aru == "ws05", 1, if_else(aru == "ws06" | aru == "ws07" | aru == "ws08" | aru == "ws09" | aru == "ws10", 2,3)))

#Separating out SSWMA water sites
ws_sswma1 = ws_sswma %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0))

ws_sswma2 = ws_sswma %>%
  dplyr::filter(ws_site == 2) %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0))

ws_sswma3 = ws_sswma %>%
  dplyr::filter(ws_site == 3) %>%
  mutate(water = 0)
ws_sswma = rbind(ws_sswma1, ws_sswma2, ws_sswma3)


##CBMA water supplementation
ws_cbma = water_species %>%
  dplyr::filter(site == "cbma") %>% #creating water sites (ws)
         group_by(aru,date,hour_utc)%>%
           dplyr::summarise(num_noca = sum(`Northern Cardinal`),
                     num_hofi = sum(`House Finch`),
                     num_casp = sum(`Cassin's Sparrow`),
                     num_eame = sum(`Eastern Meadowlark`),
                     num_weme = sum(`Western Meadowlark`),
                     num_lasp = sum(`Lark Sparrow`),
                     num_stfl = sum(`Scissor-tailed Flycatcher`),
                     num_blgr = sum(`Blue Grosbeak`),
                     num_nomo = sum(`Northern Mockingbird`),
                     num_dick = sum(`Dickcissel`),
                     num_mela = num_eame+num_weme,
                     gh_obs = mean(gh), #observed aridity in 2021
                     gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
                     arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
                     arid_across = mean(ghacross_sites), # observed aridity scaled across sites
                     hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
                     hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites
           ) %>% 
  mutate(ws_site = if_else(aru == "wg01" | aru == "wg02" | aru == "wg03", 1, 2),
         arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
                  arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5)),
                  hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
                  hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)))

#Separating out CBMA water sites
ws_cbma1 = ws_cbma %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1)) #1 = water access open

ws_cbma2 = ws_cbma %>%
  filter(ws_site == 2) %>%
  mutate(water = 1)

ws_cbma = rbind(ws_cbma1, ws_cbma2)

save(ws_sswma, file = "water_species_sswma_day_bin.Rdata")
save(ws_cbma, file = "water_species_cbma_day_bin.Rdata")

# Day bin - Water Supplementation - Data Plots-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Water supplementation sites plotted against day-binned aridity
ggplot(data = ws_sswma,
       aes(x=as.factor(arid_across), y=num_noca, color = as.factor(ws_site))) +
  geom_boxplot()+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Water Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/noca_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)

#SSWMA Water Supplementation Graphs - Day-binned date on x-axis and specific species on y-axis
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

### SSWMA Water Supplementation Rectangle Graphs
ggplot(data = ws_sswma, aes(x=date, y=num_casp, 
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
  scale_y_continuous(name = "Number of Cassin's Sparrows")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/noca_sswma_water_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)

# Day bin - CBMA Water Station Rectangle Graphs -------------------------------------
cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), xmax=as_date("2021-06-04"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), xmax=as_date("2021-07-19"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2

#CBMA Vocals Graph
ggplot(data = ws_cbma, aes(x=date, y= num_noca, 
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
  scale_y_continuous(name = "Northern Cardinal\nVocalizations")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")

# Day bin - Plotting observed aridity against historic aridity ----------------------
ggplot(data = a3,aes(x=gh_obs, y=gh_hist, color = site)) +
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


# Day bin - Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
ws_sswma$waridx = interaction(ws_sswma$arid_within,ws_sswma$ws_site,ws_sswma$water)
zwss_lasp <- glm.nb(num_lasp ~ waridx, data = ws_sswma)
summary(zwss_lasp)
check_zeroinflation(zwss_lasp)
aov(zwss_lasp)
Anova(zwss_lasp,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = ws_sswma,
      type='III')
TukeyHSD(aov(zwss_lasp))

### CBMA Site
ws_cbma$waridx = interaction(ws_cbma$arid_within,ws_cbma$ws_site,ws_cbma$water)
zwsc_lasp <- glm.nb(num_lasp ~ waridx, data = ws_cbma)
summary(zwsc_lasp)
check_zeroinflation(zwsc_lasp)
aov(zwsc_lasp)
Anova(zwsc_lasp,
      contrasts=list(factorA='arid_within',factorB='ws_site',factorC='water'), 
      data = ws_cbma,
      type='III')
TukeyHSD(aov(zwsc_lasp))


# MAS Bin - Aridity Gradient Data Organization -------------------------
a4 = arid_species %>%
  # group_by(site,date,hour_utc,mas)%>%
  group_by(site,mas)%>%
  dplyr::summarise(num_noca = sum(`Northern Cardinal`),
            num_hofi = sum(`House Finch`),
            num_casp = sum(`Cassin's Sparrow`),
            num_eame = sum(`Eastern Meadowlark`),
            num_weme = sum(`Western Meadowlark`),
            num_lasp = sum(`Lark Sparrow`),
            num_stfl = sum(`Scissor-tailed Flycatcher`),
            num_blgr = sum(`Blue Grosbeak`),
            num_nomo = sum(`Northern Mockingbird`),
            num_dick = sum(`Dickcissel`),
            num_mela = num_eame+num_weme,
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites
  )

 a4$mas_num = as.numeric(as.character(a4$mas))
 a4$arid_within = cut(a4$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
 a4$arid_across = cut(a4$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
 a4$hist_within = cut(a4$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
 a4$hist_across = cut(a4$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
a4$mas_bin = cut(a4$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

# mutate(mas = as.numeric(as.character(mas)))
# MAS binned - Aridity Gradient - Plots-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Aridity gradient sites plotted against mas-binned aridity
ggplot(data = a4,
       aes(x=as.factor(mas_bin), y=num_blgr, color = site)) +
  # facet_grid(~mas_bin)+
  geom_boxplot()+
  # geom_jitter(color="black", size=0.4, alpha=0.9) +
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Blue Grosbeak")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/blgr_masbin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)


# MAS binned - Water supplementation - Data Organization -----------------------------
### SSWMA water supplementation

water_species$ghacross_sites = scale(water_species$gh)
ws_sswma = water_species %>%
  dplyr::filter(site == "sswma") %>% 
  mutate(ws_site = if_else(aru == "ws01" | aru == "ws02" | aru == "ws03" | aru == "ws04" | aru == "ws05", 1, if_else(aru == "ws06" | aru == "ws07" | aru == "ws08" | aru == "ws09" | aru == "ws10", 2,3)))

#Separating out SSWMA water sites
ws_sswma1 = ws_sswma %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0))

ws_sswma2 = ws_sswma %>%
  dplyr::filter(ws_site == 2) %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0))

ws_sswma3 = ws_sswma %>%
  dplyr::filter(ws_site == 3) %>%
  mutate(water = 0)
ws_sswma = rbind(ws_sswma1, ws_sswma2, ws_sswma3)

wmas_sswma = ws_sswma %>%
  group_by(ws_site,water,mas) %>%
  dplyr::summarise(num_noca = sum(`Northern Cardinal`),
            num_hofi = sum(`House Finch`),
            num_casp = sum(`Cassin's Sparrow`),
            num_eame = sum(`Eastern Meadowlark`),
            num_weme = sum(`Western Meadowlark`),
            num_lasp = sum(`Lark Sparrow`),
            num_stfl = sum(`Scissor-tailed Flycatcher`),
            num_blgr = sum(`Blue Grosbeak`),
            num_nomo = sum(`Northern Mockingbird`),
            num_dick = sum(`Dickcissel`),
            num_mela = num_eame+num_weme,
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites
  ) 

wmas_sswma$mas_num = as.numeric(as.character(wmas_sswma$mas))
wmas_sswma$arid_within = cut(wmas_sswma$arid_within, breaks = 5, labels = c(1,2,3,4,5))
wmas_sswma$arid_across = cut(wmas_sswma$arid_across, breaks = 5, labels = c(1,2,3,4,5))
wmas_sswma$hist_within = cut(wmas_sswma$hist_within, breaks = 5, labels = c(1,2,3,4,5))
wmas_sswma$hist_across = cut(wmas_sswma$hist_across, breaks = 5, labels = c(1,2,3,4,5))
wmas_sswma$mas_bin = cut(wmas_sswma$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))


##CBMA water supplementation
ws_cbma = water_species %>%
  dplyr::filter(site == "cbma") %>% #creating water sites (ws)
  mutate(ws_site = if_else(aru == "wg01" | aru == "wg02" | aru == "wg03", 1, 2))

#Separating out CBMA water sites
ws_cbma1 = ws_cbma %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1)) #1 = water access open

ws_cbma2 = ws_cbma %>%
  filter(ws_site == 2) %>%
  mutate(water = 1)

ws_cbma = rbind(ws_cbma1, ws_cbma2)

wmas_cbma = ws_cbma %>%
  group_by(ws_site,water,mas)%>%
  dplyr::summarise(num_noca = sum(`Northern Cardinal`),
            num_hofi = sum(`House Finch`),
            num_casp = sum(`Cassin's Sparrow`),
            num_eame = sum(`Eastern Meadowlark`),
            num_weme = sum(`Western Meadowlark`),
            num_lasp = sum(`Lark Sparrow`),
            num_stfl = sum(`Scissor-tailed Flycatcher`),
            num_blgr = sum(`Blue Grosbeak`),
            num_nomo = sum(`Northern Mockingbird`),
            num_dick = sum(`Dickcissel`),
            num_mela = num_eame+num_weme,
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites
  )
  
wmas_cbma$mas_num = as.numeric(as.character(wmas_cbma$mas)) 
wmas_cbma = wmas_cbma %>% 
  dplyr::filter(mas_num <= 400) %>%
  dplyr::filter(mas_num >= -400)
wmas_cbma$arid_within = cut(wmas_cbma$arid_within, breaks = 5, labels = c(1,2,3,4,5))
wmas_cbma$arid_across = cut(wmas_cbma$arid_across, breaks = 5, labels = c(1,2,3,4,5))
wmas_cbma$hist_within = cut(wmas_cbma$hist_within, breaks = 5, labels = c(1,2,3,4,5))
wmas_cbma$hist_across = cut(wmas_cbma$hist_across, breaks = 5, labels = c(1,2,3,4,5))
wmas_cbma$mas_bin = cut(wmas_cbma$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))


save(wmas_sswma, file = "water_species_sswma_mas_bin.Rdata")
save(wmas_cbma, file = "water_species_cbma_mas_bin.Rdata")

# MAS binned - Water Supplementation - Plots-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Water supplementation sites plotted against day-binned aridity
ggplot(data = wmas_sswma,
       aes(x=mas_bin, y=num_noca, color = as.factor(ws_site))) +
  geom_boxplot()+
  # facet_wrap(~mas_bin)+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Water Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/noca_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)

#SSWMA Water Supplementation Graphs - Day-binned date on x-axis and specific species on y-axis
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

### SSWMA Water Supplementation Rectangle Graphs
ggplot(data = ws_sswma, aes(x=date, y=num_casp, 
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
  scale_y_continuous(name = "Number of Cassin's Sparrows")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/noca_sswma_water_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)

# MAS binned - CBMA Water Station Rectangle Graphs -------------------------------------
cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), xmax=as_date("2021-06-04"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), xmax=as_date("2021-07-19"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2

#CBMA Vocals Graph
ggplot(data = ws_cbma, aes(x=date, y= num_noca, 
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
  scale_y_continuous(name = "Northern Cardinal\nVocalizations")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")



# MAS binned - Aridity Gradient - Statistical Analyses ----------------------------------------------------
#Aridity within factor and mas bin have a 0.60 correlation factor
a4$aridx = interaction(a4$mas_bin, a4$site)
nomo_nb <- glm.nb(num_nomo ~ mas_bin*site, data = a4)
# nomo_poi <- glm(num_nomo ~ mas_bin*site, data = a4, family = "poisson")
# nomo_zi = zeroinfl(num_nomo ~ mas_bin*site, data = a4, dist = "poisson")

summary(nomo_nb)
check_zeroinflation(nomo_nb)
aov(nomo_nb)
Anova(nomo_nb,
      contrasts=list(factorA='mas_bin', FactorB ='site'), 
      data = a4,
      type='III')
TukeyHSD(aov(nomo_nb))

# MAS binned - Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
# zwss_nomo <- glmmTMB(num_nomo ~ waridx + (1|ws_site), data = wmas_sswma, ziformula=~1, family=poisson)
# summary(zwss_nomo)
# nomo_wmas_sswma = emmeans(zwss_nomo, "waridx")
# pairs(nomo_wsswma)

wmas_sswma$waridx = interaction(wmas_sswma$mas_bin,wmas_sswma$ws_site,wmas_sswma$water)
wsswma_nomo <- glm.nb(num_nomo ~ mas_bin*ws_site*water, data = wmas_sswma)
# nomo_poi <- glm(num_nomo ~ mas_bin*site, data = a4, family = "poisson")
nomo_zi = zeroinfl(num_nomo ~ mas_bin*ws_site, data = wmas_sswma, dist = "poisson")

summary(wsswma_nomo)
check_zeroinflation(wsswma_nomo)
aov(wsswma_nomo)
Anova(wsswma_nomo,
      contrasts=list(factorA='mas_bin', FactorB ='ws_site', factorC = 'water'), 
      data = wmas_sswma,
      type='III')
wsswma_nomo <- glm.nb(num_nomo ~ waridx, data = wmas_sswma) # have to run model with interaction term to get the tukeyhsd to work
TukeyHSD(aov(wsswma_nomo))

### CBMA Site
wmas_cbma$waridx = interaction(wmas_cbma$mas_bin,wmas_cbma$ws_site,wmas_cbma$water)
zwsc_nomo <- glm.nb(num_nomo ~ mas_bin*ws_site*water, data = wmas_cbma)
nomo_poi <- glm(num_nomo ~ mas_bin*ws_site*water, data = wmas_cbma, family = "poisson")
nomo_zi = zeroinfl(num_nomo ~ mas_bin*ws_site*water, data = wmas_cbma, dist = "negbin")

summary(zwsc_nomo)
check_zeroinflation(zwsc_nomo)
aov(zwsc_nomo)
Anova(zwsc_nomo,
      contrasts=list(factorA='mas_bin', FactorB ='ws_site', factorC = 'water'), 
      data = wmas_cbma,
      type='III')

Anova(zwsc_nomo, type = 'III')
zwsc_nomo <- glm.nb(num_nomo ~ waridx, data = wmas_cbma) # have to run the interaction term for TukeyHSD to work
TukeyHSD(aov(zwsc_nomo))













# Number of species per aru and site --------------------------------------

lwma_species = arid_species %>%
  dplyr::filter(site == "lwma") %>%
  group_by(common_name) %>%
  dplyr::summarise(num_species = n())

sswma_species = arid_species %>%
  dplyr::filter(site == "sswma") %>%
  group_by(common_name) %>%
  dplyr::summarise(num_species = n())
  # group_by(site,aru)%>%
  # summarize(n_distinct(common_name))

cbma_species = arid_species %>%
  dplyr::filter(site == "cbma") %>%
  group_by(common_name) %>%
  dplyr::summarise(num_species = n())
  # group_by(site,aru)%>%
  # summarize(n_distinct(common_name))

kiowa_species = arid_species %>%
  dplyr::filter(site == "kiowa") %>%
  group_by(common_name) %>%
  dplyr::summarise(num_species = n())
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
