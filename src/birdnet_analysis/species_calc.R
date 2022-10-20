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
# library(lintr)

sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("lwma"))
arid_species = NULL
water_species = NULL


# Data Cleaning and Organization ------------------------------------------
for(s in sites){
  setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/")
  if(s == "lwma"){
    # load lwma bird data
    load("birdnet_data/lwma_aru_results.Rdata")
    data <- lwma_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
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
    data = sswma_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
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
    data = cbma_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
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
    data = kiowa_aru_results %>%
      mutate(hour_utc = hour(date_time),
             date = date(date_time)) %>%
      group_by(site,aru,common_name,date_time,date) %>%
      summarise(n = n()) %>%
      pivot_wider(names_from = common_name, values_from = n, values_fill = 0) %>%
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`,`Northern Mockingbird`, `Blue Grosbeak`, `Dickcissel`)
    
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

arid_species$ghacross_sites = scale(arid_species$gh) # scaling observed aridity across sites

# Day bin - Binning arid species df by day, obtaining average aridity by day --------
arid_species2 = arid_species %>%
  # group_by(site,date,hour_utc,mas)%>%
  group_by(site,date)%>%
  summarise(num_noca = sum(`Northern Cardinal`),
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
            hist_across = mean(ghsite_scaled) #%>% #historic aridity scaled across sites sites
  )

a3 = arid_species2 %>% # data binned by date
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)), # observed aridity scaled within sites
         arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5)), # observed aridity scaled across sites
         hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)), #historic aridity scaled within sites
         hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5))) #%>% #historic aridity scaled across sites
  # mutate(mas = as.numeric(as.character(mas)))
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
save(a3, file = "species_ag_day_bin.Rdata")

# Day bin - Plotting Bird Species Vocalizations across  and aridity with day-binned data-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Aridity gradient sites plotted against day-binned aridity
ggplot(data = a3,
       aes(x=as.factor(arid_within), y=num_casp, color = site)) +
  geom_boxplot()+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Cassin's Sparrow")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/casp_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)

# Day bin - Water supplementation dataset binned by day -----------------------------
### Separating SSWMA and CBMA water supplementation sites, will need to analyze separately
## SSWMA water supplementation

water_species$ghacross_sites = scale(water_species$gh)
ws_sswma = water_species %>%
  dplyr::filter(site == "sswma") %>% 
  group_by(aru,date,hour_utc)%>%
  summarise(num_noca = sum(`Northern Cardinal`),
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
           summarise(num_noca = sum(`Northern Cardinal`),
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

# Day bin - Water Supplementation plotting Cardinal Vocalizations across  and aridity with day-binned data-----------------
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

# Day bin - Statistical Analyses ----------------------------------------------------
library(glmmTMB)
# Number of cardinal vocalizations across the aridity gradient
casp1 = glmer(num_casp ~ gh_obs + (1|site), family = "poisson", data = arid_species2)
summary(dick1)

zip_casp1 <- glmmTMB(num_casp ~ gh_obs*site + (1|site), data = a3, ziformula=~1, family=poisson)
summary(zip_casp1)

# observed aridity scaled within sites
# a3$aridx = interaction(a3$arid_within, a3$site)
# dick2 = glmer(num_casp ~ aridx + (1|site), family = "poisson", data = a3)
# summary(dick2)
# contrasts = glht(dick2, linfct = mcp(aridx = "Tukey"))
# summary(contrasts)

a3$aridx = interaction(a3$arid_within, a3$site)
zip_casp2 <- glmmTMB(num_casp ~ aridx + (1|site), data = a3, ziformula=~1, family=poisson)
summary(zip_casp2)
casp_emms = emmeans(zip_casp2, "aridx")
pairs(casp_emms)
# emmeans significance grouping
# inter.test1 = emmeans(zip_casp2, "aridx", type = "response")
# cld(inter.test1, Letter = "abcdefg")
# # multcomp comparisons
# contrasts = glht(zip_casp2, linfct = mcp(aridx = "Tukey"))
# summary(contrasts)

# eff_size(casp_emms, sigma = sigma(casp_emms),edf = 394) # can't get to work, ignore for now

# observed aridity scaled across sites
casp3 = glmer(num_casp ~ arid_across*site + (1|site), family="poisson", data = arid_species2)
summary(casp3)

zip_casp3 = glmmTMB(num_casp ~ arid_across*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_casp3)

# historic aridity scaled within sites
casp4 = glmer(num_casp ~ hist_within*hour_utc + (1|site), family = "poisson", data = arid_species2)
summary(casp4)

zip_casp4 = glmmTMB(num_casp ~ hist_within*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_casp4)

# historic aridity scaled across sites
casp5 = glmer(num_vocals ~ ghsite_scaled*scale(mas) + (1|site), family = "poisson", data = arid_species2)
summary(casp5)

aridx = interaction(a3$hist_across,a3$site)
zip_casp5 = glmmTMB(num_casp ~ hist_across*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_casp5)

zip_casp5.5 = glmmTMB(num_casp ~ aridx + (1|site), ziformula=~1, family=poisson, data = a3)
contrasts = glht(zip_casp5.5, linfct = mcp(aridx = "Tukey"))
summary(contrasts)


# Day bin - Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
ws_sswma$waridx = interaction(ws_sswma$arid_within,ws_sswma$ws_site,ws_sswma$water)
zwss_casp <- glmmTMB(num_casp ~ waridx + (1|ws_site), data = ws_sswma, ziformula=~1, family=poisson)
summary(zwss_casp)
casp_wsswma = emmeans(zwss_casp, "waridx")
pairs(casp_wsswma)

### CBMA Site
ws_cbma$waridx = interaction(ws_cbma$arid_within,ws_cbma$ws_site,ws_cbma$water)
zwsc_casp <- glmmTMB(num_casp ~ waridx + (1|ws_site), data = ws_cbma, ziformula=~1, family=poisson)
summary(zwsc_casp)
casp_wsswma = emmeans(zwsc_casp, "waridx")
pairs(casp_wsswma)



# Binning minutes after sunrise (MAS) into three sections (early, mid, late)--------
a4 = arid_species %>%
  # group_by(site,date,hour_utc,mas)%>%
  group_by(site,date,mas)%>%
  summarise(num_noca = sum(`Northern Cardinal`),
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
            hist_across = mean(ghsite_scaled), #%>% #historic aridity scaled across sites
            mas_num = as.numeric(mas)
  )

 a4$arid_within = cut(a4$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
 a4$arid_across = cut(a4$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
 a4$hist_within = cut(a4$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
 a4$hist_across = cut(a4$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
a4$mas_bin = cut(a4$mas_num, breaks = 3, labels = c("early","mid","late"))

# mutate(mas = as.numeric(as.character(mas)))
# MAS - Plotting Bird Species Vocalizations across  and aridity with day-binned data-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Aridity gradient sites plotted against mas-binned aridity
ggplot(data = a4,
       aes(x=as.factor(arid_within), y=num_blgr, color = site)) +
  facet_wrap(~mas_bin)+
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

# MAS binned - Water supplementation dataset binned by day -----------------------------
### Separating SSWMA and CBMA water supplementation sites, will need to analyze separately
## SSWMA water supplementation

water_species$ghacross_sites = scale(water_species$gh)
ws_sswma = water_species %>%
  dplyr::filter(site == "sswma") %>% 
  group_by(aru,date,hour_utc,mas)%>%
  summarise(num_noca = sum(`Northern Cardinal`),
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
            hist_across = mean(ghsite_scaled), #%>% #historic aridity scaled across sites
            mas_num = as.numeric(mas)
  ) %>%
  mutate(mas_bin = cut(mas_num, breaks = 3, labels = c("early","mid","late")),
         # creating water site (ws_site) 1,2,3
         ws_site = if_else(aru == "ws01" | aru == "ws02" | aru == "ws03" | aru == "ws04" | aru == "ws05", 1, if_else(aru == "ws06" | aru == "ws07" | aru == "ws08" | aru == "ws09" | aru == "ws10", 2,3)))

ws_sswma$arid_within = cut(ws_sswma$arid_within, breaks = 5, labels = c(1,2,3,4,5))
ws_sswma$arid_across = cut(ws_sswma$arid_across, breaks = 5, labels = c(1,2,3,4,5))
ws_sswma$hist_within = cut(ws_sswma$hist_within, breaks = 5, labels = c(1,2,3,4,5))
ws_sswma$hist_across = cut(ws_sswma$hist_across, breaks = 5, labels = c(1,2,3,4,5))

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
  group_by(aru,date,hour_utc,mas)%>%
  summarise(num_noca = sum(`Northern Cardinal`),
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
            hist_across = mean(ghsite_scaled), #%>% #historic aridity scaled across sites
            mas_num = as.numeric(mas)
  ) %>% 
  mutate(ws_site = if_else(aru == "wg01" | aru == "wg02" | aru == "wg03", 1, 2),
         mas_bin = cut(mas_num, breaks = 3, labels = c("early","mid","late")))

ws_cbma$arid_within = cut(ws_cbma$arid_within, breaks = 5, labels = c(1,2,3,4,5))
ws_cbma$arid_across = cut(ws_cbma$arid_across, breaks = 5, labels = c(1,2,3,4,5))
ws_cbma$hist_within = cut(ws_cbma$hist_within, breaks = 5, labels = c(1,2,3,4,5))
ws_cbma$hist_across = cut(ws_cbma$hist_across, breaks = 5, labels = c(1,2,3,4,5))

#Separating out CBMA water sites
ws_cbma1 = ws_cbma %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1)) #1 = water access open

ws_cbma2 = ws_cbma %>%
  filter(ws_site == 2) %>%
  mutate(water = 1)

ws_cbma = rbind(ws_cbma1, ws_cbma2)

save(ws_sswma, file = "water_species_sswma_mas_bin.Rdata")
save(ws_cbma, file = "water_species_cbma_mas_bin.Rdata")

# MAS binned - Water Supplementation plotting Cardinal Vocalizations across  and aridity with day-binned data-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Water supplementation sites plotted against day-binned aridity
ggplot(data = ws_sswma,
       aes(x=as.factor(arid_across), y=num_noca, color = as.factor(ws_site))) +
  geom_boxplot()+
  facet_wrap(~mas_bin)+
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

# MAS binned - Statistical Analyses ----------------------------------------------------
library(glmmTMB)

#Aridity within factor and mas bin have a 0.60 correlation factor
a4$aridx = interaction(a4$mas_bin, a4$site)
zip_blgr_mas <- glmmTMB(num_blgr ~ aridx + (1|site), data = a4, ziformula=~1, family=poisson)
summary(zip_blgr_mas)
blgr_emms = emmeans(zip_blgr_mas, "aridx")
pairs(blgr_emms)

# MAS binned - Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
ws_sswma$waridx = interaction(ws_sswma$mas_bin,ws_sswma$ws_site,ws_sswma$water)
zwss_blgr <- glmmTMB(num_blgr ~ waridx + (1|ws_site), data = ws_sswma, ziformula=~1, family=poisson)
summary(zwss_blgr)
blgr_wsswma = emmeans(zwss_blgr, "waridx")
pairs(blgr_wsswma)

### CBMA Site
ws_cbma$waridx = interaction(ws_cbma$mas_bin,ws_cbma$ws_site,ws_cbma$water)
zwsc_blgr <- glmmTMB(num_blgr ~ waridx + (1|ws_site), data = ws_cbma, ziformula=~1, family=poisson)
summary(zwsc_blgr)
blgr_wsswma = emmeans(zwsc_blgr, "waridx")
pairs(blgr_wsswma)


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
