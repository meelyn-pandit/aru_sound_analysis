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
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`, `Scissor-tailed Flycatcher`)
    
    # data = lwma_aru_results %>%
    #   dplyr::filter(common_name == "Northern Cardinal" | common_name == "House Finch" | common_name == "Cassin's Sparrow" | common_name == "Eastern Meadowlark" | common_name == "Western Meadowlark" | common_name == "Lark Sparrow")
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
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`,`Scissor-tailed Flycatcher`)
    
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
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`,`Scissor-tailed Flycatcher`)
    
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
      dplyr::select(`Northern Cardinal`, `House Finch`, `Cassin's Sparrow`, `Eastern Meadowlark`, `Western Meadowlark`, `Lark Sparrow`,`Scissor-tailed Flycatcher`)
    
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

# Binning arid species df by day, obtaining average aridity by day --------
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

# Plotting Cardinal Vocalizations across  and aridity with day-binned data-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Aridity gradient sites plotted against day-binned aridity
ggplot(data = a3,
       aes(x=as.factor(arid_across), y=num_noca, color = site)) +
  geom_boxplot()+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
ggsave("results/noca_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)
# for observed aridity the trend for cardinals in cbma is positive, while for historic aridity it is negative
# 2021 aridity was drier than historic aridity levels
# relationship could be driven by a few outliers

#Aridity gradient sites plotted against day-binned aridity
ggplot(data = a3,
       aes(x=as.factor(arid_across), y=num_mela, color = site)) +
  geom_boxplot()+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Meadowlark, Aridity Gradient")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
ggsave("results/noca_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)


# Water supplementation dataset binned by day -----------------------------
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




save(wss2, file = "water_species_sswma_day_bin.Rdata")
save(wsc2, file = "water_species_cbma_day_bin.Rdata")

# Water Supplementation plotting Cardinal Vocalizations across  and aridity with day-binned data-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Water supplementation sites plotted against day-binned aridity
ggplot(data = wss2,
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

ggplot(data = wss2, aes(x=date, y=num_casp, 
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


# CBMA Water Station Rectangle Graphs -------------------------------------
cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), xmax=as_date("2021-06-04"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), xmax=as_date("2021-07-19"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2

#CBMA Vocals Graph
ggplot(data = wsc2, aes(x=date, y= num_noca, 
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

#Aridity gradient sites plotted against day-binned aridity
ggplot(data = a3,
       aes(x=as.factor(arid_within), y=num_noca, color = site)) +
  geom_boxplot()+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Number of Vocals")+
  theme_classic(base_size = 20) +
  ggtitle(paste0("Species: Northern Cardinal, Water Supplementation")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
ggsave("results/noca_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)


# Binning minutes after sunrise (mas) into three sections (early, mid, late)--------

a4 = arid_species %>%
  # group_by(site,date,hour_utc,mas)%>%
  group_by(site,date,mas)%>%
  summarise(num_noca = sum(`Northern Cardinal`),
            num_hofi = sum(`House Finch`),
            num_casp = sum(`Cassin's Sparrow`),
            num_eame = sum(`Eastern Meadowlark`),
            num_weme = sum(`Western Meadowlark`),
            num_lasp = sum(`Lark Sparrow`),
            num_mela = num_eame+num_weme,
            gh_obs = mean(gh), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghobs_scaled), # observed aridity scaled within sites
            arid_across = mean(ghacross_sites), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(ghsite_scaled), #%>% #historic aridity scaled across sites
            mas_bin = cut(as.numeric(mas), breaks = 3, labels = c("early","mid","late"))
  )


# Plotting observed aridity against historic aridity ----------------------
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

# Statistical Analyses ----------------------------------------------------
library(glmmTMB)
# Number of cardinal vocalizations across the aridity gradient
noca1 = glmer(num_noca ~ gh_obs + (1|site), family = "poisson", data = arid_species2)
summary(noca1)

zip_noca1 <- glmmTMB(num_noca ~ gh_obs*site + (1|site), data = a3, ziformula=~1, family=poisson)
summary(zip_noca1)

# observed aridity scaled within sites
a3$aridx = interaction(a3$arid_within, a3$site)
noca2 = glmer(num_noca ~ aridx + (1|site), family = "poisson", data = a3)
summary(noca2)
contrasts = glht(noca2, linfct = mcp(aridx = "Tukey"))
summary(contrasts)

zip_noca2 <- glmmTMB(num_noca ~ aridx + (1|site), data = a3, ziformula=~1, family=poisson)
summary(zip_noca2)
# emmeans significance grouping
# inter.test1 = emmeans(zip_noca2, "aridx", type = "response")
# cld(inter.test1, Letter = "abcdefg")
# # multcomp comparisons
# contrasts = glht(zip_noca2, linfct = mcp(aridx = "Tukey"))
# summary(contrasts)
noca_emms = emmeans(zip_noca2, "aridx", type = "response")
pairs(noca_emms)
# eff_size(noca_emms, sigma = sigma(noca_emms),edf = 394) # can't get to work, ignore for now

# observed aridity scaled across sites
noca3 = glmer(num_noca ~ arid_across*site + (1|site), family="poisson", data = arid_species2)
summary(noca3)

zip_noca3 = glmmTMB(num_noca ~ arid_across*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_noca3)

# historic aridity scaled within sites
noca4 = glmer(num_noca ~ hist_within*hour_utc + (1|site), family = "poisson", data = arid_species2)
summary(noca4)

zip_noca4 = glmmTMB(num_noca ~ hist_within*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_noca4)

# historic aridity scaled across sites
noca5 = glmer(num_vocals ~ ghsite_scaled*scale(mas) + (1|site), family = "poisson", data = arid_species2)
summary(noca5)

aridx = interaction(a3$hist_across,a3$site)
zip_noca5 = glmmTMB(num_noca ~ hist_across*site + (1|site), ziformula=~1, family=poisson, data = a3)
summary(zip_noca5)

zip_noca5.5 = glmmTMB(num_noca ~ aridx + (1|site), ziformula=~1, family=poisson, data = a3)
contrasts = glht(zip_noca5.5, linfct = mcp(aridx = "Tukey"))
summary(contrasts)


# Meadowark Zero-inflated poisson models ----------------------------------

# observed aridity
zip_mela1 <- glmmTMB(num_mela ~ gh_obs*hour_utc*site + (1|site), data = arid_species2, ziformula=~1, family=poisson)
summary(zip_mela1)

# observed aridity scaled within sites
zip_mela2 <- glmmTMB(num_mela ~ ghwithin_scaled*scale(hour_utc)*site + (1|site), data = arid_species2, ziformula=~1, family=poisson)
summary(zip_mela2)

# observed aridity scaled across sites
zip_mela3 = glmmTMB(num_mela ~ ghacross_sites*scale(hour_utc)*site + (1|site), ziformula=~1, family=poisson, data = arid_species2)
summary(zip_mela3)

# historic aridity scaled within sites
zip_mela4 = glmmTMB(num_mela ~ ghhobs_scaled*scale(hour_utc)*site + (1|site), ziformula=~1, family=poisson, data = arid_species2)
summary(zip_mela4)

# historic aridity scaled across sites
zip_mela5 = glmmTMB(num_mela ~ ghsite_scaled*scale(hour_utc)*site + (1|site), ziformula=~1, family=poisson, data = arid_species2)
summary(zip_mela5)
contrasts = glht(zip_mela5, linfct = mcp(site = "Tukey"))
summary(contrasts)

aridx = interaction(arid_species2$ghsite_scaled,arid_species2$site)
zip_mela5.5 = glmmTMB(num_mela ~ aridx + (1|site), ziformula=~1, family=poisson, data = arid_species2)
summary(zip_mela5.5)
contrasts = glht(zip_mela5.5, linfct = mcp(aridx = "Tukey"))
summary(contrasts)

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
