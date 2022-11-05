################Stats Analysis###################
library(nlme) #AICc tests
library(lme4) #linear mixed models
library(lmerTest) #get p-values for LMMs
library(ggplot2) #graphing
library(ggthemes) #classical them for ggplot2
library(gridExtra) #create multipanel plots
library(cowplot) #create multipanel plots
library(nortest) #???
library(car) #Anova() function
library(multcomp) #posthoc tests for ANOVA type III effects
library(dplyr) #data organization
library(tidyverse) #data organization
library(suncalc) #cacluate sunrise time and sun position
library(lubridate) #date manipulation
library(hms) #time manipulation
library(bbmle) #AIC
library(lintr)
library(emmeans)

# Loading ACI and BIO datasets --------------------------------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
load("aci_full.Rdata")
aci_full$arid_across = scale(aci_full$gh_obs) #observed aridity in 2021 scaled across sites
aci_full$hist_across = scale(aci_full$gh_hobs) #historic aridity scaled across sites
load("bio_full.Rdata")
bio_full$arid_across = scale(bio_full$gh_obs) #observed aridity in 2021 scaled across sites
bio_full$hist_across = scale(bio_full$gh_hobs) #historic aridity scaled across sites

# Date Bin - Aridity Gradient - Data Organization ---------------------------------

aci_date = aci_full %>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  mutate(week = week(date_time))%>%
  group_by(site,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_aci = mean(left_channel),
            se_aci = sd(left_channel)/sqrt(n()),
            gh_obs = mean(gh_obs), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghsite_scaled), # observed aridity scaled within sites
            arid_across = mean(arid_across), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
            # mas_num = as.numeric(as.character(mas))
  )
### Testing to see if CBMA-KIOWA is the same statistics but positive instead of negative
aci_date$site = factor(aci_date$site, levels = c("lwma","sswma","cbma","kiowa"))
aci_date$arid_within = cut(aci_date$arid_within, include.lowest = TRUE, breaks = 5, labels = c(1,2,3,4,5)) # observed aciity scaled within sites
aci_date$arid_across = cut(aci_date$arid_across, include.lowest = TRUE,breaks = 5, labels = c(1,2,3,4,5)) # observed aciity scaled across sites
aci_date$hist_within = cut(aci_date$hist_within, include.lowest = TRUE,breaks = 5, labels = c(1,2,3,4,5)) #historic aciity scaled within sites
aci_date$hist_across = cut(aci_date$hist_across, include.lowest = TRUE,breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aciity scaled across sites
# aci_date$mas_bin = ifelse(aci_date$mas_num < 0, "predawn", cut(aci_date$mas_num, breaks = 3, labels = c("early","mid","late")))
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
save(aci_date, file = "aci_date.Rdata")

### Biological Diversity Index
bio_date = bio_full %>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  mutate(week = week(date_time))%>%
  group_by(site,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_bio = mean(left_channel),
            se_bio = sd(left_channel)/sqrt(n()),
            gh_obs = mean(gh_obs), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghsite_scaled), # observed aridity scaled within sites
            arid_across = mean(arid_across), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
            # mas_num = as.numeric(as.character(mas))
            )

bio_date$arid_within = cut(bio_date$arid_within, include.lowest = TRUE,breaks = 5, labels = c(1,2,3,4,5)) # observed bioity scaled within sites
bio_date$arid_across = cut(bio_date$arid_across, include.lowest = TRUE,breaks = 5, labels = c(1,2,3,4,5)) # observed bioity scaled across sites
bio_date$hist_within = cut(bio_date$hist_within, include.lowest = TRUE,breaks = 5, labels = c(1,2,3,4,5)) #historic bioity scaled within sites
bio_date$hist_across = cut(bio_date$hist_across, include.lowest = TRUE,breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic bioity scaled across sites
# bio_date$mas_bin = ifelse(bio_date$mas_num < 0, "predawn", cut(bio_date$mas_num, breaks = 3, labels = c("early","mid","late")))
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
save(bio_date, file = "bio_date.Rdata")

# Date Bin - Aridity Gradient - Data Plots -----------------

### color-blind friendly color palette
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

### Date Bin - Aridity Gradient - ACI Boxplots
ggplot(data = aci_date,
       aes(x=as.factor(arid_within), y=mean_aci, color = site)) +
  geom_boxplot()+
  # geom_jitter(width = 0.5)+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_x_discrete(name = "Aridity", labels = c("Extremely\nHumid","Humid","Normal","Arid","Extremely\nArid"))+
  scale_y_continuous(name = "Mean ACI")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Cassin's Sparrow")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aci_date_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

### Date Bin - Aridity Gradient - ACI Dotplots
graph_acidate = aci_date %>%
  dplyr::filter(is.na(date) == FALSE)%>%
  group_by(site,arid_within) %>%
  summarise(n = n(),
            aci_mean = mean(mean_aci),
            aci_se = (sd(mean_aci))/sqrt(n()))

ggplot(data = graph_acidate,
       aes(x=as.factor(arid_within), y=aci_mean, color = site)) +
  geom_point(position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = aci_mean-aci_se, ymax = aci_mean+aci_se), width = 0.2,
                position = position_dodge(0.5))+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_x_discrete(name = "Aridity", labels = c("Extremely\nHumid","Humid","Normal","Arid","Extremely\nArid"))+
  scale_y_continuous(name = "Mean ACI")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Cassin's Sparrow")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aci_date_dotplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

### Date Bin - Aridity Gradient - bio Boxplots
ggplot(data = bio_date,
       aes(x=as.factor(arid_within), y=mean_bio, color = site)) +
  geom_boxplot()+
  # geom_jitter(width = 0.5)+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_x_discrete(name = "Aridity", labels = c("Extremely\nHumid","Humid","Normal","Arid","Extremely\nArid"))+
  scale_y_continuous(name = "Mean bio")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Cassin's Sparrow")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/bio_date_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

### Date Bin - Aridity Gradient - bio Dotplots
graph_biodate = bio_date %>%
  dplyr::filter(is.na(date) == FALSE)%>%
  group_by(site,arid_within) %>%
  summarise(n = n(),
            bio_mean = mean(mean_bio),
            bio_se = (sd(mean_bio))/sqrt(n()))

ggplot(data = graph_biodate,
       aes(x=as.factor(arid_within), y=bio_mean, color = site)) +
  geom_point(position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = bio_mean-bio_se, ymax = bio_mean+bio_se), width = 0.2,
                position = position_dodge(0.5))+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_x_discrete(name = "Aridity", labels = c("Extremely\nHumid","Humid","Normal","Arid","Extremely\nArid"))+
  scale_y_continuous(name = "Mean Bio")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Cassin's Sparrow")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "bottom")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/bio_date_dotplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

# Date Bin - Aridity Gradient - Statistical Analyses ------------------------------------------
hist(scale(aci_date$mean_aci))
### Date-bin ACI - Linear Models with Log of ACI
aci_date$aridx = interaction(aci_date$arid_within, aci_date$site)
date_aci = lm(mean_aci ~ aridx, data = aci_date)
assump(date_aci)
summary(date_aci)
aov(date_aci)
Anova(date_aci,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = aci_date,
      type='III')
Anova(date_aci, type = 'III')
TukeyHSD(aov(date_aci))

### Date-bin BIO
bio_date$aridx = interaction(bio_date$arid_within, bio_date$site)
date_bio = lm(mean_bio ~ aridx, data = bio_date)
assump(date_bio)
summary(date_bio)
aov(date_bio)
Anova(date_bio,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = bio_date,
      type='III')
Anova(date_bio, type = 'III')
TukeyHSD(aov(date_bio))


# Mas Bin - Aridity Gradient - Data Organization-------------------------------
### MAS Bin - ACI
aci_mas = aci_full %>%
  dplyr::filter(is.na(mas)==FALSE)%>%
  group_by(site,mas)%>%
  summarise(mean_sunalt = mean(altitude),
            mean_aci = mean(left_channel),
            # se_aci = sd(left_channel)/sqrt(n()),
            gh_obs = mean(gh_obs), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghsite_scaled), # observed aridity scaled within sites
            arid_across = mean(arid_across), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
  )

aci_mas$mas_num = as.numeric(as.character(aci_mas$mas))
aci_mas$arid_within = cut(aci_mas$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
aci_mas$arid_across = cut(aci_mas$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
aci_mas$hist_within = cut(aci_mas$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
aci_mas$hist_across = cut(aci_mas$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
aci_mas$mas_bin = cut(aci_mas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
save(aci_mas, file = "aci_mas.Rdata")

### MAS - Bio
bio_mas = bio_full %>%
  dplyr::filter(is.na(mas)==FALSE)%>%
  group_by(site,mas)%>%
  summarise(mean_sunalt = mean(altitude),
            mean_bio = mean(left_channel),
            se_bio = sd(left_channel)/sqrt(n()),
            gh_obs = mean(gh_obs), #observed aridity in 2021
            gh_hist = mean(gh_hobs), #historic aridity from 2005-2021
            arid_within = mean(ghsite_scaled), # observed aridity scaled within sites
            arid_across = mean(arid_across), # observed aridity scaled across sites
            hist_within = mean(ghhobs_scaled), #historic aridity scaled within sites
            hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
            )

bio_mas$mas_num = as.numeric(as.character(bio_mas$mas))  
bio_mas$arid_within = cut(bio_mas$arid_within, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled within sites
bio_mas$arid_across = cut(bio_mas$arid_across, breaks = 5, labels = c(1,2,3,4,5)) # observed aridity scaled across sites
bio_mas$hist_within = cut(bio_mas$hist_within, breaks = 5, labels = c(1,2,3,4,5)) #historic aridity scaled within sites
bio_mas$hist_across = cut(bio_mas$hist_across, breaks = 5, labels = c(1,2,3,4,5)) #%>% #historic aridity scaled across sites
bio_mas$mas_bin = cut(aci_mas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
save(bio_mas, file = "bio_mas.Rdata")

# MAS-bin - Aridity Gradient - Statistical Analyses ------------------------------------------
### Mas-bin ACI - Predawn Chorus
aci_mas$aridx = interaction(aci_mas$mas_bin, aci_mas$site)
mas_aci = lm(mean_aci ~ aridx, data = aci_mas )
summary(mas_aci)
assump(mas_aci)
aov(mas_aci)
Anova(mas_aci,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = aci_mas,
      type='III')
Anova(mas_aci, type = 'III')
TukeyHSD(aov(mas_aci))


### Mas-bin bio - Predawn and Dawn Chorus
bio_mas$aridx = interaction(bio_mas$mas_bin, bio_mas$site)
mas_bio = lm(mean_bio ~ aridx, data = bio_mas)
summary(mas_bio)
assump(mas_bio)
aov(mas_bio)
Anova(mas_bio,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = aci_bio,
      type='III')
Anova(mas_bio, type = 'III')
TukeyHSD(aov(mas_bio))

# MAS-ACI  - Aridity Gradient - Dot Plots -----------------------------------------------------------
#ACI Index plotted against minutes after sunrise, grouped by MAS bin
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")

graph_acimas = aci_mas %>%
  dplyr::filter(is.na(mas_bin) == FALSE)%>%
  group_by(site,mas_bin) %>%
  summarise(n = n(),
            aci_mean = mean(mean_aci),
            aci_se = (sd(mean_aci))/sqrt(n()))

### Dot Graph of ACI - MAS bin
ggplot(data = graph_acimas %>% 
         dplyr::filter(is.na(mas_bin)==FALSE), 
       aes(x=mas_bin, y= aci_mean, color = as.factor(site))) +
  geom_point(size = 2, position = position_dodge(0.2))+
  geom_errorbar(aes(ymin = aci_mean-aci_se, ymax = aci_mean+aci_se), width = 0.2,
                position = position_dodge())+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "ACI")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aci_masbin_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

### ACI Mas-bin violin plot
ggplot(data = aci_mas %>% 
         dplyr::filter(is.na(mas)==FALSE), 
       aes(x=mas_bin, y= scale(mean_aci), color = as.factor(site))) +
  geom_violin()+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "ACI")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aci_masbin_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

#ACI Index plotted against aridity - Grouped by Site
ggplot(data = aci_mas %>% 
         dplyr::filter(is.na(mas)==FALSE),
       aes(x=site, y= mean_aci, color = mas_bin)) +
  geom_boxplot()+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "ACI")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aci_masbin_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

# MAS Bio - Aridity Gradient - Dot Plot ------------------------------------------------------
graph_biomas = bio_mas %>%
  dplyr::filter(is.na(mas_bin) == FALSE)%>%
  group_by(site,mas_bin) %>%
  summarise(n = n(),
            bio_mean = mean(mean_bio),
            bio_se = (sd(mean_bio))/sqrt(n()))

### Dot Graph of bio - MAS bin
ggplot(data = graph_biomas %>% 
         dplyr::filter(is.na(mas_bin)==FALSE), 
       aes(x=mas_bin, y= bio_mean, color = as.factor(site))) +
  geom_point(size = 2, position = position_dodge(0.2))+
  geom_errorbar(aes(ymin = bio_mean-bio_se, ymax = bio_mean+bio_se), width = 0.2,
                position = position_dodge())+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "bio")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/bio_masbin_dotplot.jpg", width = 8, height = 6, units = "in", dpi = 600)

ggplot(data = bio_mas %>% 
         dplyr::filter(is.na(mas)==FALSE),
       # %>%
       #   dplyr::filter(mas_bin == "0"),
       aes(x=as.factor(mas_bin), y=mean_bio, color = as.factor(site))) +
  geom_boxplot()+
  # facet_wrap(~mas_bin)+
  # geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "BIO Index")+
  theme_classic(base_size = 20) +
  # ggtitle(paste0("Species: Northern Cardinal")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/bio_masbin_boxplot.jpg", width = 8, height = 6, units = "in", dpi = 600)





# Water Supplementation - Load Data --------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
load("aci_water.Rdata")
aci_water$arid_across = scale(aci_water$gh) #observed aridity in 2021 scaled across sites
aci_water$mas_num = as.numeric(as.character(aci_water$mas))
labels = seq(-140, 380,5)
aci_water = aci_water %>%
  dplyr::filter(is.na(mas_num) == FALSE)
aci_water$mas_bin = cut(aci_water$mas_num, seq(-140,385,5),labels = labels, right = FALSE)

### Load BIO Water Supplementation Data
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
load("bio_water.Rdata")
bio_water$arid_across = scale(bio_water$gh) #observed aridity in 2021 scaled across sites
bio_water$mas_num = as.numeric(as.character(bio_water$mas))
labels = seq(-140, 380,5)
bio_water = bio_water %>%
  dplyr::filter(is.na(mas_num) == FALSE)
bio_water$mas_bin = cut(bio_water$mas_num, seq(-140,385,5),labels = labels, right = FALSE)

# SSWMA - ACI Water Supplementation - Data Organization--------------------
sswma_waci = aci_water %>%
  dplyr::filter(site.x == "sswma") %>%
  # mutate(mas = as.factor(mas)) %>%
  rename(aci = "left_channel") %>%
  mutate(month_day = format(as.Date(date_time), "%m-%d"))

### Load 2021 Mesonet Data

  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/sswma_mesonet.Rdata")
  sswma_mesonet = sswma_mesonet%>%
    mutate(ghobs_scaled = scale(gh),
           mas_num = as.numeric(as.character(mas))) %>%
    dplyr::filter(is.na(mas_num)==FALSE)
  labels = seq(-750,715,5)
  sswma_mesonet$mas_bin = cut(sswma_mesonet$mas_num, seq(-750,720,5),labels = labels, right = FALSE)
  
  #load historic data from 2005-2021
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/sswma_wh.Rdata")
  
  sswma_hist = sswma_wh %>%
    dplyr::select(date_time,date,month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    dplyr::filter(is.na(mas)==FALSE)  %>%
    group_by(month_day, mas)  %>% 
    summarise_all(funs(mean)) %>%
    mutate(mas = as.double(mas),
           mas_num = as.numeric(as.character(mas)))
  # labels = seq(-725,745,5)
  # sswma_hist$mas_bin = cut(sswma_hist$mas_num, seq(-725,750,5),labels = labels, right = FALSE)


### Combine SSWMA ACI Data, Weather Data, Historic Weather Data
  sswma_waci2 = sswma_waci %>%
    group_by(date_time,month_day, mas) 
  sswma_waci2 = full_join(sswma_waci2, sswma_mesonet) %>%
    arrange(date_time)
  
### Approximate missing data values
  sswma_waci2$gh = na.approx(sswma_waci2$gh, na.rm = TRUE)
  sswma_waci2$ghobs_scaled = na.approx(sswma_waci2$ghobs_scaled, na.rm = TRUE)
  
  sswma_waci3 = sswma_waci2 %>%
    rename(arid_within = "ghobs_scaled") %>%
    dplyr::filter(is.na(aci)==FALSE)


# CBMA - ACI Water Supplementation - Data Organization ------------------------

  cbma_waci = aci_water %>%
    dplyr::filter(site.x == "cbma") %>%
    # mutate(mas = as.factor(mas)) %>%
    rename(aci = "left_channel") %>%
    mutate(month_day = format(as.Date(date_time), "%m-%d"))
  
  ### Load 2021 Mesonet Data
  
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/cbma_mesonet.Rdata")
  cbma_mesonet = cbma_mesonet%>%
    mutate(ghobs_scaled = scale(gh),
           mas_num = as.numeric(as.character(mas)),
           month_day = format(as.Date(date_time), "%m-%d")) %>%
    dplyr::filter(is.na(mas_num)==FALSE)
  labels = seq(-750,715,5)
  cbma_mesonet$mas_bin = cut(cbma_mesonet$mas_num, seq(-750,720,5),labels = labels, right = FALSE)
  
  #load historic data from 2005-2021
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/cbma_wh.Rdata")
  
  ### CANNOT GET HISTORIC DATA TO COMBINE WELL, KEEPS GIVING NA VALUES!!!
  cbma_hist = cbma_wh %>%
    dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    dplyr::filter(is.na(mas)==FALSE)  %>%
    group_by(month_day, mas)  %>% 
    summarise_all(funs(mean)) %>%
    mutate(mas = as.double(mas),
           mas_num = as.numeric(as.character(mas)))
  # labels = seq(-725,745,5)
  # cbma_hist$mas_bin = cut(cbma_hist$mas_num, seq(-725,750,5),labels = labels, right = FALSE)
  
  
  ### Combine cbma ACI Data, Weather Data, Historic Weather Data
  cbma_waci2 = cbma_waci %>%
    group_by(date_time,month_day, mas) 
  cbma_waci2 = full_join(cbma_waci2, cbma_mesonet) %>%
    arrange(date_time)
  
  ### Approximate missing data values
  cbma_waci2$gh = na.approx(cbma_waci2$gh, na.rm = TRUE)
  cbma_waci2$ghobs_scaled = na.approx(cbma_waci2$ghobs_scaled, na.rm = TRUE)
  cbma_waci2$arid_across = na.approx(cbma_waci2$arid_across, na.rm = TRUE)
  
  cbma_waci3 = full_join(cbma_waci2, cbma_hist)%>%
    arrange(month_day,mas)
  
  cbma_waci3$gh = na.approx(cbma_waci3$gh, na.rm = TRUE)
  cbma_waci3$ghhobs_scaled = na.approx(cbma_waci3$ghhobs_scaled, na.rm = TRUE) #historic aridity scaled within sites
  cbma_waci3$ghmean_time = na.approx(cbma_waci3$ghmean_time, na.rm = TRUE)
  cbma_waci3$ghsite_scaled = na.approx(cbma_waci3$ghsite_scaled, na.rm = TRUE)
  
  cbma_waci3 = cbma_waci3 %>%
    rename(arid_within = "ghobs_scaled",
           hist_within = "ghhobs_scaled",
           hist_across = "ghsite_scaled") %>%
    dplyr::filter(is.na(aci)==FALSE)
  
  # sswma_waci3 = full_join(sswma_waci3,sswma_hist, by = c("month_day", "mas")) %>%
  #   arrange(month_day, mas)
  # 
  # sswma_waci3$gh_hobs = na.approx(sswma_waci3$gh_hobs)
  # sswma_waci3$ghobs_scaled = na.approx(sswma_waci3$ghobs_scaled, na.rm = TRUE)
  # sswma_waci3$ghhobs_scaled = na.approx(sswma_waci3$ghhobs_scaled, na.rm = TRUE)
  # sswma_waci3$ghmean_time = na.approx(sswma_waci3$ghmean_time, na.rm = TRUE)
  # sswma_waci3$ghsite_scaled = na.approx(sswma_waci3$ghsite_scaled, na.rm = TRUE)
  # 
  # 
  # sswma_waci3 = sswma_waci3 %>%
  #   dplyr::filter(is.na(aci)==FALSE) 
  
  
  
# ### Add historic weather data
# aci_water$arid_across = scale(aci_water$gh) #observed aridity in 2021 scaled across sites
# aci_water$hist_across = scale(aci_water$gh_hobs) #historic aridity scaled across sites
# 
# bio_water$arid_across = scale(bio_water$gh_obs) #observed aridity in 2021 scaled across sites
# bio_water$hist_across = scale(bio_water$gh_hobs) #historic aridity scaled across sites

  
# SSWMA - BIO Water Supplementation - Data Organization--------------------
  sswma_wbio = bio_water %>%
    dplyr::filter(site.x == "sswma") %>%
    # mutate(mas = as.factor(mas)) %>%
    # rename(bio = "left_channel") %>%
    mutate(month_day = format(as.Date(date_time), "%m-%d"))
  
  ### Load 2021 Mesonet Data
  
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/sswma_mesonet.Rdata")
  sswma_mesonet = sswma_mesonet%>%
    mutate(ghobs_scaled = scale(gh),
           mas_num = as.numeric(as.character(mas))) %>%
    dplyr::filter(is.na(mas_num)==FALSE)
  labels = seq(-750,715,5)
  sswma_mesonet$mas_bin = cut(sswma_mesonet$mas_num, seq(-750,720,5),labels = labels, right = FALSE)
  
  #load historic data from 2005-2021
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/sswma_wh.Rdata")
  
  sswma_hist = sswma_wh %>%
    dplyr::select(date_time,date,month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    dplyr::filter(is.na(mas)==FALSE)  %>%
    group_by(month_day, mas)  %>% 
    summarise_all(funs(mean)) %>%
    mutate(mas = as.double(mas),
           mas_num = as.numeric(as.character(mas)))
  # labels = seq(-725,745,5)
  # sswma_hist$mas_bin = cut(sswma_hist$mas_num, seq(-725,750,5),labels = labels, right = FALSE)
  
  
  ### Combine SSWMA bio Data, Weather Data, Historic Weather Data
  sswma_wbio2 = sswma_wbio %>%
    group_by(date_time,month_day, mas) 
  sswma_wbio2 = full_join(sswma_wbio2, sswma_mesonet) %>%
    arrange(date_time)
  
  ### Approximate missing data values
  sswma_wbio2$gh = na.approx(sswma_wbio2$gh, na.rm = TRUE)
  sswma_wbio2$ghobs_scaled = na.approx(sswma_wbio2$ghobs_scaled, na.rm = TRUE)
  
  sswma_wbio3 = sswma_wbio2 %>%
    rename(arid_within = "ghobs_scaled") %>%
    dplyr::filter(is.na(bio)==FALSE)
  
  
# CBMA - BIO Water Supplementation - Data Organization ------------------------
  
  cbma_wbio = bio_water %>%
    dplyr::filter(site.x == "cbma") %>%
    # mutate(mas = as.factor(mas)) %>%
    rename(bio = "left_channel") %>%
    mutate(month_day = format(as.Date(date_time), "%m-%d"))
  
  ### Load 2021 Mesonet Data
  
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/mesonet_data/cbma_mesonet.Rdata")
  cbma_mesonet = cbma_mesonet%>%
    mutate(ghobs_scaled = scale(gh),
           mas_num = as.numeric(as.character(mas)),
           month_day = format(as.Date(date_time), "%m-%d")) %>%
    dplyr::filter(is.na(mas_num)==FALSE)
  labels = seq(-750,715,5)
  cbma_mesonet$mas_bin = cut(cbma_mesonet$mas_num, seq(-750,720,5),labels = labels, right = FALSE)
  
  #load historic data from 2005-2021
  load("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/historic_weather_data/cbma_wh.Rdata")
  
  ### CANNOT GET HISTORIC DATA TO COMBINE WELL, KEEPS GIVING NA VALUES!!!
  cbma_hist = cbma_wh %>%
    dplyr::select(month_day,hour_utc,mas, gh_hobs, ghhobs_scaled,ghmean_time,ghsite_scaled)%>%
    dplyr::filter(is.na(mas)==FALSE)  %>%
    group_by(month_day, mas)  %>% 
    summarise_all(funs(mean)) %>%
    mutate(mas = as.double(mas),
           mas_num = as.numeric(as.character(mas)))
  # labels = seq(-725,745,5)
  # cbma_hist$mas_bin = cut(cbma_hist$mas_num, seq(-725,750,5),labels = labels, right = FALSE)
  
  
  ### Combine cbma bio Data, Weather Data, Historic Weather Data
  cbma_wbio2 = cbma_wbio %>%
    group_by(date_time,month_day, mas) 
  cbma_wbio2 = full_join(cbma_wbio2, cbma_mesonet) %>%
    arrange(date_time)
  
  ### Approximate missing data values
  cbma_wbio2$gh = na.approx(cbma_wbio2$gh, na.rm = TRUE)
  cbma_wbio2$ghobs_scaled = na.approx(cbma_wbio2$ghobs_scaled, na.rm = TRUE)
  cbma_wbio2$arid_across = na.approx(cbma_wbio2$arid_across, na.rm = TRUE)
  
  cbma_wbio3 = full_join(cbma_wbio2, cbma_hist)%>%
    arrange(month_day,mas)
  
  cbma_wbio3$gh = na.approx(cbma_wbio3$gh, na.rm = TRUE)
  cbma_wbio3$ghhobs_scaled = na.approx(cbma_wbio3$ghhobs_scaled, na.rm = TRUE) #historic aridity scaled within sites
  cbma_wbio3$ghmean_time = na.approx(cbma_wbio3$ghmean_time, na.rm = TRUE)
  cbma_wbio3$ghsite_scaled = na.approx(cbma_wbio3$ghsite_scaled, na.rm = TRUE)
  
  cbma_wbio3 = cbma_wbio3 %>%
    # rename(
    #   # arid_within = "ghobs_scaled",
    #        hist_within = "ghhobs_scaled",
    #        hist_across = "ghsite_scaled") %>%
    dplyr::filter(is.na(bio)==FALSE)
  
  
# Day bin - ACI Water supplementation - Data Organization -----------------------------
  sswma_waci4 = sswma_waci3 %>%
    mutate(ws_site = if_else(aru == "ws01_" | aru == "ws02_" | aru == "ws03_" | aru == "ws04_" | aru == "ws05_", 1, if_else(aru == "ws06_" | aru == "ws07_" | aru == "ws08_" | aru == "ws09_" | aru == "ws10_", 2,3))) %>%
    group_by(ws_site,water,date) %>%
    summarise(mean_aci = mean(aci),
              arid_within = mean(arid_within) # observed aridity scaled within sites
              # arid_across = mean(arid_across) # observed aridity scaled across sites
              # hist_within = mean(hist_within), #historic aridity scaled within sites
              # hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
    ) %>%
    mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
           # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
    )
  # hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
  # hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
  # creating water site (ws_site) 1,2,3
  
  #Separating out SSWMA water sites
  ws_sswma1 = sswma_waci4 %>%
    dplyr::filter(ws_site == 1) %>%
    mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0))
  
  ws_sswma2 = sswma_waci4 %>%
    dplyr::filter(ws_site == 2) %>%
    mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0))
  
  ws_sswma3 = sswma_waci4 %>%
    dplyr::filter(ws_site == 3) %>%
    mutate(water = 0)
  waci_sswma = rbind(ws_sswma1, ws_sswma2, ws_sswma3)
  
  
  ##CBMA water supplementation
  cbma_waci4 = cbma_waci3 %>%
    dplyr::filter(site == "cbma") %>% #creating water sites (ws)
    mutate(ws_site = if_else(aru == "wg01_" | aru == "wg02_" | aru == "wg03_", 1, 2)) %>%
    group_by(ws_site,water,date)%>%
    summarise(mean_aci = mean(aci),
              arid_within = mean(arid_within) # observed aridity scaled within sites
              # arid_across = mean(arid_across)
    ) %>%
    mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
           # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
    )
  # hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
  # hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
  # creating water site (ws_site) 1,2# observed aridity scaled across sites
  # hist_within = mean(hist_within), #historic aridity scaled within sites
  # hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
  
  #Separating out CBMA water sites
  ws_cbma1 = cbma_waci4 %>%
    dplyr::filter(ws_site == 1) %>%
    mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1)) #1 = water access open
  
  ws_cbma2 = cbma_waci4 %>%
    filter(ws_site == 2) %>%
    mutate(water = 1)
  
  waci_cbma = rbind(ws_cbma1, ws_cbma2)

save(waci_sswma, file = "water_aci_sswma_day_bin.Rdata")
save(waci_cbma, file = "water_aci_cbma_day_bin.Rdata")


# Day bin - BIO Water supplementation - Data Organization -----------------------------
sswma_wbio4 = sswma_wbio3 %>%
  mutate(ws_site = if_else(aru == "ws01_" | aru == "ws02_" | aru == "ws03_" | aru == "ws04_" | aru == "ws05_", 1, if_else(aru == "ws06_" | aru == "ws07_" | aru == "ws08_" | aru == "ws09_" | aru == "ws10_", 2,3))) %>%
  group_by(ws_site,water,date) %>%
  summarise(mean_bio = mean(bio),
            arid_within = mean(arid_within) # observed aridity scaled within sites
            # arid_across = mean(arid_across) # observed aridity scaled across sites
            # hist_within = mean(hist_within), #historic aridity scaled within sites
            # hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
  ) %>%
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
         # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
         )
# hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
# hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
# creating water site (ws_site) 1,2,3

#Separating out SSWMA water sites
ws_sswma1 = sswma_wbio4 %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0))

ws_sswma2 = sswma_wbio4 %>%
  dplyr::filter(ws_site == 2) %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0))

ws_sswma3 = sswma_wbio4 %>%
  dplyr::filter(ws_site == 3) %>%
  mutate(water = 0)
wbio_sswma = rbind(ws_sswma1, ws_sswma2, ws_sswma3)


##CBMA water supplementation
cbma_wbio4 = cbma_wbio3 %>%
  dplyr::filter(site == "cbma") %>% #creating water sites (ws)
  mutate(ws_site = if_else(aru == "wg01_" | aru == "wg02_" | aru == "wg03_", 1, 2)) %>%
  group_by(ws_site,water,date)%>%
  summarise(mean_bio = mean(bio),
            arid_within = mean(arid_within) # observed aridity scaled within sites
            # arid_across = mean(arid_across)
            ) %>%
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
         # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
         )
# hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
# hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
# creating water site (ws_site) 1,2# observed aridity scaled across sites
# hist_within = mean(hist_within), #historic aridity scaled within sites
# hist_across = mean(hist_across) #%>% #historic aridity scaled across sites

#Separating out CBMA water sites
ws_cbma1 = cbma_wbio4 %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1)) #1 = water access open

ws_cbma2 = cbma_wbio4 %>%
  filter(ws_site == 2) %>%
  mutate(water = 1)

wbio_cbma = rbind(ws_cbma1, ws_cbma2)

save(wbio_sswma, file = "water_bio_sswma_day_bin.Rdata")
save(wbio_cbma, file = "water_bio_cbma_day_bin.Rdata")

# Day bin - Water Supplementation - Data Plots-----------------
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999")
#################light blue, green, orange-yellow, orange, yellow, dark blue, pink, gray

#Water supplementation sites plotted against day-binned aridity
ggplot(data = waci_sswma,
       aes(x=as.factor(arid_across), y=mean_aci, color = as.factor(ws_site))) +
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

### CBMA ACI Rectangle Graph
ggplot(data = waci_cbma, aes(x=date, y= mean_aci, 
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
  scale_y_continuous(name = "Mean ACI")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change to 0 for presentations
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/aci_cbma_water_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)

#Boxplot for CBMA ACI
ggplot(data = waci_cbma,
              aes(x=as.factor(ws_site), y=mean_aci, 
                    color = as.factor(ws_site),
                    fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  # scale_y_continuous(name = "Log\n(Mean\nACI)")+
  scale_y_continuous(name = "Mean ACI")+
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
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
ggsave("results/cbmaw_aci_boxplot.jpg", width = 8, height = 7.5, units = "in")

### CBMA BIO Rectangle Graph
ggplot(data = wbio_cbma, aes(x=date, y= mean_bio, 
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
  scale_y_continuous(name = "Mean BIO")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change to 0 for presentations
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/bio_cbma_water_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)

#Boxplot for CBMA BIO
ggplot(data = wbio_cbma,
       aes(x=as.factor(ws_site), y=mean_bio, 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  # scale_y_continuous(name = "Log\n(Mean\nACI)")+
  scale_y_continuous(name = "Mean BIO")+
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
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
ggsave("results/cbmaw_bio_boxplot.jpg", width = 8, height = 7.5, units = "in")

# Day bin - ACI Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
waci_sswma$waridx = interaction(waci_sswma$arid_within,waci_sswma$ws_site,waci_sswma$water)
waci1 <- lm(mean_aci ~ arid_within*ws_site*water, data = waci_sswma)
summary(waci1)
assump(waci1)
aov(waci1)
Anova(waci1,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = waci_sswma,
      type='III')
waci1 <- lm(mean_aci ~ waridx, data = waci_sswma)
TukeyHSD(aov(waci1))

### CBMA Site
waci_cbma$waridx = interaction(waci_cbma$arid_within,waci_cbma$ws_site,waci_cbma$water)
waci2 <- lm(mean_aci ~ arid_within*ws_site*water, data = waci_cbma)
summary(waci2)
assump(waci2)
aov(waci2)
Anova(waci2,
      contrasts=list(factorA='arid_within',factorB='ws_site',factorC='water'), 
      data = ws_cbma,
      type='III')
waci2 <- lm(mean_aci ~ waridx, data = waci_cbma)
TukeyHSD(aov(waci2))


# Day bin - BIO Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
wbio_sswma$waridx = interaction(wbio_sswma$arid_within,wbio_sswma$ws_site,wbio_sswma$water)
wbio1 <- lm(mean_bio ~ arid_within*ws_site*water, data = wbio_sswma)
summary(wbio1)
assump(wbio1)
aov(wbio1)
Anova(wbio1,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = wbio_sswma,
      type='III')
wbio1 <- lm(mean_bio ~ waridx, data = wbio_sswma)
TukeyHSD(aov(wbio1))

### CBMA Site
wbio_cbma$waridx = interaction(wbio_cbma$arid_within,wbio_cbma$ws_site,wbio_cbma$water)
wbio2 <- lm(mean_bio ~ arid_within*ws_site*water, data = wbio_cbma)
summary(wbio2)
assump(wbio2)
aov(wbio2)
Anova(wbio2,
      contrasts=list(factorA='arid_within',factorB='ws_site',factorC='water'), 
      data = ws_cbma,
      type='III')
wbio2 <- lm(mean_bio ~ waridx, data = wbio_cbma)
TukeyHSD(aov(wbio2))


# MAS bin - ACI Water supplementation - Data Organization -----------------------------

sswma_wmas = sswma_waci3 %>%
  mutate(ws_site = if_else(aru == "ws01_" | aru == "ws02_" | aru == "ws03_" | aru == "ws04_" | aru == "ws05_", 1, if_else(aru == "ws06_" | aru == "ws07_" | aru == "ws08_" | aru == "ws09_" | aru == "ws10_", 2,3)))

ws_sswma1 = sswma_wmas %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0))

ws_sswma2 = sswma_wmas %>%
  dplyr::filter(ws_site == 2) %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0))

ws_sswma3 = sswma_wmas %>%
  dplyr::filter(ws_site == 3) %>%
  mutate(water = 0)
  
sswma_wmas = rbind(ws_sswma1,ws_sswma2,ws_sswma3)

sswma_wmas$mas_num = as.numeric(as.character(sswma_wmas$mas))
sswma_wmas$mas_bin = cut(sswma_wmas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

sswma_wmas2 = sswma_wmas %>%
  group_by(ws_site,water,mas_bin) %>%
  summarise(mean_aci = mean(aci),
            arid_within = mean(arid_within) # observed aridity scaled within sites
            # arid_across = mean(arid_across) # observed aridity scaled across sites
            # hist_within = mean(hist_within), #historic aridity scaled within sites
            # hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
  ) %>%
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
         # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
  )
# hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
# hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
# creating water site (ws_site) 1,2,3


##CBMA water supplementation
cbma_waci4 = cbma_waci3 %>%
  dplyr::filter(site == "cbma") %>% #creating water sites (ws)
  mutate(ws_site = if_else(aru == "wg01_" | aru == "wg02_" | aru == "wg03_", 1, 2)) 
# hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
# hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
# creating water site (ws_site) 1,2# observed aridity scaled across sites
# hist_within = mean(hist_within), #historic aridity scaled within sites
# hist_across = mean(hist_across) #%>% #historic aridity scaled across sites

#Separating out CBMA water sites
ws_cbmamas1 = cbma_waci4 %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1)) #1 = water access open

ws_cbmamas2 = cbma_waci4 %>%
  filter(ws_site == 2) %>%
  mutate(water = 1)

waci_cbmamas = rbind(ws_cbmamas1, ws_cbmamas2) 
waci_cbmamas$mas_num = as.numeric(as.character(waci_cbmamas$mas))
waci_cbmamas$mas_bin = cut(waci_cbmamas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

waci_cbma = waci_cbmamas %>%
  group_by(ws_site,water,mas_bin)%>%
  summarise(mean_aci = mean(aci),
            arid_within = mean(arid_within) # observed aridity scaled within sites
            # arid_across = mean(arid_across)
  ) %>%
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
         # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
  )
save(sswma_wmas2, file = "water_aci_sswma_mas_bin.Rdata")
save(waci_cbma, file = "water_aci_cbma_mas_bin.Rdata")


# MAS bin - BIO Water supplementation - Data Organization -----------------------------
sswma_wmas = sswma_wbio3 %>%
  mutate(ws_site = if_else(aru == "ws01_" | aru == "ws02_" | aru == "ws03_" | aru == "ws04_" | aru == "ws05_", 1, if_else(aru == "ws06_" | aru == "ws07_" | aru == "ws08_" | aru == "ws09_" | aru == "ws10_", 2,3)))

ws_sswma1 = sswma_wmas %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0))

ws_sswma2 = sswma_wmas %>%
  dplyr::filter(ws_site == 2) %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0))

ws_sswma3 = sswma_wmas %>%
  dplyr::filter(ws_site == 3) %>%
  mutate(water = 0)

sswma_wmas = rbind(ws_sswma1,ws_sswma2,ws_sswma3)

sswma_wmas$mas_num = as.numeric(as.character(sswma_wmas$mas))
sswma_wmas$mas_bin = cut(sswma_wmas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

wbio_sswmamas = sswma_wmas %>%
  group_by(ws_site,water,mas_bin) %>%
  summarise(mean_bio = mean(bio),
            arid_within = mean(arid_within) # observed aridity scaled within sites
            # arid_across = mean(arid_across) # observed aridity scaled across sites
            # hist_within = mean(hist_within), #historic aridity scaled within sites
            # hist_across = mean(hist_across) #%>% #historic aridity scaled across sites
  ) %>%
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
         # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
  )
# hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
# hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
# creating water site (ws_site) 1,2,3


##CBMA water supplementation
cbma_wbio4 = cbma_wbio3 %>%
  dplyr::filter(site == "cbma") %>% #creating water sites (ws)
  mutate(ws_site = if_else(aru == "wg01_" | aru == "wg02_" | aru == "wg03_", 1, 2)) 
# hist_within = cut(hist_within, breaks = 5, labels = c(1,2,3,4,5)),
# hist_across = cut(hist_across, breaks = 5, labels = c(1,2,3,4,5)),
# creating water site (ws_site) 1,2# observed aridity scaled across sites
# hist_within = mean(hist_within), #historic aridity scaled within sites
# hist_across = mean(hist_across) #%>% #historic aridity scaled across sites

#Separating out CBMA water sites
ws_cbmamas1 = cbma_wbio4 %>%
  dplyr::filter(ws_site == 1) %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1)) #1 = water access open

ws_cbmamas2 = cbma_wbio4 %>%
  filter(ws_site == 2) %>%
  mutate(water = 1)

wbio_cbmamas = rbind(ws_cbmamas1, ws_cbmamas2) 
wbio_cbmamas$mas_num = as.numeric(as.character(wbio_cbmamas$mas))
wbio_cbmamas$mas_bin = cut(wbio_cbmamas$mas_num, include.lowest = TRUE, breaks = c(-400,-5,125,255,400), labels = c("0","1","2","3"))

wbio_cbma = wbio_cbmamas %>%
  group_by(ws_site,water,mas_bin)%>%
  summarise(mean_bio = mean(bio),
            arid_within = mean(arid_within) # observed aridity scaled within sites
            # arid_across = mean(arid_across)
  ) %>%
  mutate(arid_within = cut(arid_within, breaks = 5, labels = c(1,2,3,4,5)),
         # arid_across = cut(arid_across, breaks = 5, labels = c(1,2,3,4,5))
  )
save(wbio_sswmamas, file = "water_bio_sswma_mas_bin.Rdata")
save(wbio_cbma, file = "water_bio_cbma_mas_bin.Rdata")

# MAS bin - ACI Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
sswma_wmas2$waridx = interaction(sswma_wmas2$mas_bin,sswma_wmas2$ws_site,sswma_wmas2$water)
mas_waci1 <- lm(mean_aci ~ mas_bin*ws_site*water, data = sswma_wmas2)
summary(mas_waci1)
assump(mas_waci1)
aov(mas_waci1)
Anova(mas_waci1,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = waci_sswma,
      type='III')
mas_waci1 <- lm(mean_aci ~ waridx, data = sswma_wmas2)
TukeyHSD(aov(mas_waci1))

### CBMA Site
waci_cbma$waridx = interaction(waci_cbma$arid_within,waci_cbma$ws_site,waci_cbma$water)
waci2 <- lm(mean_aci ~ arid_within*ws_site*water, data = waci_cbma)
summary(waci2)
assump(waci2)
aov(waci2)
Anova(waci2,
      contrasts=list(factorA='arid_within',factorB='ws_site',factorC='water'), 
      data = ws_cbma,
      type='III')
waci2 <- lm(mean_aci ~ waridx, data = waci_cbma)
TukeyHSD(aov(waci2))


# MAS bin - BIO Water supplementation Statistical Analyses ------------------------------
### SSWMA Site
wbio_sswmamas$waridx = interaction(wbio_sswmamas$arid_within,wbio_sswmamas$ws_site,wbio_sswmamas$water)
wbio1 <- lm(mean_bio ~ arid_within*ws_site*water, data = wbio_sswmamas)
summary(wbio1)
assump(wbio1)
aov(wbio1)
Anova(wbio1,
      contrasts=list(factorA='arid_within', FactorB ='site'), 
      data = wbio_sswma,
      type='III')
wbio1 <- lm(mean_bio ~ waridx, data = wbio_sswmamas)
TukeyHSD(aov(wbio1))

### CBMA Site
wbio_cbma$waridx = interaction(wbio_cbma$arid_within,wbio_cbma$ws_site,wbio_cbma$water)
wbio2 <- lm(mean_bio ~ arid_within*ws_site*water, data = wbio_cbma)
summary(wbio2)
assump(wbio2)
aov(wbio2)
Anova(wbio2,
      contrasts=list(factorA='arid_within',factorB='ws_site',factorC='water'), 
      data = ws_cbma,
      type='III')
wbio2 <- lm(mean_bio ~ waridx, data = wbio_cbma)
TukeyHSD(aov(wbio2))


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








