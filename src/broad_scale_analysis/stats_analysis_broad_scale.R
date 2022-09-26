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
library(lsmeans)


# Loading Clean Data ------------------------------------------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean/acoustic_metric_data/")
load("aci_full.Rdata")
load("bio_full.Rdata")

#color-blind friendly palette using grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#################gray, orange-yellow, light blue, green, yellow, dark blue, orange, pink

aci = aci_full #ghmean_time = within site historic aridity summarized across month_day and mas, 
#ghsite_scaled = scaled ghmean_time within each site

aci$ghobs_acrsite = scale(aci$gh_obs) #observed aridity in 2021 scaled across sites
aci$ghhobs_acrsite = scale(aci$gh_hobs) #historic aridity scaled across sites

ggplot(data = aci, aes(x = mas, y = ghobs_scaledwin, color = site))+ #facet wrap graph of observed aridity in 2021 scaled within sites plotted against minutes after sunrise
  geom_point()+
  facet_wrap(~month_day)


aci_dt = aci %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(is.na(gh_obs) == FALSE)%>%
  mutate(date = date(date_time),
         aci_all = scale(aci_obs))%>%
  group_by(site,month_day,mas) %>%
  summarise(gh_obs= mean(gh_obs),
            gh_hobs = mean(gh_hobs),
            gh_scaled = mean(ghobs_scaledwin), #observed aridity scaled within sites
            ghhobs_site = mean(ghsite_scaled), #historic aridity scaled within sites
            ghhobs_across = mean(ghobs_acrsite), #historic aridity scaled across sites
            aci_obs = mean(aci_obs),
            aci_site = mean(aci_scaled),
            aci_all = mean(aci_all)) 


### Within site observed aridity (2021) plotted against within site historic aridity (2005-2021)
ggplot(data = aci_dt, aes(x = gh_scaled, 
                          y = ghhobs_site, 
                          color = site))+ #within site aridity vs within site aci, points and lms
  geom_point()+#comment out to remove large scatterplots
  geom_smooth(method = lm)+ #comment out out to remove trend lines
  scale_x_continuous(name = "Observed Aridity Scaled\n(Within Sites)") +
  scale_y_continuous(name = "Historic Aridity Scaled\n(Within Sites)") +
  scale_colour_manual(name = "Site",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00"), #light blue, green, orange-yellow, orange
                      labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"))+
  theme_classic(base_size = 20) +
  ggtitle(label = "Observed Aridity Scaled within sites plotted against Historic Aridity Scaled")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results/")
ggsave("within_site_aridity_plot.jpg", dpi = 300, height = 6, width = 8, units = "in")


#something is wrong with cbma dataset, isolating it here to examine it
cbma_acidt = aci_dt %>% dplyr::filter(site == "cbma")

### Within site observed aridity (2021) plotted against within site historic aridity (2005-2021) just for CBMA
ggplot(data = cbma_acidt, aes(x = gh_obs, 
                          y = aci_site, 
                          color = site))+ #within site aridity vs within site aci, points and lms
  geom_point() #comment out to remove large scatterplots


### Observed (2021) Within site ACI Plots and Stats
ggplot(data = aci_dt, aes(x = gh_scaled, 
                          y = aci_site, 
                          color = site))+ #within site aridity vs within site aci, points and lms
  geom_point()+#comment out to remove large scatterplots
  geom_smooth(method = lm)+ #comment out out to remove trend lines
  scale_x_continuous(name = "Observed Aridity Scaled\n(Within Sites)") +
  scale_y_continuous(name = "Observed ACI Scaled\n(Within Sites)") +
  scale_colour_manual(name = "Site",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00"), #light blue, green, orange-yellow, orange
                      labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"))+
  theme_classic(base_size = 20) +
  ggtitle(label = "Observed Aridity Scaled within sites plotted against Observed ACI Scaled Within Sites")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results/")
ggsave("aci_within_site_aridity.jpg", dpi = 300, height = 6, width = 8, units = "in")

### Historic (2005-2021) Within site ACI Plots and Stats
ggplot(data = aci_dt, aes(x = ghhobs_site, 
                          y = aci_site, 
                          color = site))+ #within site aridity vs within site aci, points and lms
  geom_point()+#comment out to remove large scatterplots
  geom_smooth(method = lm)+ #comment out out to remove trend lines
  scale_x_continuous(name = "Observed Aridity Scaled\n(Within Sites)") +
  scale_y_continuous(name = "Observed ACI Scaled\n(Within Sites)") +
  scale_colour_manual(name = "Site",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00"), #light blue, green, orange-yellow, orange
                      labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"))+
  theme_classic(base_size = 20) +
  ggtitle(label = "Observed Aridity Scaled within sites plotted against Observed ACI Scaled Within Sites")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results/")
ggsave("aci_within_site_aridity_historic.jpg", dpi = 300, height = 6, width = 8, units = "in")



m1aciwinsite = lmer(aci_site~ghhobs_site+(1|site),data = aci_dt, REML = FALSE)
summary(m1aciwinsite)
assump(m1aciwinsite)
contrasts = glht(m1aciwinsite, linfct = mcp(site = "Tukey"))
summary(contrasts)

ggplot(data = aci_dt, aes(x = ghhobs_site, y = aci_site, color = site))+ #across site aridity vs across site aci
  # geom_point()+
  geom_smooth(method = lm)

aci_dt = aci %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(is.na(gh_obs) == FALSE)%>%
  mutate(date = date(date_time),
         aci_all = scale(aci_obs))%>%
  group_by(site,month_day,mas) %>%
  summarise(gh_obs= mean(gh_obs),
            gh_site = mean(ghsite_scaled), #aridity scaled within sites
            gh_across = mean(ghobs_acrsite), #aridity scaled across sites
            aci_obs = mean(aci_obs),
            aci_site = mean(aci_scaled),
            aci_all = mean(aci_all)) 


# Biodiversity Acoustic Index ---------------------------------------------
bio = bio_full
bio$ghobs_acrsite = scale(bio$gh_obs)
bio$ghhobs_acrsite = scale(bio$gh_hobs)

bio_mas = bio %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(is.na(gh_obs) == FALSE)%>%
  mutate(date = date(date_time),
         bio_all = scale(bio_obs))%>%
  group_by(site,mas) %>%
  summarise(gh_obs= mean(gh_obs),
            gh_site = mean(ghsite_scaled), #aridity scaled within sites
            gh_across = mean(ghobs_acrsite), #aridity scaled across sites
            bio_obs = mean(bio_obs),
            bio_site = mean(bio_scaled),
            bio_all = mean(bio_all)) 

#bio summarized by minuates after sunrise (mas)
ggplot(data = bio_mas, aes(x = gh_site, y = bio_site, color = site))+ #within site aridity vs within site aci
  geom_point()+
  geom_smooth(method = lm)

ggplot(data = bio_mas, aes(x = gh_across, y = bio_all, color = site))+ #across site aridity vs across site aci
  geom_point()+
  geom_smooth(method = lm)

bio_dt = bio %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(is.na(gh_obs) == FALSE)%>%
  mutate(date = date(date_time),
         bio_all = scale(bio_obs))%>%
  group_by(site,month_day,mas) %>%
  summarise(gh_obs= mean(gh_obs),
            gh_hobs = mean(gh_hobs),
            gh_scaled = mean(ghobs_scaledwin), #observed aridity scaled within sites
            ghhobs_site = mean(ghsite_scaled), #historic aridity scaled within sites
            ghhobs_across = mean(ghobs_acrsite), #historic aridity scaled across sites
            bio_obs = mean(bio_obs),
            bio_site = mean(bio_scaled),
            bio_all = mean(bio_all)) 

### Within site observed aridity (2021) plotted against within site historic aridity (2005-2021)
ggplot(data = bio_dt, aes(x = gh_scaled, 
                          y = ghhobs_site, 
                          color = site))+ #within site aridity vs within site bio, points and lms
  geom_point()+#comment out to remove large scatterplots
  geom_smooth(method = lm)+ #comment out out to remove trend lines
  scale_x_continuous(name = "Observed Aridity Scaled\n(Within Sites)") +
  scale_y_continuous(name = "Historic Aridity Scaled\n(Within Sites)") +
  scale_colour_manual(name = "Site",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00"), #light blue, green, orange-yellow, orange
                      labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"))+
  theme_classic(base_size = 20) +
  ggtitle(label = "Observed Aridity Scaled within sites plotted against Historic Aridity Scaled")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results/")
ggsave("within_site_aridity_plot.jpg", dpi = 300, height = 6, width = 8, units = "in")


#something is wrong with cbma dataset, isolating it here to examine it
cbma_biodt = bio_dt %>% dplyr::filter(site == "cbma")

### Within site observed aridity (2021) plotted against within site historic aridity (2005-2021) just for CBMA
ggplot(data = cbma_biodt, aes(x = gh_obs, 
                              y = gh_hobs, 
                              color = site))+ #within site aridity vs within site bio, points and lms
  geom_point() #comment out to remove large scatterplots


### Observed (2021) Within site bio Plots and Stats
ggplot(data = bio_dt, aes(x = gh_scaled, 
                          y = bio_site, 
                          color = site))+ #within site aridity vs within site bio, points and lms
  geom_point()+#comment out to remove large scatterplots
  geom_smooth(method = lm)+ #comment out out to remove trend lines
  scale_x_continuous(name = "Observed Aridity Scaled\n(Within Sites)") +
  scale_y_continuous(name = "Observed bio Scaled\n(Within Sites)") +
  scale_colour_manual(name = "Site",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00"), #light blue, green, orange-yellow, orange
                      labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"))+
  theme_classic(base_size = 20) +
  ggtitle(label = "Observed Aridity Scaled within sites plotted against Observed bio Scaled Within Sites")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results/")
ggsave("bio_within_site_aridity.jpg", dpi = 300, height = 6, width = 8, units = "in")

### Historic (2005-2021) Within site bio Plots and Stats
ggplot(data = bio_dt, aes(x = ghhobs_site, 
                          y = bio_site, 
                          color = site))+ #within site aridity vs within site bio, points and lms
  geom_point()+#comment out to remove large scatterplots
  geom_smooth(method = lm)+ #comment out out to remove trend lines
  scale_x_continuous(name = "Observed Aridity Scaled\n(Within Sites)") +
  scale_y_continuous(name = "Observed bio Scaled\n(Within Sites)") +
  scale_colour_manual(name = "Site",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00"), #light blue, green, orange-yellow, orange
                      labels = c("LWMA", "SSWMA", "CBMA", "KIOWA"))+
  theme_classic(base_size = 20) +
  ggtitle(label = "Observed Aridity Scaled within sites plotted against Observed bio Scaled Within Sites")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/results/")
ggsave("bio_within_site_aridity_historic.jpg", dpi = 300, height = 6, width = 8, units = "in")






ggplot(data = aci, aes(x = gh, y = left_channel, color = site))+
  geom_point()+
  stat_ellipse(type = "norm")

xdensity = ggplot(data = aci, aes(x = gh, fill = site)) +
  geom_density(alpha = 0.5);xdensity

ydensity = ggplot(data = aci, aes(x = left_channel, fill = site)) +
  geom_density(alpha = 0.5);ydensity

ggplot(data = bio_dh, aes(x = mean_gh, y = bio_obs, color = site))+
  geom_point()+
  stat_ellipse(type = "norm")

ggplot(data = aci, aes(x = mean_ghmd, y = left_channel, color = site))+
  # geom_point()+
  # stat_ellipse(type = "norm")+
  geom_smooth(method = "gam")

#Compre day-to-day aridity to mean aridity (mean_ghmd)
ggplot(data = aci, aes(x = hour_utc, y = stmean_ghacross, color = site))+
  # geom_point()+
  # stat_ellipse(type = "norm")+
  geom_smooth(method = lm)

m1aciobs = lmer(aci_obs~gh_obs+(1|site/aru),data = aci, REML = FALSE)
summary(m1aciobs)



m1aciacrsite = lmer(aci_scaled~stdmean_ghsite*site+(1|aru),data = aci_dh, REML = FALSE)
summary(m1aciwinsite)
assump(m1aciwinsite)
contrasts = glht(m1aciwinsite, linfct = mcp(site = "Tukey"))

m2aciobs = lmer(aci_obs~gh_obs*site+(1|aru),data = aci_dh, REML = FALSE)
summary(m2aciobs)

m2aciwinsite = lmer(aci_site~gh_site*site+(1|aru),data = aci_dh, REML = FALSE)
summary(m2aciwinsite)
assump(m2aciwinsite)
contrasts = glht(m2aciwinsite, linfct = mcp(site = "Tukey"))
summary(contrasts)

m2aciacrsite = lmer(aci_all~gh_across*site+(1|aru),data = aci_dh, REML = FALSE)
summary(m2aciacrsite)
assump(m2aciacrsite)
contrasts = glht(m2aciacrsite, linfct = mcp(site = "Tukey"))
summary(contrasts)


m2bioobs = lmer(bio_obs~gh_obs*site+(1|aru),data = bio_dh, REML = FALSE)
summary(m2bioobs)
assump(m2bioobs)
contrasts = glht(m2bioobs, linfct = mcp(site = "Tukey"))
summary(contrasts)

m2biowinsite = lmer(bio_site~gh_site*site+(1|aru),data = bio_dh, REML = FALSE)
summary(m2biowinsite)
assump(m2biowinsite)
contrasts = glht(m2biowinsite, linfct = mcp(site = "Tukey"))
summary(contrasts)

m2bioacrsite = lmer(bio_all~gh_across*site+(1|aru),data = bio_dh, REML = FALSE)
summary(m2bioacrsite)
assump(m2bioacrsite)
contrasts = glht(m2bioacrsite, linfct = mcp(site = "Tukey"))
summary(contrasts)



# Plotting multi-panel plots ----------------------------------------------

#creating two panel graph for dew temperature fluctuation and 

#Data is averaged by hour across aru and breeding season
cbpalette <- c("#56B4E9","#E69F00", "#999999",  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



aci_hour = aci %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  arrange(site,aru,hour_utc) %>%
  group_by(site,hour_utc) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_aci = mean(left_channel),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))

bio_hour = bio %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  arrange(site,aru,hour_utc) %>%
  group_by(site,hour_utc) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_bio = mean(left_channel),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))

#Summarizing ACI by date for graphs
aci_date = aci %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  mutate(date = as_date(date_time))%>%
  dplyr::filter(date<= "2021-08-15") %>%
  group_by(site,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_aci = mean(left_channel),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))

bio_date = bio %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  mutate(date = as_date(date_time))%>%
  dplyr::filter(date<= "2021-08-15") %>%
  group_by(site,date) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_bio = mean(left_channel),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))

ggplot(data = bio,
       aes(x=left_channel, y=gh, 
           color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 1)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "GH")+
  scale_x_continuous(name = "BIO")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = "right")

sswma_bio = bio %>%
  dplyr::filter(site == "sswma")%>%
  dplyr::filter(left_channel < 13)
bio_rest = bio %>%
  dplyr::filter(site != "sswma")
bio2 = rbind(sswma_bio,bio_rest)

ggplot(data = aci,
       aes(x=left_channel, y=gh, 
           color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 1)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "GH")+
  scale_x_continuous(name = "BIO")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = "right")

library(ohun)

# Plotting ACI and BIO Aridity Gradient Graphs ----------------------------

aci_h = ggplot(data = bio,aes(x=hour_utc, y=log(mean_aci), color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log\nMean\nACI")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");aci_h

#Plotting max aridity across hour
hour_aridmean = ggplot(data = aci_hour,aes(x=hour_utc, y=max_gh, color = site)) +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site", labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_continuous(name = "Hour")+
  scale_y_continuous(name = "Max\nEvaporation\nRate\n(kg/hr)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x= element_text("Mean Sun Altitude (Radians)")
  )+ #change angle to 0 for presentations, 90 for papers
  theme(legend.position = "bottom");hour_aridmean

metric_out <- plot_grid(aci_h,hour_aridmean, align = "v", ncol = 1, rel_heights = c(0.4,0.6));metric_out
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("aci_hour.jpg",plot = metric_out, width = 8, height = 7.5, units = "in")

#Mean ACI Across Date
aci_d = ggplot(data = aci_date,aes(x=date, y=log(mean_aci), color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log\nMean\nACI")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");aci_d

#Plotting max aridity across hour
aridmean_date = ggplot(data = aci_date,aes(x=date, y=max_gh, color = site)) +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site", labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_date(name = "Date")+
  # scale_y_continuous(name = "Max\nEvaporation\nRate\n(kg/hr)")+
  scale_y_continuous(name = "Max Evaporation Rate\n(kg/hr)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x= element_text("Mean Sun Altitude (Radians)")
  )+ #change angle to 0 for presentations, 90 for papers
  theme(legend.position = "bottom");aridmean_date
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("aci_date.jpg",plot = aridmean_date, width = 10, height = 7.5, units = "in")

metric_out <- plot_grid(aci_d,aridmean_date, align = "v", ncol = 1, rel_heights = c(0.4,0.6));metric_out
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("aci_date.jpg",plot = metric_out, width = 8, height = 7.5, units = "in")


bio_h = ggplot(data = bio_hour,aes(x=hour_utc, y=log(mean_bio), color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log\nMean\nBIO")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");bio_h

hour_aridmax = ggplot(data = aci_hour,aes(x=hour_utc, y=max_gh, color = site)) +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site", labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_continuous(name = "Hour")+
  scale_y_continuous(name = "Mean\nEvaporation\nRate\n(kg/hr)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x= element_text("Mean Sun Altitude (Radians)")
  )+ #change angle to 0 for presentations, 90 for papers
  theme(legend.position = "bottom");hour_aridmax

metric_out2 <- plot_grid(bio_h,hour_aridmax, align = "v", ncol = 1, rel_heights = c(0.4,0.6));metric_out2
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("bio_hour.jpg",plot = metric_out2, width = 8, height = 7.5, units = "in")

#Plotting Date for BIO Aridity Gradient
#Mean ACI Across Date
bio_d = ggplot(data = bio_date,aes(x=date, y=log(mean_bio), color = site)) +
  # ggtitle("climate_change_extreme") +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site")+
  scale_y_continuous(name = "Log\nMean\nBIO")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none");bio_d

#Plotting max aridity across hour
aridmean_date = ggplot(data = bio_date,aes(x=date, y=mean_gh, color = site)) +
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = cbpalette,name = "Site", labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Mean\nEvaporation\nRate\n(kg/hr)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x= element_text("Mean Sun Altitude (Radians)")
  )+ #change angle to 0 for presentations, 90 for papers
  theme(legend.position = "bottom");aridmean_date

bio_graph_date <- plot_grid(bio_d,aridmean_date, align = "v", ncol = 1, rel_heights = c(0.4,0.6));bio_graph_date
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("bio_date.jpg",plot = bio_graph_date, width = 8, height = 7.5, units = "in")

# Statistical Analyses Aridity Gradient ----------------------------------------------------
# Testing for Collinearity 

cor(aci_dh[,c(4:14)])
cor.test(cor_arid$num_vocals,cor_arid$species_diversity)
#ACI
hist(aci$left_channel)
hist(log(aci$left_channel))
hist(log(bio$left_channel))

m1re = lmer(log(mean_aci) ~ hour_utc*scale(date)*mean_gh + (1|aru), data = aci_dh, REML = TRUE)
m2re = lmer(log(mean_aci) ~ hour_utc*scale(date)*mean_gh + (1|site), data = aci_dh, REML = TRUE)
m3re = lmer(log(mean_aci) ~ hour_utc*scale(date)*mean_gh + (1|site/aru), data = aci_dh, REML = TRUE)
AICctab(m1re,m2re,m3re, nobs = 10358, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

m0aci = lme(log(left_channel) ~ gh_resids, random = ~1+gh_resids|site, data = aci, method = "REML")
summary(m0aci)

m1aci = lmer(log(left_channel) ~ gh_resids + (1|site), data = aci, REML = TRUE)
summary(m1aci)
assump(m1aci)
AICctab(m0aci,m1aci, nobs = 61644, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


m1aci = lmer(log(mean_aci) ~ mean_gh*site + (1|site), data = aci_dh, REML = FALSE)
summary(m1aci)
m2aci = lmer(log(mean_aci) ~ hour_utc*scale(date)*max_gh + (1|site/aru), data = aci_dh, REML = FALSE)

AICctab(m1aci,m2aci, nobs = 10358, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m2aci) #as max aridity, hour, and date increase so does ACI
assump(m2aci) 

#BIO
m1bio = lmer(log(mean_bio) ~ hour_utc*scale(date)*mean_gh + (1|site/aru), data = bio_dh, REML = FALSE)
m2bio = lmer(log(mean_bio) ~ hour_utc*scale(date)*max_gh + (1|site/aru), data = bio_dh, REML = FALSE)

AICctab(m1bio,m2bio, nobs = 10358, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m1bio) #as hour and date and mean aridity increase, BIO decreases
assump(m1bio) 

# Water Supplementation Experiment ----------------------------------------

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
load("aci_water.Rdata")
load("bio_water.Rdata")

aci_water = aci_water %>%
  rename(site = "site.y",
         hour_local = "hour.x",
         hour_utc = "hour.y") %>%
  mutate(water_int = interaction(ws_site,water))
bio_water = bio_water %>%
  rename(site = "site.y",
         hour_local = "hour.x",
         hour_utc = "hour.y") %>%
  mutate(water_int = interaction(ws_site,water))


# Creating Graphs for Water Supplementation Experiment --------------------
#Using date_time as the x variable
ggplot(data = aci_water %>% dplyr::filter(site == "sswma"), aes(x = date_time, y = left_channel, color = water_int))+
  geom_point()

#Data is averaged by hour across aru and breeding season
cbpalette <- c("#56B4E9","#E69F00","#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# AVeraging Metrics Across Dates -----------------------------------------
waci_dh = aci_water %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(date < "2021-08-07")%>%
  mutate(date = date(date_time))%>%
  group_by(site,aru,date,hour_utc,ws_site,water) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_aci = mean(left_channel),
            se_aci = sd(left_channel)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))
waci_dh$water = as.factor(waci_date$water)
waci_dh$ws_site = as.factor(waci_date$ws_site)
waci_dh$water_int = interaction(waci_dh$ws_site,waci_dh$water)
sdate_watera = waci_dh %>% dplyr::filter(site == "sswma")
cdate_watera = waci_dh %>% dplyr::filter(site == "cbma") 
#

wbio_dh = bio_water %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(date < "2021-08-07")%>%
  mutate(date = date(date_time))%>%
  group_by(site,aru,date,hour_utc,ws_site,water) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_bio = mean(left_channel),
            se_bio = sd(left_channel)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))
wbio_dh$water = as.factor(wbio_dh$water)
wbio_dh$ws_site = as.factor(wbio_dh$ws_site)
wbio_dh$water_int = interaction(wbio_dh$ws_site,wbio_dh$water)
sdate_waterb = wbio_dh %>% dplyr::filter(site == "sswma")
cdate_waterb = wbio_dh %>% dplyr::filter(site == "cbma")

# Statistical Analyses data summarized by Date - SSWMA+CBMA together ACI----------------------------------------------------
hist(waci_date$mean_aci)
hist(log(waci_date$mean_aci))
# sdate_water = waci_date %>% dplyr::filter(site == "sswma")

m1rewd = lmer(log(mean_aci) ~ scale(date)*water*mean_gh + (1|ws_site), data = sdate_watera, REML = TRUE)
m2rewd = lmer(log(mean_aci) ~ scale(date)*water*mean_gh + (1|aru), data = sdate_watera, REML = TRUE)
m3rewd = lmer(log(mean_aci) ~ scale(date)*water*mean_gh + (1|ws_site/aru), data = sdate_watera, REML = TRUE)
# m4rewd = lmer(log(mean_aci) ~ scale(date)*water*mean_gh + (1|site/ws_site), data = sdate_water, REML = TRUE)

AICctab(m1rewd,m2rewd,m3rewd, nobs = 720, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
summary(m2rewd) #best random effect is aru

# Water Supplementation Data summarized by Date and ARU - SSWMA ACI --------------------------------------------------------------------
cor(sdate_watera[,c(6:14)]) #testing collinearity
sdate_watera = waci_dh %>% dplyr::filter(site == "sswma")
sdate_watera2 = sdate_watera[-c(143,574),] #removing outliers defined by outlierTest, doesn't do anything to improve results so leaving them in


m1aciwd = lmer(log(mean_aci) ~ water_int*scale(date)*mean_gh*hour_utc +(1|aru),data=sdate_watera,REML=FALSE)
m2aciwd = lmer(log(mean_aci) ~ water_int*scale(date)*max_gh*hour_utc +(1|aru),data=sdate_watera,REML=FALSE)

AICctab(m1aciwd,m2aciwd, nobs = 5698, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m1aciwd)
assump(m1aciwd)
Anova(m1aciwd, type = "III")
contrasts = glht(m1aciwd, linfct = mcp(water_int = "Tukey"))
summary(contrasts)
outlierTest(m7aciwd)
cooksd <- cooks.distance(m7aciwd) #determining cook's distance for outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, cex = 5, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels, with cook's

summary(m7.5aciwd)
assump(m7.5aciwd)
Anova(m7.5aciwd, type = "III")
contrasts = glht(m7.5aciwd, linfct = mcp(water_int = "Tukey"))
summary(contrasts)
outlierTest(m7aciwd)
cooksd <- cooks.distance(m7aciwd) #determining cook's distance for outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, cex = 5, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels, with cook's

# Water Supplementation Data summarized by Date and ARU - CBMA ACI --------------------------------------------------------------------
cdate_watera = waci_date %>% dplyr::filter(site == "cbma")
cor(cdate_watera[,c(4:14)]) #testing collinearity

cdate_watera2 = cdate_watera[-c(21,239,270),]#removing points that are driving m7 using the outlierTest, removing outliers improves the model fit


m1aciwdc = lmer(log(mean_aci) ~ mean_gh*water_int*scale(date)*hour_utc +(1|aru),data=cdate_watera,REML=FALSE)
m2aciwdc = lmer(log(mean_aci) ~ max_gh*water_int*scale(date)*hour_utc +(1|aru),data=cdate_watera,REML=FALSE)

AICctab(m1aciwdc,m2aciwdc, nobs = 5698, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m1aciwdc) #best model with outliers
assump(m1aciwdc)
Anova(m1aciwdc, type = "III")
contrasts = glht(m1aciwdc, linfct = mcp(water_int = "Tukey"))
summary(contrasts)
outlierTest(m7aciwdc)
cooksd <- cooks.distance(m9aciwdc) #determining cook's distance for outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, cex = 5, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels, with cook's distance, outliers are rows 12, 27, 178, 239

hist(log(cdate_watera2$mean_aci))
summary(m7.5aciwdc) #best model without outliers
assump(m7.5aciwdc)
Anova(m7.5aciwdc, type = "III")
contrasts = glht(m7.5aciwdc, linfct = mcp(water_int = "Tukey"))
summary(contrasts)
outlierTest(m7.5aciwdc)
cooksd <- cooks.distance(m8aciwdc) #determining cook's distance for outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, cex = 5, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


# # Water Supplementation Data summarized by Date and ARU - SSWMA BIO --------------------------------------------------------------------

sdate_waterb = wbio_dh %>% dplyr::filter(site == "sswma")
sdate_waterb2 = sdate_waterb[-c(243),] #removing outlier determined by outlierTest(), does not really affect results so might as well keep it in analysis

m1biowd = lmer(log(mean_bio) ~ mean_gh*water_int*scale(date)*hour_utc +(1|aru),data=sdate_waterb,REML=FALSE)
m2biowd = lmer(log(mean_bio) ~ max_gh*water_int*scale(date)*hour_utc +(1|aru),data=sdate_waterb,REML=FALSE)

AICctab(m1biowd,m2biowd, nobs = 5698, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m2biowd)
assump(m2biowd)
Anova(m2biowd, type = "III")
contrasts = glht(m2biowd, linfct = mcp(water_int = "Tukey"))
summary(contrasts)
outlierTest(m2aciwdc)
cooksd <- cooks.distance(m7aciwdc) #determining cook's distance for outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, cex = 5, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

# # Water Supplementation Data summarized by Date and ARU - CBMA BIO --------------------------------------------------------------------
cdate_waterb = wbio_dh %>% dplyr::filter(site == "cbma")


m1biowdc = lmer(log(mean_bio) ~ mean_gh*water_int*scale(date)*hour_utc +(1|aru),data=cdate_waterb,REML=FALSE)
m2biowdc = lmer(log(mean_bio) ~ max_gh*water_int*scale(date)*hour_utc +(1|aru),data=cdate_waterb,REML=FALSE)

AICctab(m1biowdc,m2biowdc, nobs = 2156, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m2biowdc)
assump(m2biowdc)
Anova(m2biowdc, type = "III")
contrasts = glht(m2biowdc, linfct = mcp(water_int = "Tukey"))
summary(contrasts)
lsmeans(m2biowdc, pairwise ~ water_int)


outlierTest(m2biowdc)
cooksd <- cooks.distance(m7biowdc) #determining cook's distance for outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, cex = 5, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


# SSWMA Water Supplementation Plots ---------------------------------------

sswma1_rec1 <- data.frame (xmin=as_date("2021-05-17"), xmax=as_date("2021-05-30"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
sswma2_rec1 <- data.frame (xmin=as_date("2021-05-30"), xmax=as_date("2021-06-13"), ymin=-Inf, ymax=Inf) #start of water site 2 with water
sswma1_rec2 = data.frame (xmin=as_date("2021-06-13"), xmax=as_date("2021-07-02"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
sswma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2

#Summarizing by Date and ARU so the trends match up with Tukey Test - ACI
aci_graphd = aci_water %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(date < "2021-08-07")%>%
  arrange(site,aru,hour_utc) %>%
  mutate(date = date(date_time))%>%
  group_by(site,date,ws_site,water) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_aci = mean(left_channel),
            se_aci = sd(left_channel)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))
aci_graphd$water = as.factor(aci_graphd$water)
aci_graphd$ws_site = as.factor(aci_graphd$ws_site)
aci_graphd$water_int = interaction(aci_graphd$ws_site,aci_graphd$water)
aci_graphds = aci_graphd %>% dplyr::filter(site == "sswma")
aci_graphdc = aci_graphd %>% dplyr::filter(site == "cbma")
aci_graphdc = aci_graphdc[-c(58),]

#Summarizing by Date so the trends appear smoother than summarizing by ARU - BIO

bio_graphd = bio_water %>%
  dplyr::filter(is.na(left_channel) == FALSE) %>%
  dplyr::filter(date < "2021-08-07")%>%
  mutate(date = date(date_time))%>%
  group_by(site,aru,date,ws_site,water) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_bio = mean(left_channel),
            se_bio = sd(left_channel)/sqrt(n()),
            mean_temp = mean(temp),
            mean_relh = mean(relh),
            mean_dew = mean(dew),
            mean_arid = mean(arid),
            mean_gh = mean(gh),
            max_gh = max(gh))
bio_graphd$water = as.factor(bio_graphd$water)
bio_graphd$ws_site = as.factor(bio_graphd$ws_site)
bio_graphd$water_int = interaction(bio_graphd$ws_site,bio_graphd$water)
bio_graphds = bio_graphd %>% dplyr::filter(site == "sswma")
bio_graphdc = bio_graphd %>% dplyr::filter(site == "cbma")

aci_dates = ggplot(data = aci_graphds,
                      aes(x=date, y=log(mean_aci), 
                          color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=sswma1_rec1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#56B4E9", alpha=0.2, inherit.aes = FALSE) +
  geom_rect(data=sswma1_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#56B4E9", alpha=0.2, inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.2, inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.2, inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),
                     name = "Water Station")+
  # scale_y_continuous(name = "Log\n(Mean\nACI)")+#for presentations
  scale_y_continuous(name = "Log(Mean ACI)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none");aci_dates

#Boxplot for SSWMA ACI
boxplot_sswma_aci = ggplot(data = sdate_watera,
       aes(x=as.factor(ws_site), y=log(mean_aci), 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean\nACI)")+
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
        legend.margin=margin(t=-20));boxplot_sswma_aci

#Save Multi-panel plot for SSWMA ACI Graphs
sswmaw_plots = plot_grid(aci_dates, boxplot_sswma_aci, align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_plots
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("sswma_water_aci_date.jpg",plot = sswmaw_plots, width = 8, height = 7.5, units = "in")

###SSWMA BIO Plots
bio_dates = ggplot(data = bio_graphds,
                   aes(x=date, y=log(mean_bio), 
                       color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=sswma1_rec1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#56B4E9", alpha=0.2, inherit.aes = FALSE) +
  geom_rect(data=sswma1_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#56B4E9", alpha=0.2, inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.2, inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.2, inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),
                     name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean\nBIO)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none");bio_dates

###SSWMA Water BIO Box Plots
boxplot_sswma_bio = ggplot(data = sdate_waterb,
                          aes(x=as.factor(ws_site), y=log(mean_bio), 
                              color = as.factor(ws_site),
                              fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean\nBIO)")+
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
        legend.margin=margin(t=-20));boxplot_sswma_bio
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
sswmaw_plots_bio = plot_grid(bio_dates, 
                             boxplot_sswma_bio, 
                             align = "v", 
                             ncol = 1, 
                             rel_heights = c(0.45,0.55));sswmaw_plots_bio
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("sswmaw_plots_bio.jpg",plot = sswmaw_plots_bio, width = 8, height = 7.5, units = "in")

# CBMA Water Supplementation Plots --------------------------------------------------------
full_water1 = full_water %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = full_water %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)


cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), 
                          xmax=as_date("2021-06-04"), 
                          ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), 
                         xmax=as_date("2021-07-19"), 
                         ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), 
                         xmax=as_date("2021-08-07"), 
                         ymin=-Inf, ymax=Inf) #start of water at water site 2

aci_datec = ggplot(data = aci_graphdc,
                      aes(x=date, y=log(mean_aci), 
                          color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=cbma1_rec1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#56B4E9", alpha=0.2, inherit.aes = FALSE) +
  geom_rect(data=cbma1_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#56B4E9", alpha=0.2, inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),
                     name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log(Mean ACI)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom");aci_datec
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbma_water_aci_date.jpg",plot = aci_datec, width = 10, height = 7.5, units = "in")

#Boxplot for CBMA ACI
boxplot_cbma_aci = ggplot(data = cdate_watera,
                          aes(x=as.factor(ws_site), y=log(mean_aci), 
                              color = as.factor(ws_site),
                              fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  # scale_y_continuous(name = "Log\n(Mean\nACI)")+
  scale_y_continuous(name = "Log(Mean ACI)")+
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
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbmaw_aci_boxplot.jpg",plot = boxplot_cbma_aci, width = 8, height = 7.5, units = "in")

cbmaw_plots_aci = plot_grid(aci_datec, 
                            boxplot_cbma_aci, 
                             align = "v", 
                             ncol = 1, 
                             rel_heights = c(0.4, 0.6));cbmaw_plots_aci
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbmaw_plots_aci.jpg",plot = cbmaw_plots_aci, width = 8, height = 7.5, units = "in")

#Plotting CBMA BIO Plots
bio_datec = ggplot(data = bio_graphdc,
                       aes(x=date, y=log(mean_bio), 
                           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=cbma1_rec1, aes(xmin=xmin, 
                                 xmax=xmax, 
                                 ymin=ymin, 
                                 ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.2, 
            inherit.aes = FALSE) +
  geom_rect(data=cbma1_rec2, aes(xmin=xmin, xmax=xmax, 
                                 ymin=ymin, ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.2, 
            inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),
                     name = "Water Site")+
  scale_x_date(name = "Date")+
  # scale_y_continuous(name = "Log\n(Mean\nBIO)")+
  scale_y_continuous(name = "Log(Mean BIO)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom"
        # legend.position = c(0.9,0.89),
        # legend.direction = "vertical",
        # legend.box = "vertical"
        );bio_datec
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbmaw_bio_date.jpg",plot = bio_datec, width = 10, height = 7.5, units = "in")

#Boxplot for CBMA BIO
boxplot_cbma_bio = ggplot(data = bio_graphdc,
                          aes(x=as.factor(ws_site), y=log(mean_bio), 
                              color = as.factor(ws_site),
                              fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  # scale_y_continuous(name = "Log\n(Mean\nBIO)")+
  scale_y_continuous(name = "Log(Mean BIO)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, #change to 0 for presentations, 90 for papers
                                    vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        # legend.position = c(0.89,0.85),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20)); boxplot_cbma_bio
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbmaw_bio_boxplot.jpg",plot = boxplot_cbma_bio, width = 10, height = 7.5, units = "in")
# +
#   guides(color="none");boxplot_cbma_bio

cbmaw_plots = plot_grid(bio_datec, 
                        boxplot_cbma_bio, 
                        align = "v", 
                        ncol = 1, 
                        rel_heights = c(0.4, 0.6));cbmaw_plots


setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/results/")
ggsave("cbma_water_plots.jpg",plot = cbmaw_plots, width = 8, height = 7.5, units = "in")





# Statistical Analyses - Raw Dataset ----------------------------------------------------


m1rew = lmer(log(left_channel) ~ altitude*gh + (1|aru), data = aci_water, REML = TRUE)
m2rew = lmer(log(left_channel) ~ altitude*gh + (1|site), data = aci_water, REML = TRUE)
m3rew = lmer(log(left_channel) ~ altitude*gh + (1|site/aru), data = aci_water, REML = TRUE)
m4rew = lmer(log(left_channel) ~ altitude*gh + (1|ws_site/aru), data = aci_water, REML = TRUE)
AICctab(m1rew,m2rew,m3rew,m4rew, nobs = 51770, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

#SSWMA Data - Water Supplementation
sswma_water = aci_water %>% dplyr::filter(site == "sswma")
m1aciw = lmer(log(left_channel) ~ 1 +(1|aru),data=sswma_water,REML=FALSE)
m2aciw  = lmer(log(left_channel) ~ water +(1|aru),data=sswma_water,REML=FALSE)
m3aciw  = lmer(log(left_channel) ~ ws_site +(1|aru),data=sswma_water,REML=FALSE)
m4aciw  = lmer(log(left_channel) ~ water+ws_site +(1|aru),data=sswma_water,REML=FALSE)
m5aciw  = lmer(log(left_channel) ~ water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m6aciw  = lmer(log(left_channel) ~ arid+water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m7aciw  = lmer(log(left_channel) ~ arid*water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m8aciw  = lmer(log(left_channel) ~ relh+water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m9aciw  = lmer(log(left_channel) ~ relh*water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m10aciw  = lmer(log(left_channel) ~ gh+water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m11aciw  = lmer(log(left_channel) ~ gh*water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m12aciw  = lmer(log(left_channel) ~ altitude+water*ws_site +(1|aru),data=sswma_water,REML=FALSE)
m13aciw  = lmer(log(left_channel) ~ altitude*water_int +(1|aru),data=sswma_water,REML=FALSE)
m14aciw = lmer(log(left_channel) ~ scale(date_time)*water_int*gh +(1|aru),data=sswma_water,REML=FALSE)
m15aciw = lmer(log(left_channel) ~ scale(date_time)*water_int*gh*altitude +(1|aru),data=sswma_water,REML=FALSE)

# m14aciw  = lmer(log(left_channel) ~ gh*altitude +(1|aru),data=sswma_water,REML=FALSE)

AICctab(m1aciw,m2aciw,m3aciw,m4aciw,m5aciw,m8aciw,m9aciw,m10aciw,m11aciw,m12aciw,m13aciw,m14aciw,m15aciw, nobs = 34617, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m15aciw)
Anova(m15aciw, type='III')
contrasts = glht(m15aciw, linfct = mcp(water_int = "Tukey"))
summary(contrasts)

summary(m13aciw) #as relative humidity and sun altitude increase so does aci
assump(m13aciw) #as evaporation rate and sun altitude increase aci decreases
Anova(m13aciw, type='III')
contrasts = glht(m13aciw, linfct = mcp(water_int = "Tukey"))
options(max.print = 999999)
summary(contrasts)

#CBMA Data
cbma_water = aci_water %>% dplyr::filter(site == "cbma")
m1aciw = lmer(log(left_channel) ~ 1 +(1|aru),data=cbma_water,REML=FALSE)
m2aciw  = lmer(log(left_channel) ~ water +(1|aru),data=cbma_water,REML=FALSE)
m3aciw  = lmer(log(left_channel) ~ ws_site +(1|aru),data=cbma_water,REML=FALSE)
m4aciw  = lmer(log(left_channel) ~ water+ws_site +(1|aru),data=cbma_water,REML=FALSE)
m5aciw  = lmer(log(left_channel) ~ water_int +(1|aru),data=cbma_water,REML=FALSE)
m6aciw  = lmer(log(left_channel) ~ arid+water_int +(1|aru),data=cbma_water,REML=FALSE)
m7aciw  = lmer(log(left_channel) ~ arid*water_int +(1|aru),data=cbma_water,REML=FALSE)
m8aciw  = lmer(log(left_channel) ~ relh+water_int +(1|aru),data=cbma_water,REML=FALSE)
m9aciw  = lmer(log(left_channel) ~ relh*water_int +(1|aru),data=cbma_water,REML=FALSE)
m10aciw  = lmer(log(left_channel) ~ gh+water_int +(1|aru),data=cbma_water,REML=FALSE)
m11aciw  = lmer(log(left_channel) ~ gh*water_int +(1|aru),data=cbma_water,REML=FALSE)
m12aciw  = lmer(log(left_channel) ~ altitude+water_int +(1|aru),data=cbma_water,REML=FALSE)
m13aciw  = lmer(log(left_channel) ~ altitude*water_int +(1|aru),data=cbma_water,REML=FALSE)
# m14aciw  = lmer(log(left_channel) ~ gh*altitude +(1|aru),data=sswma_water,REML=FALSE)

AICctab(m1aciw,m2aciw,m3aciw,m4aciw,m5aciw,m8aciw,m9aciw,m10aciw,m11aciw,m12aciw,m13aciw, nobs = 17153, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m13aciw) #as relative humidity and sun altitude increase so does aci
assump(m13aciw) #as evaporation rate and sun altitude increase aci decreases
Anova(m13aciw, type='III')
contrasts = glht(m13aciw, linfct = mcp(water_int = "Tukey"))
options(max.print = 999999)
summary(contrasts)


# Bioacoustic Diversity Water Analyses ----------------------------------------------
m1rew = lmer(log(left_channel) ~ altitude*gh + (1|aru), data = bio_water, REML = TRUE)
m2rew = lmer(log(left_channel) ~ altitude*gh + (1|site), data = bio_water, REML = TRUE)
m3rew = lmer(log(left_channel) ~ altitude*gh + (1|site/aru), data = bio_water, REML = TRUE)
m4rew = lmer(log(left_channel) ~ altitude*gh + (1|ws_site/aru), data = bio_water, REML = TRUE)
AICctab(m1rew,m2rew,m3rew,m4rew, nobs = 51770, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

#SSWMA Data
sswma_water = bio_water %>% dplyr::filter(site == "sswma")
m1biow = lmer(log(left_channel) ~ 1 +(1|aru),data=sswma_water,REML=FALSE)
m2biow  = lmer(log(left_channel) ~ water +(1|aru),data=sswma_water,REML=FALSE)
m3biow  = lmer(log(left_channel) ~ ws_site +(1|aru),data=sswma_water,REML=FALSE)
m4biow  = lmer(log(left_channel) ~ water+ws_site +(1|aru),data=sswma_water,REML=FALSE)
m5biow  = lmer(log(left_channel) ~ water_int +(1|aru),data=sswma_water,REML=FALSE)
m6biow  = lmer(log(left_channel) ~ arid+water_int +(1|aru),data=sswma_water,REML=FALSE)
m7biow  = lmer(log(left_channel) ~ arid*water_int +(1|aru),data=sswma_water,REML=FALSE)
m8biow  = lmer(log(left_channel) ~ relh+water_int +(1|aru),data=sswma_water,REML=FALSE)
m9biow  = lmer(log(left_channel) ~ relh*water_int +(1|aru),data=sswma_water,REML=FALSE)
m10biow  = lmer(log(left_channel) ~ gh+water_int +(1|aru),data=sswma_water,REML=FALSE)
m11biow  = lmer(log(left_channel) ~ gh*water_int +(1|aru),data=sswma_water,REML=FALSE)
m12biow  = lmer(log(left_channel) ~ altitude+water_int +(1|aru),data=sswma_water,REML=FALSE)
m13biow  = lmer(log(left_channel) ~ altitude*water_int +(1|aru),data=sswma_water,REML=FALSE)
# m14aciw  = lmer(log(left_channel) ~ gh*altitude +(1|aru),data=sswma_water,REML=FALSE)

AICctab(m1biow,m2biow,m3biow,m4biow,m5biow,m8biow,m9biow,m10biow,m11biow,m12biow,m13biow, nobs = 34617, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m11biow) #as relative humidity and sun altitude increase so does aci
assump(m11biow) #as evaporation rate and sun altitude increase aci decreases
Anova(m11biow, type='III')
contrasts = glht(m11biow, linfct = mcp(water_int = "Tukey"))
options(max.print = 999999)
summary(contrasts)

#CBMA Data
cbma_water = bio_water %>% dplyr::filter(site == "cbma")
m1biow = lmer(log(left_channel) ~ 1 +(1|aru),data=cbma_water,REML=FALSE)
m2biow  = lmer(log(left_channel) ~ water +(1|aru),data=cbma_water,REML=FALSE)
m3biow  = lmer(log(left_channel) ~ ws_site +(1|aru),data=cbma_water,REML=FALSE)
m4biow  = lmer(log(left_channel) ~ water+ws_site +(1|aru),data=cbma_water,REML=FALSE)
m5biow  = lmer(log(left_channel) ~ water_int +(1|aru),data=cbma_water,REML=FALSE)
m6biow  = lmer(log(left_channel) ~ arid+water_int +(1|aru),data=cbma_water,REML=FALSE)
m7biow  = lmer(log(left_channel) ~ arid*water_int +(1|aru),data=cbma_water,REML=FALSE)
m8biow  = lmer(log(left_channel) ~ relh+water_int +(1|aru),data=cbma_water,REML=FALSE)
m9biow  = lmer(log(left_channel) ~ relh*water_int +(1|aru),data=cbma_water,REML=FALSE)
m10biow  = lmer(log(left_channel) ~ gh+water_int +(1|aru),data=cbma_water,REML=FALSE)
m11biow  = lmer(log(left_channel) ~ gh*water_int +(1|aru),data=cbma_water,REML=FALSE)
m12biow  = lmer(log(left_channel) ~ altitude+water_int +(1|aru),data=cbma_water,REML=FALSE)
m13biow  = lmer(log(left_channel) ~ altitude*water_int +(1|aru),data=cbma_water,REML=FALSE)
# m14biow  = lmer(log(left_channel) ~ gh*altitude +(1|aru),data=sswma_water,REML=FALSE)

AICctab(m1biow,m2biow,m3biow,m4biow,m5biow,m8biow,m9biow,m10biow,m11biow,m12biow,m13biow, nobs = 17153, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(m11biow) #as relative humidity and sun altitude increase so does aci
assump(m11biow) #as evaporation rate and sun altitude increase aci decreases
Anova(m11biow, type='III')
contrasts = glht(m11biow, linfct = mcp(water_int = "Tukey"))
options(max.print = 999999)
summary(contrasts)

# Testing for Collinearity ------------------------------------------------
cor_bio = bio2 %>%
  mutate(scaled_mas = scale(mas))%>%
  dplyr::select(date_time,site,aru,arid,scaled_mas,altitude,left_channel)
cor(cor_bio[,c(3:6)])
cor.test(cor_arid$num_vocals,cor_arid$species_diversity)

m1bio = lmer(log(left_channel) ~ 1 +(1|site/aru),data=bio,REML=FALSE)
m2bio = lmer(log(left_channel) ~ arid +(1|site/aru),data=bio,REML=FALSE)
m3bio = lmer(log(left_channel) ~ altitude +(1|site/aru),data=bio,REML=FALSE)
m4bio = lmer(log(left_channel) ~ arid+altitude +(1|site/aru),data=bio,REML=FALSE)
m5bio = lmer(log(left_channel) ~ arid*altitude +(1|site/aru),data=bio,REML=FALSE)
m6bio = lmer(log(left_channel) ~ scale(mas) +(1|site/aru),data=bio,REML=FALSE)
m7bio = lmer(log(left_channel) ~ scale(mas)+arid +(1|site/aru),data=bio,REML=FALSE)
m8bio = lmer(log(left_channel) ~ scale(mas)*arid +(1|site/aru),data=bio,REML=FALSE)
m9bio = lmer(log(left_channel) ~ relh +(1|site/aru),data=bio,REML=FALSE)
m10bio = lmer(log(left_channel) ~ relh+altitude +(1|site/aru),data=bio,REML=FALSE)
m11bio = lmer(log(left_channel) ~ relh*altitude +(1|site/aru),data=bio,REML=FALSE)
m12bio = lmer(log(left_channel) ~ gh +(1|site/aru),data=bio,REML=FALSE)
m13bio = lmer(log(left_channel) ~ gh+altitude +(1|site/aru),data=bio,REML=FALSE)
m14bio = lmer(log(left_channel) ~ gh*altitude +(1|site/aru),data=bio,REML=FALSE)

AICctab(m1bio,m2bio,m3bio,m6bio,m9bio,m10bio,m11bio,m12bio,m13bio,m14bio, nobs = 61661, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(m14bio) #as evaporation rate and altitude increase, bio decreases
assump(m14bio)
summary(m1_aci)
assump(m1_aci)
Anova(m1_aci, type='III')
contrasts = glht(m1_aci, linfct = mcp(site = "Tukey"))
options(max.print = 999999)
summary(contrasts)

acidf = as.data.frame(cbind(as_date(aci2$date_time),aci2$hour))








