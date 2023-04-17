#### Extreme Aridity Data and Analyses ####

library(tidyverse) #data manipulation
library(lubridate)
library(hms) #manipulate time
library(zoo) #for na.approx to approximate missing values in weather dataset
library(gridExtra) #ggplot multi panels
library(cowplot)
library(lme4) #lmm and glmm analysis
library(lmerTest) #get p-values for lmm tests
library(reshape2) #???
library(suncalc) #calculate sunrise time and sun altitude 
library(zoo)#approximating rows with NA in the weather data
library(car)#ANOVAs
library(bbmle) #AIC comparisons
library(emmeans)
library(gt)
library(htmltools)
library(webshot2)
library(ggbiplot) # plot pcas


# Loading files with functions --------------------------------------------


source("src/functions.R")
source("src/aridity_gradient_functions/aridity_gradient_mas_functions.R")
source("src/aridity_gradient_functions/aridity_gradient_graphs.R")
source("src/water_supp_functions/sswma_water_functions.R")
source("src/water_supp_functions/cbma_water_functions.R")
source("src/ece_functions/ece_functions.R")
source("src/ece_functions/sswma_water_ece_functions.R")
source("src/ece_functions/cbma_ece_functions.R")
source("src/evap_rate_function.R")
source("src/inflection_points.R") # used to determine thresholds for ECE analysis

# Climate ECE - top 5% of aridity data
# Threshold ECE - when biological response crashes/threshold, use gam and inflection_points to find threshold

# Climate ECE - Loading MAS and Date Aridity Gradient Data ------------------------------
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis")
# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
load("data_clean/aridity_data_clean.Rdata")

extreme_arid_n = aw4 %>%
  group_by(site) %>%
  arrange(desc(ew_vol)) %>%
  dplyr::summarise(n = n(),
         top5per = 0.05*n())


# Aridwithin Dataframe Subsetting -----------------------------------------

exa_lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>% # 12006 obs
  # mutate(gh_within = scale_this(gh)) %>%
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 601)

exa_sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>% # 12461 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 623)

exa_cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>% # 13158 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 658)

exa_kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>% # 16325 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 816) # min gh_within = 

ex_arid = rbind(exa_lwma, exa_sswma, exa_cbma, exa_kiowa)

ex_arid_n = ex_arid %>%
  dplyr::select(site, ew_vol) %>%
  group_by(site) %>%
  dplyr::summarise(min_evap = min(ew_vol),
                   max_evap = max(ew_vol))

# extreme min and max evap wind
# lwma:  0.689-1.16 
# sswma: 0.837-1.70
# cbma:  1.170-2.34
# kiowa: 1.140-2.13

# Climate ECE - Summarizing data into MAS and Date for Arid Within --------------------------------------

exaw_mas = ex_arid %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  # mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity, 
                           temp:dew, 
                           ew_vol:e1_vol,
                           atten_alpha04:atten_dist12,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) 


# Climate ECE - MAS and Date ----------------------------------------------

# PC1 - Acoustic diversity
ece_pc1_mas = ece_contrast_mas(exaw_mas, 
                               exaw_mas$pc1,
                               exaw_mas$ew_vol); ece_pc1_mas[[6]]

# PC2 - Avian Abundance
ece_pc2_mas = ece_contrast_mas(exaw_mas, 
                               exaw_mas$pc2,
                               exaw_mas$ew_vol); ece_pc2_mas[[6]]

# PC3 - Acoustic Complexity
ece_pc3_mas = ece_contrast_mas(exaw_mas, 
                               exaw_mas$pc3,
                               exaw_mas$ew_vol)

# General Additive Model to find threshold for impact ece --------------------------------------------------

library(mgcv)

gam1 = gam(pc1 ~ s(ew_vol, bs = "cs", k = -1), data = aw4)
summary(gam1)
gam1$smooth[[1]]$xp 
coef(gam1)
plot(gam1, se=TRUE,col="blue")
abline(v=14.0154, col="red")
abline(v=-54.34, col="green")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam1, 1.0) # inflection point for pc1 is 1.40154


gam2 = gam(pc2 ~ s(ew_vol, bs = "cs", k = -1), data = aw4)
summary(gam2)
gam2$smooth[[1]]$xp #0.95
coef(gam2)
plot(gam2, se=TRUE,col="blue")
abline(v=-54.34, col="green")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam2, 0.5) # inflection point for pc2 is 0.7302139
abline(v = 0.7302139, col="green")


gam3 = gam(pc3 ~ s(ew_vol, bs = "cs", k = -1), data = aw4)
summary(gam3)
gam3$smooth[[1]]$xp 
coef(gam3)
plot(gam3, se=TRUE,col="blue")
abline(v=1.071766, col="green")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam3, 1.0) # inflection point for pc3 is 1.071766, but slope becomes significantly positive later
abline(v= 1.071766, col="green")

# Reference: https://rpubs.com/hrlai/gam_inflection

# ECE - Impact - Aridity Gradient - Impact Definition Data --------------------------------
# Create impact data frame
awthres = aw4 %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           temp:dew,
                             gh,
                             evap_wind:e1_vol,
                             atten_alpha04:atten_dist12,
                             pc1:pc3), ~ mean(.x, na.rm = TRUE))

awthres_n = awthres %>%
  group_by(site) %>%
  dplyr::summarise(total = n(),
                   percent = n()/length(awthres))

# PC1 threshold dataframe, filtered by values 
# greater than or equal to 1.40154
awthrespc1 = awthres %>% dplyr::filter(ew_vol > 1.40154)
ecethres_pc1_mas = ece_impact_mas1(awthrespc1, 
                                    awthrespc1$pc1,
                                    awthrespc1$ew_vol);ecethres_pc1_mas[6]

awthrespc2 = awthres %>% dplyr::filter(ew_vol > 0.7302139)
ecethres_pc2_mas = ece_impact_mas2(awthrespc2, 
                                    awthrespc2$pc2,
                                    awthrespc2$ew_vol);ecethres_pc2_mas[6]

awthrespc3 = awthres %>% dplyr::filter(ew_vol > 1.071766)
ecethres_pc3_mas = ece_impact_mas3(awthrespc3, 
                                    awthrespc3$pc3,
                                    awthrespc3$gh);ecethres_pc3_mas[6]

# ECE - Climate and Impact Tables Together -----------------------------

# All PCs for Climate and Impact ECE in One Table 
ece_pc_table = ece_tables_combined2(ece_pc1_mas[[5]],
                    ecethres_pc1_mas[[5]],
                    ece_pc2_mas[[5]],
                    ecethres_pc2_mas[[5]],
                    ece_pc3_mas[[5]],
                    ecethres_pc3_mas[[5]]
                    ); ece_pc_table
ece_pc_table %>% gtsave("results/ece_all_pcs.png", vwidth = 2000, vheight = 1500, expand = 100)

### ECE Aridity gradient slopes table - climate and threshold combined table
# create NA table for PC3 impact table
na_table = data.frame(site = c(NA,NA,NA),
                      xvar.trend = c(NA,NA,NA),
                      SE = c(NA,NA,NA),
                      df = c(NA,NA,NA),
                      lower.CL = c(NA,NA,NA),
                      upper.CL = c(NA,NA,NA))

ece_all_tables_table = ece_tables_slopes(ece_pc1_mas[[6]],
                                        ecethres_pc1_mas[[6]],
                                        ece_pc2_mas[[6]],
                                        ecethres_pc2_mas[[6]],
                                        ece_pc3_mas[[6]],
                                        na_table)

ece_all_tables_table %>% gtsave("results/ece_all_pcs_slopes.png", 
                                vwidth = 1100, 
                                vheight = 1500, 
                                expand = 10)



# ECE - Climate - Water Supp - SSWMA - Lag - Data Organization --------------------------------------

### Only analyzing half of water supplementation period
load("data_clean/filtered_water_supp_data.Rdata")

sswma_water = ww3 %>%
  dplyr::filter(site == "sswma") %>%
  mutate(ws_site = as.factor(ws_site),
         water = as.factor(water),
         date = date(date_time),
         week = week(date_time)) %>%
  arrange(date_time,ws_site,water)

sswma_water_pca = prcomp(sswma_water[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
summary(sswma_water_pca) #PC1 and PC2 have highest proportion of variance
sswma_water_pcadf = as.data.frame(sswma_water_pca[["x"]])
ggbiplot(sswma_water_pca, choices = c(1,2), ellipse = TRUE, alpha = 0, groups = sswma_water$ws_site) 
ggbiplot(sswma_water_pca, choices = c(1,3), ellipse = TRUE, alpha = 0, groups = sswma_water$ws_site) 

### PC1: ADI and AEI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and higher BIO

sswma_water$pc1 = sswma_water_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
sswma_water$pc2 = sswma_water_pcadf$PC2*-1 # Multiply PC2 by -1 to make num-vocals and species diversity have positive values
sswma_water$pc3 = sswma_water_pcadf$PC3
#*-1 # multiply pc3 by -1 to make aci values have positive values

### SSWMA
# Separating out SSWMA water sites
sswma_wlag1 = sswma_water %>%
  filter(aru == "ws01"| aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05")%>%
  mutate(water = ifelse(date(date_time) >= "2021-05-23" & date(date_time) <"2021-05-30"| date(date_time) >= "2021-06-22" & date(date_time) < "2021-07-02", 1,0),
         ws_site = 1)

sswma_wlag2 = sswma_water %>%
  filter(aru == "ws06"| aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10") %>%
  mutate(water = ifelse(date(date_time) >= "2021-06-06" & date(date_time) <"2021-06-12"| date(date_time) >= "2021-07-21" & date(date_time) < "2021-08-07", 1,0),
         ws_site = 2)

sswma_wlag3 = sswma_water %>%
  filter(aru == "ws11"| aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15") %>%
  mutate(water = 0,
         ws_site = 3)

sswma_wlag = rbind(sswma_wlag1, 
                   sswma_wlag2, 
                   sswma_wlag3)

sswmawl = sswma_wlag %>%
  dplyr::filter(date(date_time)>= "2021-05-23" & date(date_time) <"2021-05-30"| date(date_time) >= "2021-06-22" & date(date_time) < "2021-07-02" & date(date_time) >= "2021-06-06" & date(date_time) <"2021-06-12"| date(date_time) >= "2021-07-21" & date(date_time) < "2021-08-07") %>% dplyr::mutate(date = as_date(date_time))

# sswma_waterpca = prcomp(sswmawl[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
# 
# sswma_waterpcadf = as.data.frame(sswma_waterpca[["x"]])
# ggbiplot(sswma_waterpca, choices = c(1,3),ellipse = TRUE, alpha = 0, groups = sswmawl$site) # Plot PCs

# #3D pCA Plot
# pca3d(sswma_waterpca, biplot = true) # only run this on windows machine
# snapshotPCA3d("sswma_water_lag_pca.png")

### PC1: ADI and AEI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and BIO

# sswmawl$pc1 = sswma_waterpcadf$PC1*-1
# sswmawl$pc2 = sswma_waterpcadf$PC2*-1
# sswmawl$pc3 = sswma_waterpcadf$PC3

# Taking top 5% of evaporation rate (ew_vol) values

sswmawl_climate = sswmawl %>%
  mutate(date = as_date(date_time)) %>%
  # arrange(desc(arid_within)) %>%
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = (7308*0.05))
# sswmawl_climate = sswmawl_climate[1:384,]

clsswma_dates = unique(sswmawl_climate$date) # dates with climate ece aridity:
# 2021-05-25, 2021-07-22, 2021-07-24, 2021-07-27, 2021-07-28, 2021-07-29, 
# 2021-07-30, 2021-07-31, 2021-08-03, 2021-08-04, 2021-08-05, 2021-08-06

# # filtering based on full day
# sswmawl_climate2 = sswmawl %>%
#   mutate(date = as_date(date_time)) %>%
#   filter(date == "2021-05-25" | 
#          date == "2021-07-22" | 
#          date == "2021-07-24" | 
#          date >= "2021-07-27" & date <= "2021-07-31" |
#          date >= "2021-08-03" & date <= "2021-08-06") 
# 
# # filtering based on next day after extreme aridity
# sswmawl_climate3 = sswmawl %>%
#   mutate(date = as_date(date_time)) %>%
#   filter(date == "2021-05-26" | 
#            date == "2021-07-23" | 
#            date == "2021-07-25" | 
#            date >= "2021-07-28" & date <= "2021-08-01" |
#            date >= "2021-08-04" & date <= "2021-08-07") 

# ECE - Climate - Water Supp - SSWMA - Lag - MAS and Date ----------------------

sswmawl_clmas = sswmawl_climate %>%
# sswmawl_clmas = sswmawl_climate2 %>%
# sswmawl_clmas = sswmawl_climate3 %>%
  mutate(date = date(date_time),
         ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           evap_wind:e1_vol,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE))

# hist(sswmawl_clmas$arid_within)
# hist(as.numeric(sswmawl_clmas$arid_withinf)) # top 5% so only fours and fives


# PC1: ADI, AEI, positive  values more likely to have higher ADI
sswmawl_clmas_pc1 = sswma_water_climate(data = sswmawl_clmas,
                                        yvar = sswmawl_clmas$pc1,
                                        xvar = sswmawl_clmas$ew_vol); sswmawl_clmas_pc1

# PC2: Num vocals and species diversity
sswmawl_clmas_pc2 = sswma_water_climate(data = sswmawl_clmas,
                                        yvar = sswmawl_clmas$pc2,
                                        xvar = sswmawl_clmas$ew_vol); sswmawl_clmas_pc2

# PC3: ACI and BIO
sswmawl_clmas_pc3 = sswma_water_climate(data = sswmawl_clmas,
                                        yvar = sswmawl_clmas$pc1,
                                        xvar = sswmawl_clmas$ew_vol); sswmawl_clmas_pc3

sswma_pc_climate_table = sswma_water_climate_table(sswmawl_clmas_pc1[[3]],                          sswmawl_clmas_pc2[[3]],                       sswmawl_clmas_pc3[[3]]); sswma_pc_climate_table
# 
sswma_pc_climate_table %>% gtsave("results/sswmawl_lag_climate_ece_tables.png",
                          expand = 10,
                          vwidth = 1100,
                          vheight = 1500)

# ECE - Impact - Water Supp - SSWMA ---------------------------------------
# Do not need to do a GAM for water supp data, using threshold for aridity gradient experiment.
sswmawl_thres = sswmawl %>%
  dplyr::mutate(date = as_date(date_time),
                ws_site = as.factor(ws_site),
                water = as.factor(water)) %>%
  group_by(site, ws_site, water, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           evap_wind:e1_vol,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) 


# Determine Thresholds for SSWMA Water Supplementation experiment ---------
library(mgcv)

gam1 = gam(pc1 ~ s(ew_vol, bs = "cs", k = -1), 
           data = sswmawl)
summary(gam1)
gam1$smooth[[1]]$xp 
coef(gam1)
plot(gam1, se=TRUE,col="blue")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam1, 0.5) # inflection point for sswma pc1 is 0.5906158 but becomes positive afterwards
abline(v=0.5906158, col="red")

gam2 = gam(pc2 ~ s(ew_vol, bs = "cs", k = -1), data = sswmawl)
summary(gam2)
gam2$smooth[[1]]$xp #0.95
coef(gam2)
plot(gam2, se=TRUE,col="blue")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam2, 0.5) # inflection point for pc2 is 0.6060231 but becomes positive afterwards
abline(v = 0.6060231, col="green")

gam3 = gam(pc3 ~ s(ew_vol, bs = "cs", k = -1),
           data = sswmawl)
summary(gam3)
gam3$smooth[[1]]$xp 
coef(gam3)
plot(gam3, se=TRUE,col="blue")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam3, 0.3) # inflection point for pc3 is 0.5443937, but slope becomes significantly positive after
abline(v= 0.5443937, col="green")

# Reference: https://rpubs.com/hrlai/gam_inflection


# SSWMA Impact ECE Stats Analysis -----------------------------------------


sswmawl_threspc1 = sswmawl_thres %>% 
  dplyr::filter(ew_vol >= 0.5906158)
sswmawl_immas_pc1 = sswma_water_impact(sswmawl_threspc1,
                                       sswmawl_threspc1$pc1,
                                       sswmawl_threspc1$ew_vol)
sswmawl_immas_pc1[[3]]

sswmawl_threspc2 = sswmawl_thres %>% dplyr::filter(ew_vol >= 0.6060231)
sswmawl_immas_pc2 = sswma_water_impact(sswmawl_threspc2,
                                       sswmawl_threspc2$pc2,
                                       sswmawl_threspc2$ew_vol)
sswmawl_immas_pc2[[3]]

sswmawl_threspc3 = sswmawl_thres %>% dplyr::filter(ew_vol >= 0.5443937)
sswmawl_immas_pc3 = sswma_water_impact(sswmawl_threspc3,
                                       sswmawl_threspc3$pc3,
                                       sswmawl_threspc3$ew_vol)
sswmawl_immas_pc3[[3]]

# # Finding cutoff for arid across for pc3
# aathres = aw4 %>%
#   dplyr::filter(arid_across >=0.98) %>%
#   group_by(site, date, mas_bin) %>%
#   dplyr::summarise_at(vars(aci:species_diversity,
#                            # temp:dew, 
#                            # gh, 
#                            # gh_within,
#                            # arid_within, 
#                            # hist_within:arid_across,
#                            # arid_withinf:hist_acrossf,
#                            # sound_atten04:sound_atten12,
#                            pc1:pc3), ~ mean(.x, na.rm = TRUE)) 
# 
# ecethres_pc3_mas = ece_contrast_mas2(aathres, 
#                                      aathres$pc3)
# write.csv(ecethres_pc3_mas[[5]], 
#           'results/ece_threshold_pc3_mas_arid_across.csv', 
#           row.names = FALSE)
# 
# ecethres_pc3_mas[[5]] %>% gtsave('ece_threshold_pc3_mas_arid_across.png', expand = 100)
# 
# exthresm3 = lm(pc3 ~ site + scale(date), data = awthres)
# summary(exthresm3)
# emmeans(exthresm3, pairwise ~ site)


# SSWMA - Combining Climate and Impact Tables -----------------------------

sswma_all_eces = sswma_ece_table(sswmawl_clmas_pc1[[3]],
                 sswmawl_clmas_pc2[[3]],
                 sswmawl_clmas_pc3[[3]],
                 sswmawl_immas_pc1[[3]],
                 sswmawl_immas_pc2[[3]],
                 sswmawl_immas_pc3[[3]]);sswma_all_eces

sswma_all_eces %>% gtsave("results/sswma_water_ece_pcs.png", vwidth = 20000, vheight = 15000, expand = 100)


# ECE - Climate - Water Supp - CBMA - Lag - Data Organization --------------------------------------

load("data_clean/filtered_water_supp_data.Rdata")

cbma_water = ww3 %>%
  dplyr::filter(site == "cbma") %>%
  mutate(ws_site = as.factor(ws_site),
         water = as.factor(water),
         date = date(date_time),
         week = week(date_time)) %>%
  arrange(date_time,ws_site,water)

cbma_water_pca = prcomp(cbma_water[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
summary(cbma_water_pca) 
cbma_water_pcadf = as.data.frame(cbma_water_pca[["x"]])
ggbiplot(cbma_water_pca, choices = c(1,2), ellipse = TRUE, alpha = 0, groups = cbma_water$ws_site) # need to multiply pc1 by -1
ggbiplot(cbma_water_pca, choices = c(1,3), ellipse = TRUE, alpha = 0, groups = cbma_water$ws_site) # need to multiply pc3 by -1

### PC1: ADI and AEI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and higher BIO

cbma_water$pc1 = cbma_water_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
cbma_water$pc2 = cbma_water_pcadf$PC2 # Multiply PC2 by -1 to make num-vocals and species diversity have positive values
cbma_water$pc3 = cbma_water_pcadf$PC3*-1

#Separating out CBMA water sites
cbma_wlag1 = cbma_water %>%
  dplyr::filter(year(date_time) == 2021) %>%
  mutate(date = date(date_time)) %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date < "2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

cbma_wlag2 = cbma_water %>%
  mutate(date = date(date_time)) %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)

# Lag by week
cbmawl = rbind(cbma_wlag1, cbma_wlag2) %>% 
  dplyr::filter(date >="2021-05-28"  & date < "2021-06-04" | # ws1 water open, closed on 2021-06-04
                  date >= "2021-06-18" & date < "2021-06-25" | # ws1 water closed, opened on 2021-06-25
                  date >= "2021-07-12" & date < "2021-07-19" | # ws1 water open, closed on 2021-07-19
                  date >= "2021-07-27" & date < "2021-08-02" | # ws1 water closed, open on 2021-08-02
                  date >= "2021-08-09" & date < "2021-08-16") %>% # ws1 water open, end of experiment on 2021-08-15
  dplyr::mutate(ws_site = as.factor(ws_site),
                water = as.factor(water))

# cbma_waterlagpca = prcomp(cbmawl[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
# 
# cbma_waterpcadf = as.data.frame(cbma_waterlagpca[["x"]])
# ggbiplot(cbma_waterlagpca, choices = c(1,2),ellipse = TRUE, alpha = 0) # Plot PCs

# #3D pCA Plot
# pca3d(cbmawl, biplot = true) # only run this on windows machine
# snapshotPCA3d("cbma_water_lag_pca.png")

### PC1: ADI, AEI, ACI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity higher positive values = higher num vocals and species diversity (after running line 699)
### PC3: BIO, higher values = higher BIO

# cbmawl$pc1 = cbma_waterpcadf$PC1 # Higher ADI increases with positive values already
# cbmawl$pc2 = cbma_waterpcadf$PC2 * -1 # switching direction of Num Vocals/Species Diversity so that it is positive
# cbmawl$pc3 = cbma_waterpcadf$PC3 # Higher ACI and BIO with higher positive values

cbmawl_climate = cbmawl %>%
  # mutate(gh_within = scale_this(gh)) %>%
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = (5922*0.05))
cbmawl_climate = cbmawl_climate[1:296,]

# ECE - Climate - Water Supp - CBMA - Lag - MAS and Date ----------------------

cbmawl_clmas = cbmawl_climate %>%
  mutate(date = date(date_time),
         ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           evap_wind:e1_vol,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE))

# hist(cbmawl_clmas$arid_within)
# hist(as.numeric(cbmawl_clmas$arid_withinf)) # top 5% so only fours and fives

# PC1: ADI, AEI, positive  values more likely to have higher ADI
cbmawl_clmas_pc1 = cbma_water_climate(data = cbmawl_clmas,
                                      yvar = cbmawl_clmas$pc1,
                                      xvar = cbmawl_clmas$ew_vol); cbmawl_clmas_pc1

# PC2: Num vocals and species diversity
cbmawl_clmas_pc2 = cbma_water_climate(data = cbmawl_clmas,
                                      yvar = cbmawl_clmas$pc2,
                                      xvar = cbmawl_clmas$ew_vol); cbmawl_clmas_pc2

# PC3: ACI and BIO
cbmawl_clmas_pc3 = cbma_water_climate(data = cbmawl_clmas,
                                      yvar = cbmawl_clmas$pc3,
                                      xvar = cbmawl_clmas$ew_vol); cbmawl_clmas_pc3

cbma_excl = cbma_water_climate_table(cbmawl_clmas_pc1[[3]],
                                     cbmawl_clmas_pc2[[3]],
                                     cbmawl_clmas_pc3[[3]]);cbma_excl

cbma_excl %>% gtsave("results/cbma_water_exclim.png",
                          expand = 10,
                          vwidth = 1100, 
                          vheight = 1500)

cbma_exclooxml = cbma_excl %>% as_word();View(cbma_exclooxml)
# Determine Thresholds for CBMA Water Supplementation experiment ---------
library(mgcv)

gam1 = gam(pc1 ~ s(ew_vol, bs = "cs", k = -1), 
           data = cbmawl)
summary(gam1)
gam1$smooth[[1]]$xp 
coef(gam1)
plot(gam1, se=TRUE,col="blue")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam1, 1.5) # inflection point for sswma pc1 is 1.542871 but becomes significantly negative afterwards
abline(v = 1.542871, col="red")

gam2 = gam(pc2 ~ s(ew_vol, bs = "cs", k = -1), data = cbmawl)
summary(gam2)
gam2$smooth[[1]]$xp #0.95
coef(gam2)
plot(gam2, se=TRUE,col="blue")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam2, 1.2) # inflection point for pc2 is 1.24843 
abline(v = 1.24843, col="green")

gam3 = gam(pc3 ~ s(ew_vol, bs = "cs", k = -1),
           data = cbmawl)
summary(gam3)
gam3$smooth[[1]]$xp 
coef(gam3)
plot(gam3, se=TRUE,col="blue")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam3, 1.4) # inflection point for pc3 is 1.589982, but slope becomes significantly negative after
abline(v= 1.589982, col="green")

# Reference: https://rpubs.com/hrlai/gam_inflection


# ECE - Impact - Water Supp - CBMA ---------------------------------------
# Do not need to do a GAM for water supp data, using threshold for aridity gradient experiment.
cbmawl_thres = cbmawl %>%
  dplyr::mutate(date = as_date(date_time),
                ws_site = as.factor(ws_site),
                water = as.factor(water)) %>%
  group_by(site, ws_site, water, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           evap_wind:ew_vol,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE))

cbmawl_threspc1 = cbmawl_thres %>% dplyr::filter(ew_vol >= 1.542871)
cbmawl_immas_pc1 = cbma_water_impact(cbmawl_threspc1,
                                     cbmawl_threspc1$pc1,
                                     cbmawl_threspc1$ew_vol)
cbmawl_immas_pc1

cbmawl_threspc2 = cbmawl_thres %>% dplyr::filter(ew_vol >= 1.24843)
cbmawl_immas_pc2 = cbma_water_impact(cbmawl_threspc2,
                                     cbmawl_threspc2$pc2,
                                     cbmawl_threspc2$ew_vol);cbmawl_immas_pc2[[3]]

cbmawl_threspc3 = cbmawl_thres %>% dplyr::filter(ew_vol >=1.589982)
cbmawl_immas_pc3 = cbma_water_impact(cbmawl_threspc3,
                                     cbmawl_threspc3$pc3,
                                     cbmawl_threspc3$ew_vol)
cbmawl_immas_pc3[[3]]

#### CHECK TO SEE IF YOU CAN USE FULL DATASET

# CBMA - Combining Climate and Impact Tables -----------------------------

na_table2 = data.frame(contrast = c(NA,NA),
                      estimate = c(NA,NA),
                      SE = c(NA,NA),
                      df = c(NA,NA),
                      t.ratio = c(NA,NA),
                      p.value = c(NA,NA))

cbma_all_eces = cbma_ece_table(cbmawl_clmas_pc1[[3]],
                               cbmawl_clmas_pc2[[3]],
                               cbmawl_clmas_pc3[[3]],
                               # cbmawl_immas_pc1[[3]],
                               na_table2,
                               cbmawl_immas_pc2[[3]],
                               na_table2);cbma_all_eces

cbma_all_eces %>% gtsave("results/cbma_water_ece_pcs.png", vwidth = 1100, vheight = 1500, expand = 10)
