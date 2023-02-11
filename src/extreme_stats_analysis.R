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

# Climate ECE - top 5% of aridity data
# Threshold ECE - when biological response crashes/threshold

# Climate ECE - Loading MAS and Date Aridity Gradient Data ------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis")
# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
load("data_clean/aridity_data_clean.Rdata")

extreme_arid_n = aw4 %>%
  arrange(desc(gh_within)) %>%
  group_by(site) %>%
  dplyr::summarise(n = n(),
         top5per = 0.05*n())


# Aridwithin Dataframe Subsetting -----------------------------------------

exa_lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>% # 12006 obs
  # mutate(gh_within = scale_this(gh)) %>%
  arrange(desc(gh)) %>%
  slice_max(gh,n = 601)

exa_sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>% # 12461 obs
  arrange(desc(gh)) %>%
  slice_max(gh,n = 623)

exa_cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>% # 13158 obs
  arrange(desc(gh)) %>%
  slice_max(gh,n = 658)

exa_kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>% # 16325 obs
  arrange(desc(gh)) %>%
  slice_max(gh,n = 816) # min gh_within = 

extreme_aridwithin = rbind(exa_lwma, exa_sswma, exa_cbma, exa_kiowa)

ea_aridwithin = extreme_aridwithin %>%
  dplyr::select(site, gh ,arid_within) %>%
  group_by(site) %>%
  dplyr::summarise(min_aw = min(arid_within),
                   max_aw = max(arid_within),
                   min_gh = min(gh),
                   max_gh = max(gh))
# max lwma  gh = -15.9
# max sswma gh = -12.8
# max cbma  gh = -3.18
# max kiowa gh = -2.76

#checking to see if that is really the case
# gh2 = ((25+(19*exa_lwma$ws2m))* 1 *(max_sat(exa_lwma$temp)-(exa_lwma$relh/100)))

# Arid Across Dataframe Subsetting ----------------------------------------

exa_lwma2 = aw4 %>%
  dplyr::filter(site == "lwma") %>%
  # mutate(gh_within = scale_this(gh)) %>%
  arrange(desc(arid_across)) %>%
  slice_max(arid_across,n = 601)

exa_sswma2 = aw4 %>%
  dplyr::filter(site == "sswma") %>%
  arrange(desc(arid_across)) %>%
  slice_max(arid_across,n = 620)

exa_cbma2 = aw4 %>%
  dplyr::filter(site == "cbma") %>%
  arrange(desc(arid_across)) %>%
  slice_max(arid_across,n = 662)

exa_kiowa2 = aw4 %>%
  dplyr::filter(site == "kiowa") %>%
  arrange(desc(arid_across)) %>%
  slice_max(arid_across,n = 816) # min gh_within = 

extreme_aridacross = rbind(exa_lwma2, exa_sswma2, exa_cbma2, exa_kiowa2)

ea_aridacross = extreme_aridacross %>%
  dplyr::select(site, gh, arid_across) %>%
  group_by(site) %>%
  dplyr::summarise(min = min(arid_across),
                   max = max(arid_across))

# Climate ECE - Simple Plots ------------------------------------------------------------
# Full Dataset
ggplot(data = extreme_aridacross, aes(x = arid_within, y = pc2, color = site)) +
  # geom_point() +
  geom_smooth(method = loess)
# we do get threshold for cbma but not the other sites

# MAS Summarized data
ggplot(data = aw4, aes(x = arid_within, y = pc2, color = site)) +
  # geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 1, color = "red")


# Climate ECE - Statistical Analysis ----------------------------------------------------

m1ex = lmer(pc1 ~ mas_bin + (1|site), data = extreme_aridwithin)
summary(m1ex)  
assump(m1ex)
emmeans(m1ex, ~ mas_bin)

m2ex = lmer(pc2 ~ site + (1|site), data = extreme_aridwithin)
summary(m2ex)  
assump(m2ex)
emmeans(m2ex, pairwise ~ site)


m3ex = lm(pc3 ~ arid_within*mas_bin + scale(date), data = extreme_arid)
summary(m3ex)  

# Climate ECE - Summarizing data into MAS and Date for Arid Within --------------------------------------

exaw_mas = extreme_aridwithin %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  # mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity, 
                           temp:dew, 
                           gh, 
                           # gh_within,
                           # arid_within, 
                           # hist_within:arid_across,
                           # arid_withinf:hist_acrossf,
                           sound_atten04:sound_atten12,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) 
# %>%
#   mutate_at(c("arid_withinf",
#               "arid_acrossf",
#               "hist_withinf",
#               "hist_acrossf"), round_factor) 

exaa_mas = extreme_aridacross %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity, 
                           temp:dew, 
                           gh, 
                           gh_within,
                           arid_within, 
                           hist_within:arid_across,
                           arid_withinf:hist_acrossf,
                           sound_atten04:sound_atten12,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) %>%
  mutate_at(c("arid_withinf",
              "arid_acrossf",
              "hist_withinf",
              "hist_acrossf"), round_factor) 


# Climate ECE - MAS and Date ----------------------------------------------
ex1mas = lm(pc1 ~ site + scale(date), data = exaw_mas)
summary(ex1mas)
assump(ex1mas)
emmeans(ex1mas, pairwise ~ site)

ece_pc1_mas = ece_contrast_mas(exaw_mas, 
                               exaw_mas$pc1)
write.csv(ece_pc1_mas[[5]], 
          'results/climate_ece_mas_pc1.csv', 
          row.names = FALSE)
# ece_pc1_mas[[5]] %>% gtsave('results/climate_ece_mas_pc1.png', expand = 100)

ece_pc2_mas = ece_contrast_mas(exaw_mas, 
                               exaw_mas$pc2)
write.csv(ece_pc2_mas[[5]], 
          'results/climate_ece_mas_pc2.csv', 
          row.names = FALSE)
# ece_pc2_mas[[5]] %>% gtsave('results/climate_ece_mas_pc2.png', expand = 100)

ece_pc3_mas = ece_contrast_mas(exaw_mas, 
                               exaw_mas$pc3)
write.csv(ece_pc3_mas[[5]], 
          'results/climate_ece_mas_pc3.csv', 
          row.names = FALSE)

ece_pc3_mas[[5]] %>% gtsave('results/climate_ece_mas_pc3_arid_within.png', expand = 100)

ece_pc3_mas = ece_contrast_mas(exaa_mas, 
                               exaa_mas$pc3)
ece_pc3_mas[[5]] %>% gtsave('results/climate_ece_mas_pc3_arid_across.png', expand = 100)

# General Additive Model --------------------------------------------------

library(mgcv)
# gam1 = gam(pc2 ~ s(arid_within, by = site), data = aw6)
# gam1 = gam(pc1 ~ s(arid_within, bs = "cs", k = -1), data = aw4)
gam1 = gam(pc1 ~ s(gh, bs = "cs", k = -1), data = aw4)
gam1b = gam(pc1 ~ s(gh), data = aw4)
plot(gam1b)
summary(gam1)
gam1$smooth[[1]]$xp 
coef(gam1)
plot(gam1, se=TRUE,col="blue")
abline(v=-62, col="red")
abline(v=-54.34, col="green")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam1, -80) # inflection point for pc1 is -50.20955

emm(gam1, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))

gam2 = gam(pc2 ~ s(gh, bs = "cs", k = -1), data = aw4)
summary(gam2)
gam2b = gam(pc2 ~ s(gh), data = aw4)
plot(gam2b)
gam2$smooth[[1]]$xp #0.95
coef(gam2)
plot(gam2, se=TRUE,col="blue")
abline(v=-54.34, col="green")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam2, -70) # inflection point for pc2 is -51.74
abline(v = -51.74, col="green")
abline(v = -22.66, col = "purple")
emm(gam2, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))

gam3 = gam(pc3 ~ s(gh, bs = "cs", k = -1), data = aw4)
summary(gam3)
gam3$smooth[[1]]$xp 
coef(gam3)
plot(gam3, se=TRUE,col="blue")
abline(v=-37.96, col="green")

# Find inflection points in gam model, first load functions in inflection_points.R script
find_gam_ip(gam3, -70) # inflection point for pc3 is -54.80148
abline(v=-54.80, col="green")
abline(v = -22.66, col = "purple")
emm(gam2, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))
emm(gam3, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))
ggplot(data = aw4, aes(x = arid_within, y = pc2)) +
  geom_smooth()
# Reference: https://rpubs.com/hrlai/gam_inflection


# ECE - Impact - MCP LMM Piecewise -------------------------------------
# https://lindeloev.github.io/mcp/articles/predict.html#extracting-fitted-values-1
library(mcp)
library(rjags)
Sys.setenv(JAGS_HOME="C:/Program Files/JAGS/JAGS-4.3.0") # setting path to jags library
# plotting to see if they have similar start and end points

# Creating new arid_within variable because mcp won't recognize arid_within[,1]
aw4$arid_within2 = as.vector(aw4$arid_within)
aw4$arid_across2 = as.vector(aw4$arid_across)

# Finding Knots(changepoints) for PC1 data - Joined slopes
# Random effects included

# PC1 - Full Dataset - ADI/AEI

mcp_rem1 = list(pc1 ~ 1,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2,
                1 + (1|site) ~ 0 + arid_within2)

  pc1_fit = mcp(mcp_rem1, 
               data = aw4, 
               sample = 'prior')
  
  summary(pc1_fit)
  mcp::ranef(pc1_fit)
  plot_pars(pc1_fit, pars = c("cp_9_site[lwma]", 
                              "cp_9_site[sswma]", 
                              "cp_9_site[cbma]",
                              "cp_9_site[kiowa]"))
  
  # PC2 - Full Dataset - Num Vocals/Species Diversity
  
  mcp_rem2 = list(pc2 ~ 1,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2,
                  1 + (1|site) ~ 0 + arid_within2)
  
  pc2_fit = mcp(mcp_rem2, 
                data = aw4, 
                sample = 'prior')
  
  summary(pc2_fit)
  mcp::ranef(pc2_fit)
  plot_pars(pc2_fit, pars = "cp_9")
  plot_pars(pc2_fit, pars = c("cp_9_site[lwma]", 
                              "cp_9_site[sswma]", 
                              "cp_9_site[cbma]",
                              "cp_9_site[kiowa]"))

  # PC3 - Full Dataset - Num Vocals/Species Diversity
  
  mcp_rem3 = list(pc3 ~ 1,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2,
                  1 + (1|site) ~ 0 + arid_across2)
  
  pc3_fit = mcp(mcp_rem3, 
                data = aw4, 
                sample = 'prior')
  
  summary(pc3_fit)
  mcp::ranef(pc3_fit)
  plot_pars(pc3_fit, pars = "cp_9")
  plot_pars(pc3_fit, pars = c("cp_9_site[lwma]", 
                              "cp_9_site[sswma]", 
                              "cp_9_site[cbma]",
                              "cp_9_site[kiowa]"))
  
# predict results using mcp_rem model and new data generated below
new_x = rep(seq(-2, 3), 4)
sites = c("lwma", 'sswma', 'cbma', 'kiowa')

newdata = NULL
for(s in sites) {
  new_x = seq(-2, 2)
  # site = rep(s, length(new_x))
  df_temp = data.frame(site = s,
                       arid_within2 = new_x)
  newdata = rbind(newdata, df_temp)
}


fitted(pc1_fit, newdata = newdata)
predict_forecast = predict(pc1_fit, newdata = newdata)
summary(predict_forecast)

ggplot(data = predict_forecast, aes(x = arid_within2,
                                    y = predict,
                                    color = site)) + 
  geom_line()

plot(predict_forecast, facet_by = "site")
pp_check(fit, facet_by = "site")

# ECE - Impact - Aridity Gradient - Impact Definition Data --------------------------------

awthres = aw4 %>%
  # dplyr::filter(arid_within >0.95) %>%
  # dplyr::filter(year(date_time)==2021) %>%
  # dplyr::filter(as_date(date_time) < "2021-08-16") %>%
#   mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
#                            temp:dew, 
                           gh,
#                            gh_within,
#                            arid_within, 
#                            hist_within:arid_across,
#                            arid_withinf:hist_acrossf,
#                            sound_atten04:sound_atten12,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) 
#   mutate_at(c("arid_withinf",
#               "arid_acrossf",
#               "hist_withinf",
#               "hist_acrossf"), round_factor) 

awthres_n = awthres %>%
  group_by(site) %>%
  dplyr::summarise(total = n(),
                   percent = n()/length(awthres))

exthresm1 = lm(pc1 ~ site*mas_bin + scale(date), data = aw4 %>% dplyr::filter(gh > -50.20955))
summary(exthresm1)
emmeans(exthresm1, pairwise ~ site)
awthrespc1 = awthres %>% dplyr::filter(gh > -50.20955)
ecethres_pc1_mas = ece_contrast_mas(awthrespc1, 
                                    awthrespc1$pc1);ecethres_pc1_mas[5]
# write.csv(ecethres_pc1_mas[[5]], 
#           'results/ece_threshold_pc1_mas.csv', 
#           row.names = FALSE)

# ecethres_pc1_mas[[5]] %>% gtsave('ece_threshold_pc1_mas.png', expand = 100)
awthrespc2 = awthres %>% dplyr::filter(gh > -51.74)
ecethres_pc2_mas = ece_contrast_mas(awthrespc2, 
                                    awthrespc2$pc2)
write.csv(ecethres_pc2_mas[[5]], 
          'results/ece_threshold_pc2_mas.csv', 
          row.names = FALSE)

ecethres_pc2_mas[[5]] %>% gtsave('ece_threshold_pc2_mas.png', expand = 100)

awthrespc3 = awthres %>% dplyr::filter(gh > -54.80148)
ecethres_pc3_mas = ece_contrast_mas(awthrespc3, 
                                    awthrespc3$pc3) # -54.80148
write.csv(ecethres_pc3_mas[[5]], 
          'results/ece_threshold_pc3_mas.csv', 
          row.names = FALSE)

ecethres_pc3_mas[[5]] %>% gtsave('ece_threshold_pc3_mas.png', expand = 100)

# Finding cutoff for arid across for pc3
aathres = aw4 %>%
  dplyr::filter(arid_across >=0.98) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           #                            temp:dew, 
                           #                            gh, 
                           #                            gh_within,
                           #                            arid_within, 
                           #                            hist_within:arid_across,
                           #                            arid_withinf:hist_acrossf,
                           #                            sound_atten04:sound_atten12,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) 

ecethres_pc3_mas = ece_contrast_mas2(aathres, 
                                    aathres$pc3)
write.csv(ecethres_pc3_mas[[5]], 
          'results/ece_threshold_pc3_mas_arid_across.csv', 
          row.names = FALSE)

ecethres_pc3_mas[[5]] %>% gtsave('ece_threshold_pc3_mas_arid_across.png', expand = 100)

exthresm3 = lm(pc3 ~ site + scale(date), data = awthres)
summary(exthresm3)
emmeans(exthresm3, pairwise ~ site)


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

# Taking top 5% of arid_within values
# Plotting arid_within to make sure values are correct
ggplot(sswmawl, aes(x = gh, y = pc1, color = as.factor(ws_site))) +
  # geom_point() +
  geom_smooth(method = "gam")

hist(sswmawl$arid_within)

sswmawl_climate = sswmawl %>%
  mutate(date = as_date(date_time)) %>%
  # arrange(desc(arid_within)) %>%
  arrange(desc(gh)) %>%
  slice_max(gh,n = (7308*0.05))
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
                           gh,
                           # arid_within,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE))

hist(sswmawl_clmas$arid_within)
hist(as.numeric(sswmawl_clmas$arid_withinf)) # top 5% so only fours and fives


# PC1: ADI, AEI, positive  values more likely to have higher ADI
sswmawl_clmas_pc1 = sswma_water_climate(pc = sswmawl_clmas$pc1); sswmawl_clmas_pc1

# PC2: Num vocals and species diversity
sswmawl_clmas_pc2 = sswma_water_climate(pc = sswmawl_clmas$pc2); sswmawl_clmas_pc2

# PC3: ACI and BIO
sswmawl_clmas_pc3 = sswma_water_climate(pc = sswmawl_clmas$pc3); sswmawl_clmas_pc3

sswma_pc_climate_table = sswma_water_climate_table(sswmawl_clmas_pc1[[3]],                          sswmawl_clmas_pc2[[3]],                       sswmawl_clmas_pc3[[3]]); sswma_pc_climate_table

sswma_pc_table %>% gtsave("results/sswma_water_allpcs_lag.png",
                          expand = 100,
                          vwidth = 2000, 
                          vheight = 1500)




# ECE - Impact - Water Supp - SSWMA ---------------------------------------
# Do not need to do a GAM for water supp data, using threshold for aridity gradient experiment.
sswmawl_thres = sswmawl %>%
  # dplyr::filter(arid_within >0.95) %>% # similar to aridity gradient data
  # dplyr::filter(year(date_time)==2021) %>%
  # dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  #   mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
  dplyr::mutate(date = as_date(date_time),
                ws_site = as.factor(ws_site),
                water = as.factor(water)) %>%
  group_by(site, ws_site, water, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           gh,
                           # arid_within,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) 

sswmawl_threspc1 = sswmawl_thres %>% dplyr::filter(gh > -50.20955)
sswmawl_immas_pc1 = sswma_water_impact(sswmawl_threspc1,
                                       sswmawl_threspc1$pc1)
sswmawl_immas_pc1[[3]]

sswmawl_threspc2 = sswmawl_thres %>% dplyr::filter(gh > -51.74)
sswmawl_immas_pc2 = sswma_water_impact(sswmawl_threspc2,
                                       sswmawl_threspc2$pc2)
sswmawl_immas_pc2[[3]]

sswmawl_threspc3 = sswmawl_thres %>% dplyr::filter(gh > -54.80148)
sswmawl_immas_pc3 = sswma_water_impact(sswmawl_threspc3,
                                       sswmawl_threspc3$pc3)
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
  arrange(desc(gh)) %>%
  slice_max(gh,n = (5922*0.05))
cbmawl_climate = cbmawl_climate[1:296,]

# ECE - Climate - Water Supp - CBMA - Lag - MAS and Date ----------------------

cbmawl_clmas = cbmawl_climate %>%
  mutate(date = date(date_time),
         ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           gh,
                           # arid_within,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE))

# hist(cbmawl_clmas$arid_within)
# hist(as.numeric(cbmawl_clmas$arid_withinf)) # top 5% so only fours and fives

# PC1: ADI, AEI, positive  values more likely to have higher ADI
cbmawl_clmas_pc1 = cbma_water_climate(pc = cbmawl_clmas$pc1); cbmawl_clmas_pc1

# PC2: Num vocals and species diversity
cbmawl_clmas_pc2 = cbma_water_climate(pc = cbmawl_clmas$pc2); cbmawl_clmas_pc2

# PC3: ACI and BIO
cbmawl_clmas_pc3 = cbma_water_climate(pc = cbmawl_clmas$pc3); cbmawl_clmas_pc3

cbma_pc_climate_table = cbma_water_climate_table(cbmawl_clmas_pc1[[3]],                          cbmawl_clmas_pc2[[3]],                       cbmawl_clmas_pc3[[3]]); cbma_pc_climate_table

cbma_pc_table %>% gtsave("results/cbma_water_allpcs_lag.png",
                          expand = 100,
                          vwidth = 2000, 
                          vheight = 1500)




# ECE - Impact - Water Supp - CBMA ---------------------------------------
# Do not need to do a GAM for water supp data, using threshold for aridity gradient experiment.
cbmawl_thres = cbmawl %>%
  # dplyr::filter(arid_within >0.95) %>% # similar to aridity gradient data
  # dplyr::filter(year(date_time)==2021) %>%
  # dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  #   mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
  dplyr::mutate(date = as_date(date_time),
                ws_site = as.factor(ws_site),
                water = as.factor(water)) %>%
  group_by(site, ws_site, water, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity,
                           gh,
                           # arid_within,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE))

cbmawl_threspc1 = cbmawl_thres %>% dplyr::filter(gh > -50.20955)
cbmawl_immas_pc1 = cbma_water_impact(cbmawl_threspc1,
                                     cbmawl_threspc1$pc1)
cbmawl_immas_pc1[[3]]

cbmawl_threspc2 = cbmawl_thres %>% dplyr::filter(gh > -51.74)
cbmawl_immas_pc2 = cbma_water_impact(cbmawl_threspc2,
                                     cbmawl_threspc2$pc2)
cbmawl_immas_pc2[[3]]

cbmawl_threspc3 = cbmawl_thres %>% dplyr::filter(gh > -54.80148)
cbmawl_immas_pc3 = cbma_water_impact(cbmawl_threspc3,
                                     cbmawl_threspc3$pc3)
cbmawl_immas_pc3[[3]]

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


# CBMA - Combining Climate and Impact Tables -----------------------------

cbma_all_eces = cbma_ece_table(cbmawl_clmas_pc1[[3]],
                                 cbmawl_clmas_pc2[[3]],
                                 cbmawl_clmas_pc3[[3]],
                                 cbmawl_immas_pc1[[3]],
                                 cbmawl_immas_pc2[[3]],
                                 cbmawl_immas_pc3[[3]]);cbma_all_eces

cbma_all_eces %>% gtsave("results/cbma_water_ece_pcs.png", vwidth = 20000, vheight = 15000, expand = 100)
