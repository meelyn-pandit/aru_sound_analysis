###########################################################
#### Reproducible Example of Aridity Gradient Analysis ####
###########################################################


# Install packages not yet installed --------------------------------------

# Package names
packages = c("tidyverse", "RColorBrewer", "viridis", "lubridate", "hms", "zoo", "gridExtra", "lme4", "lmerTest", "reshape2", "suncalc", "car", "multcomp", "bbmle", "performance", "emmeans", "magrittr", "gt","htmltools", "webshot2", "ggbiplot", "broom", "docstring")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

library(tidyverse) #data manipulation
library(RColorBrewer) # brewer color palette
library(viridis) # viridis color palette
library(lubridate) #manipulating date and time
library(hms) #manipulate time
library(gridExtra) #ggplot multi panels
library(lme4) #lmm and glmm analysis
library(lmerTest) #get p-values for lmm tests
library(reshape2) #???
library(suncalc) #calculate sunrise time and sun altitude 
library(zoo)#approximating rows with NA in the weather data
library(car)#ANOVAs
library(multcomp) #posthoc tests for ANOVA type III effects
library(bbmle) #AIC comparisons
library(performance) #performance
library(emmeans)
library(magrittr)
library(gt)
library(htmltools)
library(webshot2)
library(ggbiplot) # plot pcas
library(broom)
library(docstring)


# Load Scripts with Functions ---------------------------------------------

source("src/functions.R") # file with some useful functions
source("src/aridity_gradient_functions/aridity_gradient_mas_functions.R") # functions to analyze linear models, find site and mas_bin specific slopes, contrasts across sites and across time periods, and make gt tables
source("src/aridity_gradient_functions/aridity_gradient_graphs.R") # make graphs for aridity gradient
source("src/water_supp_functions/sswma_water_functions.R") # analyze sswma water supplementation data
source("src/water_supp_functions/cbma_water_functions.R") # analyze cbma water supplementation data
source("src/ece_functions/ece_functions.R") # subset and analyze aridity gradient data for extreme climate events based on climate and impact definition
source("src/ece_functions/sswma_water_ece_functions.R") # subset and analyze sswma water data for extreme climate events based on climate and impact definition
source("src/ece_functions/cbma_ece_functions.R") # subset and analyze cbma water data for extreme climate events based on climate and impact definition
source("src/evap_rate_function.R") # functions to calculate evaporation rate
source("src/inflection_points.R") # used to determine thresholds for ECE analysis

# Load Full Dataset and Clean ---------------------------------------------

load("data_clean/audio_and_weather_data.Rdata")

# Aridity Gradient - Create PCA of Audio Variables, filter out files with NA ACI and  --------
aw_bad1 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-07-09" & site == "lwma" & aru == "aru04") 
aw_bad2 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-07-10" & site == "lwma" & aru == "aru04")
aw_bad3 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-28" & site == "kiowa" & aru == "aru05") 
aw_bad4 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-29" & site == "kiowa" & aru == "aru05")
aw_bad5 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-30" & site == "kiowa" & aru == "aru05")
aw_bad6 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-05-28" & site == "cbma" & aru == "aru03")

aw_bad_total = rbind(aw_bad1,aw_bad2,aw_bad3,aw_bad4,aw_bad5,aw_bad6)
aw3 = setdiff(aw2, aw_bad_total)
aw3$site = factor(aw3$site, levels = c("lwma","sswma","cbma","kiowa"))
aw3 = aw3 %>% dplyr::filter(is.na(mas_bin) == FALSE)

# enter in 0 for rows with rain == NA
aw3 = aw3 %>%
  dplyr::mutate(rain = replace_na(rain,0))

# Filter out NA aci and only look at values before 2021-08-16
aw4 = aw3 %>%
  dplyr::filter(is.na(aci) == FALSE) %>%
  dplyr::filter(rain == 0) %>% # filtering out 5 min bins with rain
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  mutate(date = date(date_time),
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
  dplyr::mutate(ew_vol = evap_wind*0.1, # volume of water (mL) being evaporated per day from a circular pan with a radius of 10cm, units are mL/cm^2/day
                e1_vol = evap_1*0.1) %>% # volume of water (mL) being evaporated per day from a circular pan with a radius of 10cm, units are mL/cm^2/day
  dplyr::mutate(atten_alpha04 = att_coef(4000, temp, relh, Pa = (pres/1000)), # sound attenuation coefficient at 4 kHz
                atten_alpha08 = att_coef(8000, temp, relh, Pa = (pres/1000)), # sound attenuation coefficient at 8 kHz
                atten_alpha12 = att_coef(12000, temp, relh, Pa = (pres/1000))) %>% # sound attenuation coefficient at 12 kHz
  dplyr::mutate(atten_dist04 = aud_range(f = 4000, # sound distance travelled based on attenuation at 4 khz
                                         T_cel = temp, 
                                         h_rel = relh, 
                                         Pa = (pres/1000)),
                atten_dist08 = aud_range(f = 8000, # sound distance travelled based on attenuation at 8 khz
                                         T_cel = temp, 
                                         h_rel = relh, 
                                         Pa = (pres/1000)),
                atten_dist12 = aud_range(f = 12000, # sound distance travelled based on attenuation at 12khz
                                         T_cel = temp, 
                                         h_rel = relh, 
                                         Pa = (pres/1000)))

# Check correlations among weather variables
arid_comp = aw4 %>% dplyr::select(temp,relh,dew,
                                  evap_wind,evap_1,ew_vol,e1_vol)
cor(arid_comp)

# Check historgrams of evaporation rate
hist(aw4$evap_wind)
hist(aw4$evap_1)
hist(aw4$ew_vol)
hist(aw4$e1_vol)

# Refactor site so it is ordered as lwma, sswma, cbma, and kiowa
aw4$site = factor(aw4$site, levels = c("lwma","sswma","cbma","kiowa"))

# Audio Variable PCAs - Create principal componenets based on acoustic metrics
audio_pca = prcomp(aw4[,c("aci","bio","adi","aei","num_vocals","species_diversity")], 
                   center = TRUE, # variables shifted to be zero centered
                   scale. = TRUE) # variables are scaled to have unit variance

# Plot PCs to see which acoustic metrics are grouped together and what direction they are going in
summary(audio_pca) #PC1 and PC2 have highest proportion of variance

# Plot PC1 against PC2
ggbiplot(audio_pca, choices = c(1,2),
         ellipse = TRUE, 
         alpha = 0, 
         groups = aw4$site)

# Plot PC1 against PC3
ggbiplot(audio_pca, 
         choices = c(1,3),
         ellipse = TRUE, 
         alpha = 0, 
         groups = aw4$site) # Plot PCs

# Create gt table of pcs table for the manuscript
pc_df = data.frame(pc = c(1,2,3),
                   std = c(1.413,1.273,1.008),
                   prop_var = c(0.333, 0.27, 0.165),
                   cum_prop = c(0.333, 0.603, 0.772),
                   desc = c("Acoustic Diversity", "Avian Abundance", "Acoustic Complexity"))

pc_gt = pc_df %>%
  gt() %>%
  cols_align('center') %>%
  cols_label(pc = md("**PC**"),
             std = md("**Standard Deviation**"),
             prop_var = md("**Proportion\nof Variance**"),
             cum_prop = md("**Cumulative\nProportion**"),
             desc = md("**Description**")) %>%
  opt_table_font(
    font = "Times New Roman")%>% gtsave("results/pc_table.png",
                                        vwidth = 1100,
                                        vheight = 1500,
                                        expand = 10)

### PC1: ADI and AEI, higher values mean higher diversity (after running line 65)
### PC2: Num Vocals and Species Diversity, higher values mean higher avian abundance
### PC3: ACI and BIO, higher values = higher ACI
audio_pcadf = as.data.frame(audio_pca[["x"]]) # extract pc scores from pca summary

aw4$pc1 = audio_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
aw4$pc2 = audio_pcadf$PC2 
aw4$pc3 = audio_pcadf$PC3

# check to see if pc scores correlate with weather data and acoustic metrics
pc_df = aw4 %>% 
  dplyr::select(aci:species_diversity,
                temp:dew,
                evap_wind:ew_vol,
                pc1:pc3)
cor(pc_df)

# Linear Models - data pooled together (data not designated by sit --------
# Linear mixed model (LMM) - pc2 (avian abundance) against interaction between evaporation rate (ew_vol) and mas_bin, with site as random effect
m1_lmm = lmer(pc2 ~ ew_vol*mas_bin + scale(date) + (1|site), data = aw4)
summary(m1_lmm)
emm1_lmm = emtrends(m1_lmm, ~ mas_bin,
                    var = "ew_vol",
                    type = "response",
                    weights = "cells")

# Creating labels to make only relevant comparisons in post-hoc test
mas0 = c(1,0,0,0)
mas1 = c(0,1,0,0)
mas2 = c(0,0,1,0)
mas3 = c(0,0,0,1)
contrast_lmm1 = summary(contrast(emm1_lmm, 
                            method = list("Early-Predawn" = mas1-mas0,
                                          "Mid-Predawn" = mas2-mas0,
                                          "Late-Predawn" = mas3-mas0,
                                          "Mid-Early" = mas2-mas1,
                                          "Late-Early" = mas3-mas1,
                                          "Late-Mid" = mas3-mas2),
                            adjust = "bonferroni")); contrast

# Linear model - pc2 (avian abundance) against interaction between evaporation rate (ew_vol) and mas_bin
m1_lm = lm(pc2 ~ ew_vol*mas_bin + scale(date), data = aw4)
summary(m1_lm)
emm1_lm = emtrends(m1_lm, ~ mas_bin, 
               var = "ew_vol", 
               type = "response",
               weights = "cells") 
mas0 = c(1,0,0,0)
mas1 = c(0,1,0,0)
mas2 = c(0,0,1,0)
mas3 = c(0,0,0,1)
contrast_lm1 = summary(contrast(emm1_lm, 
                            method = list("Early-Predawn" = mas1-mas0,
                                          "Mid-Predawn" = mas2-mas0,
                                          "Late-Predawn" = mas3-mas0,
                                          "Mid-Early" = mas2-mas1,
                                          "Late-Early" = mas3-mas1,
                                          "Late-Mid" = mas3-mas2),
                            adjust = "bonferroni"));contrast_lm1

# Plot PC2 (Avian Abundance) against evaporation rate (ew_vol) ------------

ggplot(data = aw4, aes(x = ew_vol,
                       y = pc2)) +
  geom_smooth(method = lm) +
  facet_grid(~mas_bin)


# Plotting PC2 (Avian Abundance) against evaporation rate (ew_vol) --------

ggplot(data = aw4, aes(x = ew_vol,
                       y = pc2,
                       color)) +
  geom_smooth(method = lm) +
  facet_grid(~mas_bin)

# Aridity Gradient - Summarized by Date and MAS ---------------------------
# summarizing full dataset by site, date, and mas bin to reduce data variation
aw6 = aw4 %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE), n = n()) 

arid_comp = aw6 %>% dplyr::select(temp,relh,gh,evap_wind:atten_dist12)
cor(arid_comp[,c(-1,-2)])

# Creating MAS bin labels for graphs
aw6$mas_labels = factor(aw6$mas_bin, levels = c("0","1","2","3"),
                        labels = c("Predawn","Early","Mid","Late"))

# Creating site labels for graphs
aw6$site_labels = factor(aw6$site, levels = c("lwma","sswma","cbma","kiowa"),
                         labels = c("LWMA","SSWMA","CBMA","KIOWA"))

save(aw6, file = "data_clean/aridity_gradient_mas.Rdata")

# Aridity Gradient - Summarized by Date and MAS - Linear Models
### Evaporation rate (mL/cm2/day) is treated as a continuous variable rather than a factor to reduce complexity

### PC 1 - Acoustic Diversity
## LM for PC1 - Acoustic Diversity across sites, within time periods
source("src/aridity_gradient_functions/aridity_gradient_mas_functions.R")
lmpc1site = ag_contrasts_convar_site(aw6,
                                     aw6$pc1,
                                     aw6$ew_vol);lmpc1site

## PC1 - Acoustic Diversity across sound attenuation coefficient
# 4 kHz
atten04pc1 = ag_contrasts_convar_site(aw6,
                                      aw6$pc1,
                                      aw6$atten_alpha04)

# 8 kHz
atten08pc1 = ag_contrasts_convar_site(aw6,
                                      aw6$pc1,
                                      aw6$atten_alpha08)

# 12 kHz
atten12pc1 = ag_contrasts_convar_site(aw6,
                                      aw6$pc1,
                                      aw6$atten_alpha12)

## PC1 plotted against aridity (gh), facet grid by mas_bin (comparisons across site, within time)
xlab = expression(paste("Water Evaporation Rate (mL/cm"^"2","/day)"))

ag_graph_site_paper(aw6$pc1, 
                    aw6$ew_vol,
                    "PC1 - Acoustic Diversity",
                    xlab)
ggsave('results/arid_grad_pc1_site_paper.png', dpi = 600, height = 6, width = 8, units = "in")

### LM for PC1 - Acoustic Diversity, across time periods, within sites
lmpc1time = ag_contrasts_convar_time(aw6,
                                     aw6$pc1,
                                     aw6$ew_vol);lmpc1time

## PC1 plotted against aridity (gh), facet grid by site (comparisons across time, within site)
ag_graph_time_paper(aw6$pc1, 
                    aw6$ew_vol,
                    "PC1 - Acoustic Diversity",
                    xlab)
ggsave('results/arid_grad_pc1_time_paper.png', dpi = 600, height = 6, width = 8, units = "in")

### PC2 - Avian Abundance
## LMs for PC2 with aridity (gh) as the independent variable
lmpc2site = ag_contrasts_convar_site(aw6,
                                     aw6$pc2,
                                     aw6$ew_vol);lmpc2site

ag_graph_site_paper(aw6$pc2, 
                    aw6$ew_vol,
                    "PC2 - Avian Abundance",
                    xlab)
ggsave('results/arid_grad_pc2_site_paper.png', dpi = 600, height = 6, width = 8, units = "in")


## LM for PC2 - Avian Abundance, across time periods, within site
lmpc2time = ag_contrasts_convar_time(aw6,
                                     aw6$pc2,
                                     aw6$ew_vol);lmpc2time

ag_graph_time_paper(aw6$pc2, 
                    aw6$ew_vol,
                    "PC2 - Avian Abundance",
                    xlab)
ggsave('results/arid_grad_pc2_time_paper.png', 
       dpi = 600, 
       height = 6, width = 10, units = "in")

## PC2 - Avian Abundance across sound attenuation coefficient
# 4 kHz
atten04pc2 = ag_contrasts_convar_site(aw6,
                                      aw6$pc2,
                                      aw6$atten_alpha04)

# 8 kHz
atten08pc2 = ag_contrasts_convar_site(aw6,
                                      aw6$pc2,
                                      aw6$atten_alpha08)

# 12 kHz
atten12pc2 = ag_contrasts_convar_site(aw6,
                                      aw6$pc2,
                                      aw6$atten_alpha12)

# Plotting sound attenuation coefficient as the continuous, independent variable
atten_graph_time_paper(aw6$pc2, 
                       aw6$atten_alpha08, 
                       "PC2 - Avian Abundance", 
                       "Sound Attenuation Coefficient (alpha)")
ggsave('results/sound_atten8khz_pc2_time_paper.png', dpi = 600, height = 8, width = 6, units = "in")

# Plotting sound attenuation coefficient as the continuous, independent variable

atten_graph_time_paper(aw6$pc2, 
                       aw6$atten_dist08, 
                       "PC2 - Avian Abundance", 
                       "Sound Attenuation distance at 8kHz (m)")
ggsave('results/sound_atten8khz_distance_pc2_time_paper.png', dpi = 600, height = 8, width = 6, units = "in")

### PC3 - Acoustic Complexity
## LMs for PC3 across sites, within mas_bin
lmpc3site = ag_contrasts_convar_site(aw6,
                                     aw6$pc3,
                                     aw6$ew_vol);lmpc3site


ag_graph_site_paper(aw6$pc3, 
                    aw6$ew_vol,
                    "PC3 - Acoustic Complexity",
                    xlab)
ggsave('results/arid_grad_pc3_site_paper.png', dpi = 600, height = 6, width = 8, units = "in")

### LM for PC3 - Acoustic Complexity, Across time periods, within sites
lmpc3time = ag_contrasts_convar_time(aw6,
                                     aw6$pc3,
                                     aw6$ew_vol);lmpc3time

ag_graph_time_paper(aw6$pc3, 
                    aw6$ew_vol,
                    "PC3 - Acoustic Complexity",
                    xlab)
ggsave('results/arid_grad_pc3_time_paper.png', dpi = 600, height = 6, width = 8, units = "in")

## PC3 - Acoustic complexity across sound attenuation coefficient
# 4 kHz
atten04pc3 = ag_contrasts_convar_site(aw6,
                                      aw6$pc3,
                                      aw6$atten_alpha04)

# 8 kHz
atten08pc3 = ag_contrasts_convar_site(aw6,
                                      aw6$pc3,
                                      aw6$atten_alpha08)

# 12 kHz
atten12pc3 = ag_contrasts_convar_site(aw6,
                                      aw6$pc3,
                                      aw6$atten_alpha12)

### Sound attenuation at 4kHz tables
atten04_tables = ag_slopes_table(atten04pc1[[6]],
                                 atten04pc2[[6]],
                                 atten04pc3[[6]]) %>% gtsave("results/atten04_slopes.png", 
                                                             vwidth = 1100,
                                                             # vheight = 15000, 
                                                             expand = 1000)

### Sound attenuation at 8kHz tables
atten08_tables = ag_slopes_table(atten08pc1[[6]],
                                 atten08pc2[[6]],
                                 atten08pc3[[6]]) %>% gtsave("results/atten08_slopes.png", 
                                                             vwidth = 1100,
                                                             # vheight = 15000, 
                                                             expand = 1000)

### Sound attenuation at 8kHz tables
atten12_tables = ag_slopes_table(atten12pc1[[6]],
                                 atten12pc2[[6]],
                                 atten12pc3[[6]]) %>% gtsave("results/atten12_slopes.png", 
                                                             vwidth = 1100,
                                                             # vheight = 15000, 
                                                             expand = 1000)

### Big table for all aridity gradient pcs slopes
pc_tables = ag_slopes_table(lmpc1site[[6]],
                            lmpc2site[[6]],
                            lmpc3site[[6]]); pc_tables

pc_tables %>% gtsave("results/ag_all_pcs_slopes.png", 
                     vwidth = 1100,
                     # vheight = 15000, 
                     expand = 1000)

pc_tables_contrasts_site = ag_contrasts_table_site(tab1 = lmpc1site[[3]],
                                                   tab2 = lmpc2site[[3]],
                                                   tab3 = lmpc3site[[3]]);pc_tables_contrasts_site
pc_tables_contrasts_site %>% gtsave("results/ag_all_pcs_contrasts_site.png", 
                                    vwidth = 1100,
                                    # vheight = 15000, 
                                    expand = 1000)

pc_tables_contrasts_time = ag_contrasts_table_time(tab1 = lmpc1time[[3]],
                                                   tab2 = lmpc2time[[3]],
                                                   tab3 = lmpc3time[[3]]);pc_tables_contrasts_time
pc_tables_contrasts_time %>% gtsave("results/ag_all_pcs_contrasts_time.png", 
                                    vwidth = 1100,
                                    # vheight = 15000, 
                                    expand = 1000)

