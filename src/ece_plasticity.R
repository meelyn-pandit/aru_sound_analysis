########################################
##### ECE - Plasticity #################
### Determining if responses to evaporation rate (ml/cm2/day) correlate with min, max, climate extreme, and impact extreme levels of evaporation rate
library(rptR)
# Climate ECE - Loading MAS and Date Aridity Gradient Data ------------------------------
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis")
# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
load("data_clean/aridity_data_clean.Rdata")

ece_df = aw4 %>%
  dplyr::group_by(site,date,mas_bin) %>%
  dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), 
                          list(mean = mean,
                               min = min,
                               max = max)),
                   n = n())

# Evaporation Rate Dataframe Subsetting -----------------------------------------
### Finding top 5% values based on climate definition
exa_lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>% # 12006 obs
  # mutate(gh_within = scale_this(gh)) %>%
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 601) %>%
  # group_by(date,mas_bin) %>%
  # dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean)) %>%
  dplyr::mutate(site = "lwma")
min(exa_lwma$ew_vol) # min value is 0.6889226

ag_lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>%
  dplyr::mutate(react_norm = if_else(ew_vol > 0.6889226, 
                                     "extreme",
                                     "normal"))
tally(ag_lwma %>% dplyr::filter(react_norm == "extreme"))
  
exa_sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>% # 12461 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 623) %>%
  # dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean))%>%
  dplyr::mutate(site = "sswma")
min(exa_sswma$ew_vol) # min value is 0.8365307

ag_sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>%
  dplyr::mutate(react_norm = if_else(ew_vol > 0.8365307, 
                                     "extreme",
                                     "normal"))
tally(ag_sswma %>% dplyr::filter(react_norm == "extreme"))

exa_cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>% # 13158 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 658) %>%
  # dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean))%>%
  dplyr::mutate(site = "cbma")
min(exa_cbma$ew_vol) # min value is 1.165413

ag_cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>%
  dplyr::mutate(react_norm = if_else(ew_vol > 1.165413, 
                                     "extreme",
                                     "normal"))
tally(ag_cbma %>% dplyr::filter(react_norm == "extreme"))

exa_kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>% # 16325 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 816)  %>%
  # dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean)) %>%
  dplyr::mutate(site = "kiowa")
min(exa_kiowa$ew_vol) # min value is 1.141292

ag_kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>%
  dplyr::mutate(react_norm = if_else(ew_vol > 1.141292, 
                                     "extreme",
                                     "normal"))
tally(ag_kiowa %>% dplyr::filter(react_norm == "extreme"))

# Dataframe that separates data based on climate definition of ece
climate_arid = rbind(ag_lwma, ag_sswma, ag_cbma, ag_kiowa) %>%
  dplyr::mutate(react_climate = as.numeric(factor(react_norm, 
                                       levels = c("normal", "extreme")),
                                       labels = c(0,1)))

# ex_arid_n = ex_arid %>%
#   dplyr::select(site, ew_vol) %>%
#   group_by(site) %>%
#   dplyr::summarise(min_evap = min(ew_vol),
#                    max_evap = max(ew_vol))


# Combine summary df and climate ece df -----------------------------------
### This would be looking a mean extreme aridity based on climate definition
# ece_df2 = left_join(ece_df,ex_arid, by = c("site"))


# Repeatability estimates using rptR --------------------------------------

rpt1 = rpt(pc1 ~ react_norm + scale(date) + (1|mas_bin) + (1|site), 
           grname = c("site","mas_bin", "Fixed"),
           data = climate_arid,
           datatype = "Gaussian",
           nboot = 0,
           parallel = TRUE,
           update = FALSE,
           ncores = (detectCores()-4))
summary(rpt1)

plot(rpt1, 
     grname = "Fixed")

rpt2 = rpt(pc2 ~ react_norm + (1|site) + (1|mas_bin), grname = c("site","mas_bin", "Fixed"),
           data = climate_arid,
           datatype = "Gaussian",
           parallel = TRUE,
           nboot = 0,
           # npermut = 100,
           update = TRUE,
           ncores = (detectCores()-4))
summary(rpt2)
plot(rpt2, 
     grname = "Fixed")


# Aridity Gradient - Climate Definition - LMMs ----------------------------


ece_lmm1 = lmer(pc1 ~ react_climate*site + (1|site), data = climate_arid)
summary(ece_lmm1)
broom.mixed::tidy(ece_lmm1, 
                  effects = "ran_coefs", 
                  conf.int = TRUE) %>% print(n = 100) #fixed + random effects
broom.mixed::tidy(ece_lmm1, 
                  effects = "fixed", 
                  conf.int = TRUE) %>% print(n = 100) #fixed effects
broom.mixed::tidy(ece_lmm1, 
                  effects = "ran_vals", 
                  conf.int = TRUE) %>% print(n = 100)# random effects intercepts and slopes
broom.mixed::tidy(ece_lmm1, 
                  effects = "ran_pars", 
                  conf.int = TRUE) %>% print(n = 100)
emm = emtrends(ece_lmm1, ~ site, 
               var = "react_climate", 
               type = 'response',
               weights = "cells") # across sites
summary(emm)

# PC2
ece_lmm2 = lmer(pc2 ~ react_climate*site + (1|site), data = climate_arid)
summary(ece_lmm2)
emm2 = emtrends(ece_lmm2, ~ site, 
               var = "react_climate", 
               type = 'response',
               weights = "cells") # across sites
summary(emm2)

#PC3
ece_lmm3 = lmer(pc3 ~ react_climate*site + (1|site), data = climate_arid)
summary(ece_lmm3)
emm3 = emtrends(ece_lmm3, ~ site, 
                var = "react_climate", 
                type = 'response',
                weights = "cells") # across sites
summary(emm3)
