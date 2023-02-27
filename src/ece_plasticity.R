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
  dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), list(mean = mean,
                                       min = min,
                                       max = max)),
                   n = n())

# Evaporation Rate Dataframe Subsetting -----------------------------------------

exa_lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>% # 12006 obs
  # mutate(gh_within = scale_this(gh)) %>%
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 601) %>%
  # group_by(date,mas_bin) %>%
  dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean)) %>%
  dplyr::mutate(site = "lwma")
  
exa_sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>% # 12461 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 623) %>%
  dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean))%>%
  dplyr::mutate(site = "sswma")

exa_cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>% # 13158 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 658) %>%
  dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean))%>%
  dplyr::mutate(site = "cbma")


exa_kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>% # 16325 obs
  arrange(desc(ew_vol)) %>%
  slice_max(ew_vol,n = 816)  %>%
  dplyr::summarise(across(c(ew_vol,pc1,pc2,pc3), mean)) %>%
  dplyr::mutate(site = "kiowa")


ex_arid = rbind(exa_lwma, exa_sswma, exa_cbma, exa_kiowa)

# ex_arid_n = ex_arid %>%
#   dplyr::select(site, ew_vol) %>%
#   group_by(site) %>%
#   dplyr::summarise(min_evap = min(ew_vol),
#                    max_evap = max(ew_vol))


# Combine summary df and climate ece df -----------------------------------
### This would be looking a mean extreme aridity based on climate definition
ece_df2 = left_join(ece_df,ex_arid, by = c("site"))


# Repeatability estimates using rptR --------------------------------------

rpt1 = rpt(pc1 ~ ew_vol + (1|site) + (1|mas_bin), grname = c("site","mas_bin"),
           data = aw4,
           datatype = "Gaussian",
           parallel = TRUE,
           ncores = (detectCores()-4))
summary(rpt1)

rpt2 = rpt(pc2 ~ ew_vol + (1|site) + (1|mas_bin), grname = c("site","mas_bin"),
           data = aw4,
           datatype = "Gaussian",
           parallel = TRUE,
           ncores = (detectCores()-4))
summary(rpt2)
