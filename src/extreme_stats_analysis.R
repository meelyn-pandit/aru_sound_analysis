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

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
load("data_clean/aridity_data_clean.Rdata")

extreme_arid_n = aw4 %>%
  arrange(desc(gh)) %>%
  group_by(site) %>%
  dplyr::summarise(n = n(),
         top5per = 0.05*n())

exa_lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>%
  arrange(desc(gh_within)) %>%
  slice_max(gh_within,n = 601)

exa_sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>%
  arrange(desc(gh_within)) %>%
  slice_max(gh_within,n = 620)

exa_cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>%
  arrange(desc(gh_within)) %>%
  slice_max(gh_within,n = 662)

exa_kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>%
  arrange(desc(gh_within)) %>%
  slice_max(gh_within,n = 816)

extreme_arid = rbind(exa_lwma, exa_sswma, exa_cbma, exa_kiowa)
# Climate ECE - Simple Plots ------------------------------------------------------------
# Full Dataset
ggplot(data = aw4, aes(x = gh, y = pc3, color = site)) +
  # geom_point() +
  geom_smooth(method = loess)
# we do get threshold for cbma but not the other sites

# MAS Summarized data
ggplot(data = ea_mas, aes(x = gh_within, y = pc3, color = site)) +
  geom_point() +
  geom_smooth(method = lm)


# Climate ECE - Statistical Analysis ----------------------------------------------------

m1ex = lmer(pc1 ~ gh_within+site + mas_bin + scale(date) + (1|aru), data = extreme_arid)
summary(m1ex)  
assump(m1ex)
emmeans(m1ex, pairwise ~ site)

m2ex = lmer(pc2 ~ arid_within*site + mas_bin + scale(date) + (1|aru), data = extreme_arid)
summary(m2ex)  
assump(m2ex)
emmeans(m2ex, pairwise ~ site)


m3ex = lm(pc3 ~ arid_within*mas_bin + scale(date), data = extreme_arid)
summary(m3ex)  

# Climate ECE - Summarizing data into MAS and Date --------------------------------------

ea_mas = extreme_arid %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  mutate_at(c("arid_within", "arid_across", "hist_within", "hist_across"), as.numeric) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity, 
                           temp:dew, 
                           gh, 
                           gh_within,
                           arid_within, 
                           hist_within:arid_across,
                           sound_atten04:sound_atten12,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) %>%
  mutate_at(c("arid_within",
              "arid_across",
              "hist_within",
              "hist_across"), round_factor) 



# Climate ECE - MAS and Date ----------------------------------------------
ex1mas = lm(pc1 ~ arid_within*site + mas_bin + scale(date), data = ea_mas)
summary(ex1mas)  
assump(ex1mas)
emmeans(ex1mas, pairwise ~ site)

# Threshold - ECE - Threshold Modelling -----------------------------------------------------
# Reference: https://www.r-bloggers.com/2021/04/other-useful-functions-for-nonlinear-regression-threshold-models-and-all-that/
library(sandwich)
library(lmtest)
library(drcSeedGerm)

# Threshold - ECE - Hyperbolic Model --------------------------------------------------------

thres1 <- drm(ea_mas$pc1 ~ ea_mas$gh_within*ea_mas$site, fct = GRT.YL())
summary(thres1)
plot(thres1, log="", 
     legendPos = c(5, 1.0), xlab = "Aridity")

# Threshold - ECE - Exponential switch-off model --------------------------------------------

modExb <- drm(ea_mas$pc1 ~ ea_mas$gh_within*ea_mas$site, fct = GRT.Exb())
summary(modExb)
plot(modExb, log="", 
     legendPos = c(5, 1.0), xlab = "Temperature (°C)")


# Threshold - ECE - Broken-curvilinear model ------------------------------------------------

modExb <- drm(ea_mas$pc1 ~ ea_mas$gh_within*ea_mas$site, fct = GRT.RFb())
summary(modExb)
plot(modExb, log="", 
     legendPos = c(5, 1.0), xlab = "Temperature (°C)")



# Threshold - ECE - Piecewise Regression ----------------------------------------------------

library(segmented)
library(nlme)

# Full Dataset - Site affects slope, aridity (z) affects changepoint
## U = random effects in the slope-difference parameter
## G0 = random effects in the breakpoints
m1 = lme(pc1 ~ gh, random = ~1|site, data = extreme_arid %>% 
           dplyr::filter(site == "lwma"))
m1s = segmented.lme(m1, ~gh,
                    random = list(site=pdDiag(~1+gh+U+G0)),
                    control = seg.control(n.boot=10), display = TRUE)
summary(m1s) # does not give breakpoints for each individual site
slope(m1s)


fit = lm(pc1 ~ gh, data = extreme_arid %>% dplyr::filter(site == "lwma"))
segmented.fit = segmented(fit, seg.Z = ~gh, psi = -9)
summary(segmented.fit)


# MAS
m1 = lm(pc1 ~ gh, data = ea_mas)
m1s = segmented(m1, seg.Z=~gh)
davies.test(m1, seg.Z = gh*site)
summary(m1s) # does not give breakpoints for each individual site
slope(m1s)

# General Additive Model --------------------------------------------------

library(mgcv)
gam1 = gam(pc1 ~ gh*site + s(site, bs = "re")
                    + s(gh, site, bs = "re"), data = extreme_arid)
summary(gam1)
coef(gam1)
plot(gam1)
emm(gam1, ~gh*site)
