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


# Loading MAS and Date Aridity Gradient Data ------------------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
load("data_clean/aridity_gradient_mas.Rdata")

extreme_arid_n = aw6 %>%
  arrange(desc(gh)) %>%
  group_by(site) %>%
  dplyr::summarise(n = n(),
         top5per = 0.05*n())

extreme_arid = aw6 %>%
  # dplyr::mutate(gh_within = scale_this(gh)) %>%
  group_by(site) %>%
  arrange(desc(gh)) %>%
  slice_max(gh,n = 17)


# Statistical Analysis ----------------------------------------------------

ggplot(data = extreme_arid, aes(x = gh, y = pc1, color = site)) +
  geom_point() +
  geom_smooth()
# we do get threshold for cbma but not the other sites
       
m1ex = lm(pc1 ~ arid_within*mas_bin + scale(date), data = extreme_arid)
summary(m1ex)  

m2ex = lm(pc2 ~ arid_within*mas_bin + scale(date), data = extreme_arid)
summary(m2ex)  

m3ex = lm(pc3 ~ arid_within*mas_bin + scale(date), data = extreme_arid)
summary(m3ex)  
