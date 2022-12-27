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
  dplyr::filter(site == "lwma") %>%
  # mutate(gh_within = scale_this(gh)) %>%
  arrange(desc(arid_within)) %>%
  slice_max(arid_within,n = 601)

exa_sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>%
  arrange(desc(arid_within)) %>%
  slice_max(arid_within,n = 620)

exa_cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>%
  arrange(desc(arid_within)) %>%
  slice_max(arid_within,n = 662)

exa_kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>%
  arrange(desc(arid_within)) %>%
  slice_max(arid_within,n = 816) # min gh_within = 

extreme_aridwithin = rbind(exa_lwma, exa_sswma, exa_cbma, exa_kiowa)

ea_aridwithin = extreme_aridwithin %>%
  dplyr::select(site, gh, gh_within,arid_within) %>%
  group_by(site) %>%
  dplyr::summarise(min_aw = min(arid_within),
                   max_aw = max(arid_within),
                   min_gw = min(gh_within),
                   max_gw = max(gh_within),
                   min_gh = min(gh),
                   max_gh = max(gh))
# lwma  gh_within min = 1.74, max = 2.66
# sswma gh_within min = 1.73, max = 3.04
# cbma  gh_within min = 1.74, max = 2.72
# kiowa gh_within min = 1.88, max = 2.56


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

m1ex = lmer(pc1 ~ site + (1|site), data = extreme_aridwithin)
summary(m1ex)  
assump(m1ex)
emmeans(m1ex, pairwise ~ site)

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
# ex1mas = lm(pc1 ~ site + scale(date), data = exaw_mas)
# summary(ex1mas)  
# assump(ex1mas)
# emmeans(ex1mas, pairwise ~ site)

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
gam1 = gam(pc1 ~ s(arid_within, bs = "cs", k = -1), data = aw4)
summary(gam1)
gam1$smooth[[1]]$xp #0.95
coef(gam1)
plot(gam1, se=TRUE,col="blue")
emm(gam1, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))

gam2 = gam(pc2 ~ s(arid_within, bs = "cs", k = -1), data = aw4)
summary(gam2)
gam2$smooth[[1]]$xp #0.95
coef(gam2)
plot(gam2, se=TRUE,col="blue")
emm(gam2, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))

gam3 = gam(pc3 ~ s(arid_within, bs = "cs", k = -1), data = aw4)
summary(gam3)
gam3$smooth[[1]]$xp #0.98 for arid_across, 0.95 for arid_within
coef(gam3)
plot(gam3, se=TRUE,col="blue")
emm(gam3, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))
ggplot(data = aw4, aes(x = arid_within, y = pc2)) +
  geom_smooth()
# Reference: https://www.r-bloggers.com/2021/04/other-useful-functions-for-nonlinear-regression-threshold-models-and-all-that/


# ECE - Threshold - MCP LMM Piecewise -------------------------------------
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

# ECE - Threshold - Impact Definition Data --------------------------------

awthres = aw4 %>%
  dplyr::filter(arid_within >0.95) %>%
  # dplyr::filter(year(date_time)==2021) %>%
  # dplyr::filter(as_date(date_time) < "2021-08-16") %>%
#   mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
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
#   mutate_at(c("arid_withinf",
#               "arid_acrossf",
#               "hist_withinf",
#               "hist_acrossf"), round_factor) 

awthres_n = awthres %>%
  group_by(site) %>%
  dplyr::summarise(total = n(),
                   percent = n()/length(awthres))

exthresm1 = lm(pc1 ~ site + scale(date), data = awthres)
summary(exthresm1)
emmeans(exthresm1, pairwise ~ site)

ecethres_pc1_mas = ece_contrast_mas(awthres, 
                                    awthres$pc1)
write.csv(ecethres_pc1_mas[[5]], 
          'results/ece_threshold_pc1_mas.csv', 
          row.names = FALSE)

ecethres_pc1_mas[[5]] %>% gtsave('ece_threshold_pc1_mas.png', expand = 100)

ecethres_pc2_mas = ece_contrast_mas(awthres, 
                                    awthres$pc2)
write.csv(ecethres_pc2_mas[[5]], 
          'results/ece_threshold_pc2_mas.csv', 
          row.names = FALSE)

ecethres_pc2_mas[[5]] %>% gtsave('ece_threshold_pc2_mas.png', expand = 100)

ecethres_pc3_mas = ece_contrast_mas(awthres, 
                                    awthres$pc3)
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


# ECE - Climate and Threshold Tables Together -----------------------------

# All PCs for Climate and Impact ECE in One Table 
ece_pc_table = ece_tables_combined2(ece_pc1_mas[[5]],
                    ecethres_pc1_mas[[5]],
                    ece_pc2_mas[[5]],
                    ecethres_pc2_mas[[5]],
                    ece_pc3_mas[[5]],
                    ecethres_pc3_mas[[5]]
                    ); ece_pc_table
ece_pc_table %>% gtsave("results/ece_all_pcs.png", vwidth = 2000, vheight = 1500, expand = 100)



# ECE - Threshold - Multi-piecewise linear regression ---------------------

### Estimating continuous piecewise linear regression ###
# https://www.r-bloggers.com/2013/04/estimating-continuous-piecewise-linear-regression/
# N <- 325 # number of sampled points, lwma = 325, sswma = 354, cbma = 368, kiowa = 367

# Fixing Within-normalized aridity
aw4 = aw4 %>%
  group_by(site) %>%
  dplyr::mutate(gh_within = scale_this(gh))

# checking it against separating sites and scaling aridity 

aw4lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4 = rbind(aw4lwma,aw4sswma,aw4cbma,aw4kiowa)

# Checking to see if they scaled within aridity is different or not
arid_df = aw4 %>%
  dplyr::select(site,gh,gh_within, gh_within2) %>%
  arrange(gh_within,site)
# gh_wihtin and gh_within2 are the same so within scaling seems to have worked

# plotting to see changepoints - full dataset

ggplot(data = aw4, aes(x = gh_within,
                       y = pc1,
                       # color = site
                       )
       )+
  geom_smooth(method = loess, se = FALSE) 

# plotting to see if where changepoints are - MAS dataset

ggplot(data = aw6, aes(x = gh_within,
                       y = pc2,
                       # color = site
                      )
      )+
  geom_smooth(method = loess, se = TRUE) 
# aw4site = aw4 %>% dplyr::filter(site == "kiowa") %>% dplyr::select(gh_within, pc2)
# x = aw4site$gh_within
# y = aw4site$pc2

# x <- seq(-1, 1, len = N)
# y <- f(x) + rnorm(length(x))

knot_model = function(num_knots,
                      x_var,
                      y_var) {
  
  # Setting up piecewise regression with multiple breaks or knots
  
  piece.formula <- function(var.name, knots) {
    formula.sign <- rep(" - ", length(knots))
    formula.sign[knots < 0] <- " + "
    paste(var.name, "+",
          paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
                collapse = " + ", sep=""))
  }
  
  f <- function(x) {
    2 * sin(6 * x)
  }
  
  x = x_var
  y = y_var
  
  set.seed(1)
  K <- num_knots  # number of knots
  knots <- seq(min(x), max(x), len = K + 2)[-c(1, K + 2)]
  model <- lm(formula(paste("y ~", piece.formula("x", knots))))
  
  par(mar = c(4, 4, 1, 1))
  plot(x, y)
  
  # ggplot(data = aw4, aes(gh_within,pc2))+
  #   geom_smooth(method = loess, se = FALSE)
  
  lines(x, f(x))
  new.x <- seq(min(x), max(x) ,len = 10000)
  points(new.x, predict(model, newdata = data.frame(x = new.x)),
         col = "red", pch = ".")
  points(knots, predict(model, newdata = data.frame(x = knots)),
         col = "red", pch = 18)
  summary(model) # for kiowa at least, threshold for gh_wihtin is 2.26
  return(model)
}


# Checking which piecewise regression model has best fit with AIC
knotms = lapply(c(1:10), FUN = function(x) knot_model(num_knots = x,
                              x_var = aw4$gh_within, 
                              y_var = aw4$pc3))

AICctab(knotms[[1]],knotms[[2]],knotms[[3]],knotms[[4]],knotms[[5]],
        knotms[[6]],knotms[[7]],knotms[[8]],knotms[[9]],knotms[[10]],
        nobs = 1411, base=T, weights=T, delta=T, logLik=T)

summary(knotms[[1]])


# ECE - Threshold - Loess Regression --------------------------------------

ggplot(data = aw4, aes(x = gh_within,
                       y = pc2,
                       # color = site
                       ))+
geom_smooth(method = loess, se = FALSE) +
  geom_vline(xintercept = 0.5520, color = "red")
  geom_vline(xintercept = -1.162, color = "red") +
  geom_vline(xintercept = -0.057, color = "blue") +
  geom_vline(xintercept = 1.166, color = 'green')
  
  abline(v = -0.057, col = "blue")
abline(v = 1.166, col = "green")

loess_m10 = loess(pc2 ~ gh_within, data = aw4, span = 0.10)
loess_m25 = loess(pc2 ~ gh_within, data = aw4, span = 0.25)
loess_m50 = loess(pc2 ~ gh_within, data = aw4, span = 0.50)
summary(loess_m50)

# Get smooth output
smoothed10 <- predict(loess_m10) 
smoothed25 <- predict(loess_m25) 
smoothed50 <- predict(loess_m50) 

plot(y = aw4$pc2, aw4$gh_within, type = "p", main = "Loess Smoothing and Prediction")
lines(smoothed10, x=aw4$gh_within, col="red")
lines(smoothed25, x=aw4$gh_within, col="green")
lines(smoothed50, x=aw4$gh_within, col="blue")

# find changepoints in loess fit for full dataset, but manually segmenting data to where there is a dropoff in pc
# pc1: 1-2
# pc2: 1-2
# pc3: 1-2

loess_max = function(data,
                     x_var,
                     y_var,
                     arid_min,
                     arid_max) {
  # loess_m = loess(y_var ~ gh_within, data = aw4)
  
  # segmenting data from gh_within 1 to 2 since that is where we see the drop in the loess model
  data_filtered = data %>% 
    dplyr::filter(gh_within >= arid_min) %>% 
    dplyr::filter(gh_within <= arid_max)
  loess_mex = loess(y_var ~ x_var, data = data_filtered)
  
  x <- x_var[x_var >= 1]
  x = x[x <= 2]
  # x = seq(-2,3, by = 0.1)
  px <- predict(loess_mex, newdata=x)
  px1 <- diff(px)
  # px1_ex = px1[px1 >= 1] 
  
  max_gh = which.max(px1) # max is 4 but start value of x is -2, means the curve is flat at position -2+4 = 2
  px1[max_gh] # 0.01929 value with max
  loess_predict = cbind(x = x[-1], y = px1)
  max_arid = loess_predict[max_gh,] # max x is 1.4769
  
  par(mfrow=c(1, 2))
  plot(x, px, main="loess model")
  abline(v=max_arid, col="red")
  
  # loess_predict[7488,] # x = 1.136
  
  plot(x[-1], px1, main="diff(loess model)")
  abline(v=max_arid, col="red")
  return(max_arid)
}

loess_max(aw4, aw4$gh_within, aw4$pc1,-1,1.5) # 1
loess_max(aw4, aw4$gh_within, aw4$pc2,1,2) # 1.136
loess_max(aw4, aw4$gh_within, aw4$pc3,1,2) # 1.597

# Piecewise Linear Regression model on loess predicted data

loess_m = loess(pc1 ~ gh_within, data = aw4)
set.seed(124)
# x = rnorm(1000, 0,1.2)
x = seq(-2,3, by = 0.1)
px <- predict(loess_m, newdata=x)

loess_predict = data.frame(x = x,
                           y = px)

# par(mfrow=c(1, 1))
plot(x, px, main="loess model")
# abline(v = -1.162, col = "red")
# abline(v = -0.057, col = "blue")
# abline(v = 1.166, col = "green")

# map piecewise regression to loess model. 3 breakpoints for pc1, 4 for pc2, 3 for pc3

mcp_loess = list(y ~ 1, # intercept
                  ~ 0 + x, #linear segment1 (int_1)
                  ~ 0 + x, #linear segment2 slope (time_2) at cp_1
                  ~ 0 + x # disjoined slope (int_3, time_3) at cp_2
                 # ~ 1+x
)
fit_loess = mcp(mcp_loess, data = loess_predict, sample = 'prior')
summary(fit_loess)
summaryfl = summary(fit_loess)

# mcp predicted breakpoints
plot(x, px, main="loess model")
abline(v = summaryfl$mean[1], col = "red")
abline(v = summaryfl$mean[2], col = "blue")
abline(v = summaryfl$mean[3], col = "green")
# abline(v = 2.0423, col = "green")





# Threshold - ECE - Threshold Modelling - Piece wise -----------------------------------------------------

library(segmented)
library(nlme)

# Full Dataset - Site affects slope, aridity (z) affects changepoint
## U = random effects in the slope-difference parameter
## G0 = random effects in the breakpoints
# does not give breakpoints for each individual site, have to analyze each site separately
# MAS

thres_seg(site,ps1,ps2){
  m1 = lm(pc ~ gh, data = data %>%
            dplyr::filter(site == site))
  m1s = segmented(m1, seg.Z=~gh,
                  # npsi = 2
                  # psi = psi1
                  psi = list(gh = c(psi1,psi2))
  )
}

m1s_summary = summary(m1s) 
m1s_slope = slope(m1s)
# get the fitted data
my.fitted <- fitted(m1s)
my.model <- data.frame(data %>% 
                         dplyr::filter(site == site) %>%
                         dplyr::select(gh), pc = my.fitted)

thres_lwma = thres_seg(data = aw6,
                      pc = aw6$pc2,
                      site = "lwma",
                      psi1 = -24,
                      psi2 = -14
                      ); thres_lwma[[2]];thres_lwma[[3]]

thres_sswma = thres_seg(data = aw6,
                      pc = aw6$pc2,
                      site = "sswma",
                      psi1 = -18,
                      psi2 = -10
                      ); thres_sswma[[2]];thres_sswma[[3]]

thres_cbma = thres_seg(data = aw6,
                      pc = aw6$pc2,
                      site = "cbma",
                      psi1 = -18,
                      psi2 = -10
                      ); thres_cbma[[1]];thres_cbma[[2]];thres_cbma[[3]]

thres_kiowa = thres_seg(data = aw6,
                       pc = aw6$pc2,
                       site = "kiowa",
                       psi1 = -15,
                       psi2 = -10
                       ); thres_kiowa[[1]];thres_kiowa[[2]];thres_kiowa[[3]]



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



# Threshold - ECE - findthresh --------------------------------------------

library(evir)

findthresh(aw6, 100)


# ECE - Threshold - EnvCpt ------------------------------------------

library(EnvCpt)
aw6 = aw6 %>% 
  # dplyr::filter(site == "lwma") %>%
  # dplyr::select(gh_within, pc1:pc3) %>%
  arrange(pc2)
fit_envcpt = envcpt(aw6$pc2)  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
out=fit_envcpt # run all models with default values
out[[1]] # first row is twice the negative log-likelihood for each model
# second row is the number of parameters
AIC(out) # returns AIC for each model.
which.min(AIC(out)) # gives trendar2cpt (model 12) as the best model fit.
out$trendar1cpt # gives the model fit for the meancpt model.
AICweights(out) # gives the AIC weights for each model
BIC(out) # returns the BIC for each model.
which.min(BIC(out)) # gives meancpt (model 2) as the best model fit too.
plot(out,type='fit') # plots the fits
plot(out,type="aic") # plots the aic values
plot(out,type="bic") # plots the bic values # run all models with default values

out$meancpt@cpts # example code
out$meancpt@param.est #example code

fit_envcpt$trendar1cpt@cpts # example code
fit_envcpt$trendar2cpt@param.est #example code


