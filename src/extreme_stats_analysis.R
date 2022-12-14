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
ggplot(data = extreme_arid, aes(x = gh, y = pc2, color = site)) +
  geom_point() +
  geom_smooth(method = loess)
# we do get threshold for cbma but not the other sites

# MAS Summarized data
ggplot(data = aw6, aes(x = gh, y = pc2, color = site)) +
  # geom_point() +
  geom_smooth(method = gam)


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

ex2mas = lm(pc2 ~ arid_within*site + mas_bin + scale(date), data = ea_mas)
summary(ex2mas)  
assump(ex2mas)
emmeans(ex2mas, pairwise ~ site)

ex3mas = lm(pc3 ~ arid_within*site + mas_bin + scale(date), data = ea_mas)
summary(ex3mas)  
assump(ex3mas)
emmeans(ex3mas, pairwise ~ site)

# Threshold - ECE - Threshold Modelling - Piece wise -----------------------------------------------------

library(segmented)
library(nlme)

# Full Dataset - Site affects slope, aridity (z) affects changepoint
## U = random effects in the slope-difference parameter
## G0 = random effects in the breakpoints
# does not give breakpoints for each individual site, have to analyze each site separately
# MAS
m1 = lm(pc ~ gh, data = data %>%
          dplyr::filter(site == site))
m1s = segmented(m1, seg.Z=~gh,
                # npsi = 2
                # psi = psi1
                psi = list(gh = c(psi1,psi2))
)
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

### Estimating continuous piecewise linear regression ###
# https://www.r-bloggers.com/2013/04/estimating-continuous-piecewise-linear-regression/
# N <- 325 # number of sampled points, lwma = 325, sswma = 354, cbma = 368, kiowa = 367

ggplot(data = aw4, aes(x = gh_within,
                       y = pc2,
                       color = site))+
  geom_smooth(method = loess, se = FALSE)

K <- 3  # number of knots

arid_df = aw6 %>%
  dplyr::select(site,gh,gh_within)

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

set.seed(1)
# Should use full dataset??? 
aw6 = aw6 %>%
  group_by(site) %>%
  dplyr::mutate(gh_within = scale_this(gh))

# aw6lwma = aw4 %>% dplyr::filter(site == "kiowa") %>% dplyr::select(gh_within, pc2)
# x = aw6lwma$gh_within
# y = aw6lwma$pc2
x = aw4$gh_within
y = aw4$pc2
# x <- seq(-1, 1, len = N)
# y <- f(x) + rnorm(length(x))

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

# ECE - Threshold - Impact Definition Data --------------------------------

# Setting threshold based on full dataset, gh_within = 2.26 based on 4 knots
awthres_n = aw4 %>%
  dplyr::filter(gh_within >=2.26) %>%
  group_by(site) %>%
  dplyr::summarise(n = n())

awthres = aw4 %>%
  dplyr::filter(gh_within >=2.26) %>%
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

exthresm1 = lm(pc2 ~ site*mas_bin + scale(date), data = awthres)
summary(exthresm1)
emmeans(exthresm1, pairwise ~site|mas_bin)

exthresm2 = lm(pc2 ~ site + scale(date), data = awthres)
summary(exthresm2)
emmeans(exthresm2, pairwise ~site) # at the most extreme aridity, cbma still has higher avian abundance than kiowa

# Setting threshold based on MAS dataset, gh_within = 2.02

awthres_n = aw4 %>%
  dplyr::filter(gh_within >=2.02) %>%
  group_by(site) %>%
  dplyr::summarise(n = n())

awthres = aw4 %>%
  dplyr::filter(gh_within >=2.02) %>%
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

exthresm1 = lm(pc2 ~ site*mas_bin + scale(date), data = awthres)
summary(exthresm1)
emmeans(exthresm1, pairwise ~site|mas_bin)

exthresm2 = lm(pc2 ~ site + scale(date), data = awthres)
summary(exthresm2)
emmeans(exthresm2, pairwise ~site)

# General Additive Model --------------------------------------------------

library(mgcv)
gam1 = gam(pc2 ~ s(gh, by = site), data = aw6)
summary(gam1)
coef(gam1)
plot(gam1)
emm(gam1, pairwise ~ site)
contrast(emmeans(gam1, pairwise ~ site))

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



# Threshold - ECE - findthresh --------------------------------------------

library(evir)

findthresh(aw6, 100)
