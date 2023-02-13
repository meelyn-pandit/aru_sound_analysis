library(tidyverse) #data manipulation
library(RColorBrewer) # brewer color palette
library(viridis) # viridis color palette
library(lubridate) #manipulating date and time
library(hms) #manipulate time
library(zoo) #for na.approx to approximate missing values in weather dataset
library(gridExtra) #ggplot multi panels
library(ggpubr)
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
# library(pca3d)
library(gt)
library(htmltools)
library(webshot2)
library(ggbiplot) # plot pcas
library(broom)
# library(cowplot)
# library(magick)
# library(patchwork)
# library(flextable)

### Install ggbiplot ###
library(devtools)
install_github("vqv/ggbiplot")

### Install dotwhisker ###
devtools::install_github("fsolt/dotwhisker")


setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis")
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
         sound_atten04 = att_coef(4000, temp, relh, (pres/1000)),
         sound_atten08 = att_coef(8000, temp, relh, (pres/1000)),
         sound_atten12 = att_coef(12000, temp, relh, (pres/1000)),
         gh = ((25+(19*ws2m))* 1 *(max_sat(temp)-(relh/100))) # correct gh equation (25+(19*ws2m)) whole thing needs to be in parentheses
  )

# see how many gh rows are within each arid factor

arid_check = aw4 %>% group_by(site,mas_bin,arid_within,arid_withinf) %>% tally(gh)

ggplot(data = arid_check, aes(x = arid_withinf,
                              y = n,
                              color = mas_bin)) + geom_point()

# Full clean dataset

aw4 = aw4 %>%
  group_by(site) %>%
  dplyr::mutate(gh_within = scale_this(gh)) # split up datasets by site and scale!!!

aw4$site = factor(aw4$site, levels = c("lwma","sswma","cbma","kiowa"))

# Audio Variable PCAs
audio_pca = prcomp(aw4[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
summary(audio_pca) #PC1 and PC2 have highest proportion of variance
audio_pcadf = as.data.frame(audio_pca[["x"]])
ggbiplot(audio_pca, choices = c(1,2),ellipse = TRUE, alpha = 0, groups = aw4$site) # Plot PCs
ggbiplot(audio_pca, choices = c(1,3),ellipse = TRUE, alpha = 0, groups = aw4$site) # Plot PCs

### PC1: ADI and AEI, higher values mean higher diversity (after running line 65)
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI

aw4$pc1 = audio_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
aw4$pc2 = audio_pcadf$PC2 
aw4$pc3 = audio_pcadf$PC3

save(aw4, file = "data_clean/aridity_data_clean.Rdata")

# Checking full dataset and gam plots
ggplot(data = aw4, aes(x = gh,
                              y = pc2,
                              color = site)) +
  geom_smooth(method = lm)
# # Sound Attenuation PCAs - all pcs in the same direction
# atten_pca = prcomp(aw4[,c("sound_atten04","sound_atten08","sound_atten12")])
# summary(atten_pca)
# ggbiplot(atten_pca, choices = c(2,3),ellipse = TRUE, alpha = 0, groups = aw4$site) # Plot PCs

# #3D pCA Plot
# pca3d(audio_pca, biplot = true) # only run this on windows machine
# snapshotPCA3d("audio_pca.png")

# AIC tests to see which aridity variable to use --------------------------
# Arid within factor
m_aridwithin = lmer(pc2 ~ arid_withinf*site*mas_bin + 
                      # mas_bin + 
                      scale(date) + 
                      (1|site), data = aw4)
summary(m_aridwithin)
assump(m_aridwithin)
emmeans(m_aridwithin, pairwise ~ site*arid_withinf|mas_bin, lmerTest.limit = 54007)
# Arid across factor
m_aridacross = lmer(pc3 ~ arid_acrossf*site + 
                      mas_bin + 
                      scale(date) + 
                      (1|site), data = aw4)

# Historic arid within factor
m_histwithin = lmer(pc3 ~ hist_withinf*site + 
                      mas_bin + 
                      scale(date) + 
                      (1|site), data = aw4)

# Historic arid within factor
m_histacross = lmer(pc3 ~ hist_acrossf*site + 
                      mas_bin + 
                      scale(date) + 
                      (1|site), data = aw4)

# Sound attenuation at 4kHz
m_sound4khz = lmer(pc3 ~ sound_atten04*site + 
                      mas_bin + 
                      scale(date) + 
                      (1|site), data = aw4)

# Sound attenuation at 8kHz
m_sound8khz = lmer(pc3 ~ sound_atten08*site + 
                     mas_bin + 
                     scale(date) + 
                     (1|site), data = aw4)

m_sound12khz = lmer(pc3 ~ sound_atten12*site + 
                     mas_bin + 
                     scale(date) + 
                     (1|site), data = aw4)

AICctab(m_aridwithin,m_aridacross,m_histwithin,m_histacross,m_sound4khz,m_sound8khz,m_sound12khz, nobs = 54007, base=T, weights=T, delta=T, logLik=T)

# arid_within is best aridity metric to predict pc1, pc2
# arid_across is best aridity metric to predict pc3,
# sound_attn4khz is second best aridity metric to predict pc3

# PC1: ADI, AEI, positive  values more likely to have higher ADI
arid_pc1 = aridity_contrasts_lmer(aw4, aw4$pc1)
arid_pc1[[5]] %>% gtsave("results/arid_grad_pc1_lag.png")
plot(arid_pc1[[4]])

# m1 = lmer(pc1 ~ gh + mas_bin + scale(date) + (gh|site), data = aw4, REML = FALSE)


# emm_options(pbkrtest.limit = 54931) # run this R will crash
emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons
emmeans(m1, ~ site)

pairs(emmeans(m1, ~ site|arid_within), data = aw4)
pairs(emmeans(m1, ~ arid_within|site), data = aw4)

# PC2: Num vocals and species diversity
m2 = lmer(pc2 ~ site*arid_within + scale(date_time) + (1|site), data = aw4)
summary(m2)
assump(m2)
emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons
pairs(emmeans(m2, ~ site|arid_within), data = aw4)
pairs(emmeans(m2, ~ arid_within|site), data = aw4)

# PC3: ACI and BIO
m3 = lmer(pc3 ~ site*arid_within + scale(date_time) + (1|site), data = aw4)
summary(m3)
assump(m3)
emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons
pairs(emmeans(m3, ~ site|arid_within), data = aw4)
pairs(emmeans(m3, ~ arid_within|site), data = aw4)

# Aridity Gradient - Summarized by Datetime -------------------------------
aw5 = aw4 %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  mutate(date = as_date(date_time)) %>%
  group_by(site, date_time) %>%
  dplyr::summarise(n = n(),
                   aci_mean = mean(aci, na.rm = TRUE),
                   bio_mean = mean(bio, na.rm = TRUE),
                   adi_mean = mean(adi, na.rm = TRUE),
                   aei_mean = mean(aei, na.rm = TRUE),
                   pc1_mean = mean(pc1),
                   pc2_mean = mean(pc2),
                   pc3_mean = mean(pc3),
                   vocals_mean = mean(num_vocals),
                   species_mean = mean(species_diversity),
            mean_aridwithin = factor(round(mean(as.numeric(arid_within))),levels = c(1,2,3,4,5)),
            mean_aridacross = as.factor(round(mean(as.numeric(arid_across)))),
            mean_histwithin = as.factor(round(mean(as.numeric(hist_within)))),
            mean_histacross = as.factor(round(mean(as.numeric(hist_across)))))

# audio_pca2 = prcomp(aw5[,c(4:9)], center = TRUE, scale. = TRUE)
# audio_pca2 = prcomp(aw5[,c(5:10)], center = TRUE, scale. = TRUE) # use if summarize by date and mas_bin

# ggbiplot(audio_pca2, choices = c(2,3),ellipse = TRUE, alpha = 0, groups = aw5$site) # Plot PCs
# #3D Plot of PCAs
# pca3d(audio_pca2, biplot = true)
# snapshotPCA3d("audio_pca_datetime.png")

# summary(audio_pca2) #PC1 and PC2 have highest proportion of variance
# audio_pcadf2 = as.data.frame(audio_pca2[["x"]]) # Creating dataframe of PCA variance table
# ggbiplot(audio_pca2, ellipse = TRUE, alpha = 0, groups = aw5$site) #Plotting PCAs to see directions
# # Displaying PCAs 1 and 3
# ggbiplot(audio_pca2, choices=c(1,2,3),ellipse = TRUE, alpha = 0, groups = aw5$site) #Plotting PCAs to see directions


# aw5$pc1 = audio_pcadf2$PC1 # Higher PC1 leads to higher ADI i.e. acoustic diversity
# aw5$pc2 = audio_pcadf2$PC2 # Higher PC2 leads to higher num_vocals and species_diversity

# Aridity Gradient - Datetime - Statistical Analysis ----------------------
# PC1: ACI, ADI, AEI, negative values more likely to have higher ADI
m1 = lm(pc1_mean ~ site*mean_aridwithin + scale(date_time), data = aw5)
m1emmeans = emmeans(m1, ~ site|mean_aridwithin)
summary(m1)
assump(m1)
Anova(m1)
pairs(emmeans(m1, ~site|mean_aridwithin, data = aw5)) #across site comparisions
summary(m1dt_across_sites)
pairs(emmeans(m1, ~mean_aridwithin|site, data = aw5)) # within site comparisons
summary(m1dt_within_sites)


# PC2: Vocalization Number, Species Diversity
m2 = lm(pc2_mean ~ site*mean_aridwithin + scale(date_time), data = aw5)
summary(m2)
assump(m2)
Anova(m2)
pairs(emmeans(m2, ~site|mean_aridwithin, data = aw5)) #across site comparisions
pairs(emmeans(m2, ~mean_aridwithin|site, data = aw5)) # within site comparisons
summary(m2dt_across_sites)
summary(m2dt_within_sites)
pwpp(emmeans(m2, ~mean_aridwithin|site, data = aw5)) # Pairwise p-value plots

# PC3: ACI and BIO
m3 = lm(pc3_mean ~ site*mean_aridwithin + scale(date_time), data = aw5)
summary(m3)
assump(m3)
Anova(m3)
pairs(emmeans(m3, ~site|mean_aridwithin, data = aw5)) #across site comparisions
pairs(emmeans(m3, ~mean_aridwithin|site, data = aw5)) # within site comparisons
pwpp(emmeans(m3, ~mean_aridwithin|site, data = aw5)) # Pairwise p-value plots


# Aridity Gradient - Summarized by Date and MAS ---------------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
load("data_clean/aridity_data_clean.Rdata")

aw6 = aw4 %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  # mutate_at(c("arid_withinf", "arid_acrossf", "hist_withinf", "hist_acrossf"), as.numeric) %>%
  mutate_at(c("arid_withinf", "arid_acrossf"), as.numeric) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise_at(vars(aci:species_diversity, 
                           temp:dew, 
                           gh:ws10m,
                           # gh, 
                           # gh_within,
                           # arid_within,
                           # arid_across,
                           # arid_withinf,
                           # arid_acrossf,
                           # hist_within:arid_across,
                           # arid_withinf:hist_acrossf,
                           sound_atten04:sound_atten12,
                           pc1:pc3), ~ mean(.x, na.rm = TRUE)) 
  
# mutate_at(c("arid_withinf","arid_acrossf","hist_within","hist_across"), round_factor)

# Creating normalized aridity variable and factor within sites

all_sites = NULL
for(i in unique(aw6$site)) {
  aw_site = aw6 %>% dplyr::filter(site == i) %>%
    dplyr::arrange(gh)
  aw_site$arid_within = as.vector(scale(aw_site$gh))
  aw_site$arid_withinf = cut(aw_site$arid_within, breaks = 5, labels = c(1,2,3,4,5))
  all_sites = rbind(all_sites,aw_site)
}
aw6 = all_sites

# Checking to see if histograms match
hist(aw6$gh)
hist(aw6$arid_within)
hist(as.numeric(aw6$arid_withinf))


# Creating aridity variable and factor normalized across sites
aw6 = aw6 %>% 
  arrange(gh)
aw6$arid_across = as.vector(scale(aw6$gh))
aw6$arid_acrossf = cut(aw6$arid_across, breaks = 5, labels = c(1,2,3,4,5))

# Checking to see if histograms match
hist(aw6$gh)
hist(as.numeric(aw6$arid_across))
hist(as.numeric(aw6$arid_acrossf))

aw6$arid_within-aw6$arid_across

ggplot(data = aw6, 
       aes(x = arid_across,
           y = pc2,
           color = site
       )) + 
  geom_point() +
  geom_smooth()

aw6 %>% group_by(site,arid_withinf) %>% tally(n())

aw6 %>% group_by(site,arid_acrossf) %>% tally()

# Creating MAS bin labels for graphs
aw6$mas_labels = factor(aw6$mas_bin, levels = c("0","1","2","3"),
                        labels = c("Predawn","Early","Mid","Late"))

# Creating site labels for graphs
aw6$site_labels = factor(aw6$site, levels = c("lwma","sswma","cbma","kiowa"),
                         labels = c("LWMA","SSWMA","CBMA","KIOWA"))

save(aw6, file = "data_clean/aridity_gradient_mas.Rdata")

# Aridity Gradient - Summarized by Date and MAS - LINEAR MODELS! --------
### Aridity (gh) is treated as a continuous variable rather than a factor to reduce complexity

load("data_clean/aridity_gradient_mas.Rdata")

### PC 1 - Acoustic Diversity
## LM for PC1 - Acoustic Diversity

lmpc1 = ag_contrasts_convar_site(aw6,
                                 aw6$pc1,
                                 aw6$gh)
lmpc1[[5]] %>% gtsave("results/ag_pc1_contrasts.png", 
                             vwidth = 20000, 
                             vheight = 15000, 
                             expand = 100)
lmpc1[[7]] %>% gtsave("results/ag_pc1_slopes.png", 
                      vwidth = 20000, 
                      vheight = 15000, 
                      expand = 100)

### Good Paper graphs for lm regressions
ag_graph_site_paper(aw6$pc1, 
                    aw6$gh,
                    "PC1 - Acoustic Diversity",
                    "Evaporation Rate (kg of water/h")
ggsave('results/arid_grad_pc1_site_paper.png', dpi = 600, height = 6, width = 8, units = "in")


ag_graph_time_paper(aw6$pc1, 
                    aw6$gh,
                    "PC1 - Acoustic Diversity",
                    "Sound Attenuation at 8kHz")
ggsave('results/arid_grad_pc1_site_time.png', dpi = 600, height = 6, width = 8, units = "in")

assump(m1)

plot(emmeans(m1,~ gh*site|mas_bin, type = 'response'))
contrast(emmeans(m1, ~ gh*site|mas_bin), type = 'response')

# m1 = lm(pc1 ~ gh*site*mas_bin + scale(date), data = aw6)
# summary(m1)
# tidy(m1, conf.int = TRUE) %>% print(n = 100)
# emm1 = emmeans(m1, ~ gh*site|mas_bin, type = 'response');emm1
# emtrends(m1, pairwise ~ site|mas_bin, var = "gh", type = 'response',weights = "cells") # across sites
# emtrends(m1, pairwise ~ mas_bin|site, var = "gh", type = 'response',weights = "cells") # within sites
# tidy(emm1)



### PC2 - Avian Abundance
## LMs for PC2
lmpc2 = ag_contrasts_convar_site(aw6,
                                 aw6$pc2,
                                 aw6$gh)

lmpc2[[5]] %>% gtsave("results/ag_pc2_contrasts.png", 
                      vwidth = 20000, 
                      vheight = 15000, 
                      expand = 100)
lmpc2[[7]] %>% gtsave("results/ag_pc2_slopes.png", 
                      vwidth = 20000, 
                      vheight = 15000, 
                      expand = 100)

ag_graph_site_paper(aw6$pc2, 
                    aw6$gh,
                    "PC2 - Avian Abundance",
                    "Evaporation Rate (kg of water/h")
ggsave('results/arid_grad_pc2_site_paper.png', dpi = 600, height = 6, width = 8, units = "in")

ag_graph_time_paper(aw6$pc2, 
                    aw6$gh,
                    "PC2 - Avian Abundance",
                    "Evaporation Rate (kg of water/h")
ggsave('results/arid_grad_pc2_time_paper.png', dpi = 600, height = 6, width = 8, units = "in")

# Plotting PCs across sound attenuation
atten_graph_site_paper(aw6$pc2, 
                       aw6$sound_atten08, 
                       "PC2 - Avian Abundance", 
                       "Sound Attenuation at 8kHz (m)")

atten_graph_time_paper(aw6$pc2, 
                       aw6$sound_atten08, 
                       "PC2 - Avian Abundance", 
                       "Sound Attenuation at 8kHz (m)")

# m2 = lm(pc2 ~ gh*site*mas_bin + scale(date), data = aw6)
# summary(m2)
# emtrends(m2, pairwise ~ site|mas_bin, 
#          var = "gh", type = 'response',weights = "cells") # across sites
# emtrends(m2, pairwise ~ mas_bin|site, 
#          var = "gh", type = 'response',weights = "cells") # within sites, across time


assump(m2)

# emmeans(m2, ~ gh*site|mas_bin, type = "response")
# plot(emmeans(m2, ~ gh*site|mas_bin))
# emmip(m2, site ~ gh|mas_bin, cov.reduce = range)
# contrast(emmeans(m2, ~ pairwise ~ gh*site|mas_bin))

### PC3 - Acoustic Complexity
## LMs for PC2
lmpc3 = ag_contrasts_convar_site(aw6,
                         aw6$pc3,
                         aw6$gh)

lmpc3[[5]] %>% gtsave("results/ag_pc3_contrasts.png", 
                      vwidth = 20000, 
                      vheight = 15000, 
                      expand = 100)
lmpc3[[7]] %>% gtsave("results/ag_pc3_slopes.png", 
                      vwidth = 20000, 
                      vheight = 15000, 
                      expand = 100)

# m3 = lm(pc3 ~ gh*site*mas_bin + scale(date), data = aw6)
# summary(m3)
# emtrends(m3, pairwise ~ mas_bin|site, var = "gh", type = 'response',weights = "cells") # within sites, across time
# 
# emtrends(m3, pairwise ~ site|mas_bin, var = "gh", type = 'response',weights = "cells") # across sites

ag_graph_site_paper(aw6$pc3, 
                    aw6$gh,
                    "PC3 - Acoustic Complexity",
                    "Evaporation Rate (kg of water/h")
ggsave('results/arid_grad_pc3_site_paper.png', dpi = 600, height = 6, width = 8, units = "in")


ag_graph_time_paper(aw6$pc3, 
                    aw6$gh,
                    "PC3 - Acoustic Complexity",
                    "Evaporation Rate (kg of water/h")
ggsave('results/arid_grad_pc3_time_paper.png', dpi = 600, height = 6, width = 8, units = "in")


assump(m3)
# emmeans(m3, ~ gh*site|mas_bin, type = "response")
# plot(emmeans(m3, ~ gh*site|mas_bin))
# plot(emmeans(m3, ~ site|mas_bin))

emmip(m3, site ~ gh|mas_bin, cov.reduce = range)
contrast(emmeans(m3, ~ pairwise ~ gh*site|mas_bin))

### Big table for all aridity gradient pcs
pc_tables = ag_slopes_table(lmpc1[[6]],
                            lmpc2[[6]],
                            lmpc3[[6]])

pc_tables %>% gtsave("results/ag_all_pcs_slopes.png", 
                      vwidth = 1100,
                      # vheight = 15000, 
                      expand = 1000)

# Aridity Gradient - Date and MAS - Statistical Analysis ------------------
# PC1: ADI, AEI, positive values more likely to have higher ADI 
# (after being multiplied by -1)
# setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis")

m1 = lm(pc1 ~ arid_withinf*mas_bin*site + scale(date), 
        data = aw6)
summary(m1)
assump(m1)
emm = emmeans(m1,  pairwise ~ site*arid_withinf|mas_bin)
source(aridity_contrasts_mas)
arid_pc1_mas = aridity_contrasts_mas(aw6$pc1,
                                     aw6$arid_withinf);arid_pc1_mas
write.csv(arid_pc1_mas[[5]], 'results/arid_pc1_mas_table.csv', row.names = FALSE)
arid_pc1_mas[[5]] %>% gtsave("results/arid_gradient_pc1_mas.png", 
                             vwidth = 20000, 
                             vheight = 15000, 
                             expand = 100)
plot(arid_pc1_mas[[6]])
aridity_graph_table(arid_pc1_mas[[6]],
                    "PC1 - Acoustic Diversity",
                    "results/arid_pc1_mas.png")


# PC2: Vocalization Number, Species Diversity higher with positive values
# (after being multiplied by -1)
arid_pc2_mas = aridity_contrasts_mas(aw6$pc2,
                                     aw6$arid_withinf)
arid_pc2_mas[[3]]

# write.csv(arid_pc2_mas[[5]], 'results/arid_pc2_mas_table.csv', row.names = FALSE)
arid_pc2_mas[[5]] %>% gtsave("results/arid_gradient_pc2_mas.png", vwidth = 20000, vheight = 15000, expand = 100)
aridity_graph_table(arid_pc2_mas[[6]],
                    "PC2 - Avian Abundance",
                    "results/arid_pc2_mas.png")
# plot(arid_pc2_mas[[6]])

# PC3: ACI, BIO higher positive values have higher ACI and lower BIO
# (after being multiplied by -1)
arid_pc3_mas = aridity_contrasts_mas(aw6$pc3,
                                     aw6$arid_withinf)
arid_pc3_mas[[3]]
# write.csv(arid_pc3_mas[[5]], 'results/arid_pc3_mas_table.csv', row.names = FALSE)
 
arid_pc3_mas[[5]] %>% gtsave("results/arid_gradient_pc3_mas.png", vwidth = 20000, vheight = 15000, expand = 100)
aridity_graph_table(arid_pc3_mas[[6]],
                    "PC3 - Acoustic Complexity",
                    "results/arid_pc3_mas.png")
plot(arid_pc3_mas[[6]])

# Aridity Graident - Dot Plots - Date and MAS -----------------------------

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs

# Load MAS binned data
load("data_clean/aridity_gradient_mas.Rdata")

mas_graphs = aw6 %>%
  group_by(site, mas_bin, arid_withinf) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
                   pc1_se = (sd(pc1))/sqrt(n()),
                   pc2_mean = mean(pc2),
                   pc2_se = (sd(pc2))/sqrt(n()),
                   pc3_mean = mean(pc3),
                   pc3_se = (sd(pc3))/sqrt(n())) %>%
  dplyr::mutate(mas_bin = case_when(mas_bin == "0" ~ "Predawn",
                                    mas_bin == "1" ~ "Early",
                                    mas_bin == "2" ~ "Mid",
                                    mas_bin == "3" ~ "Late")) %>%
  dplyr::mutate(mas_bin = factor(mas_bin,
                                 levels = c("Predawn",
                                            "Early",
                                            "Mid",
                                            "Late")))
### PC1 - Acoustic Diversity
ggplot(data = mas_graphs,
       aes(x=arid_withinf, y=pc1_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  # ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc1_mean-pc1_se, 
                    ymax = pc1_mean+pc1_se), width = 0.5,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  # scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid")) +
  scale_x_discrete(name = "Aridity - Normalized Within") +
  scale_y_continuous(name = "PC1 - Acoustic Diversity")+
  facet_grid(. ~ mas_bin) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "bottom") +
  # facet_wrap(vars(mas_bin)) + 
  theme(strip.text.y = element_text(angle = 0))
ggsave('results/arid_pc1_mas.png', dpi = 600, height = 6, width = 8, units = "in")

### PC2 - Num vocals and Species Diversity
ggplot(data = mas_graphs,
       aes(x=arid_withinf, y=pc2_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  # ggtitle("Datetime Summarized - PC2 - Avian Vocal Abundance")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc2_mean-pc2_se, 
                    ymax = pc2_mean+pc2_se), width = 0.5,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  # scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_x_discrete(name = "Aridity - Normalized Within") +
  scale_y_continuous(name = "PC2 - Avian Abundance")+
  facet_grid(. ~ mas_bin) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "bottom") +
  # facet_grid(vars(mas_bin)) + 
  theme(strip.text.y = element_text(angle = 0))
ggsave('results/arid_pc2_mas.png', dpi = 600, height = 6, width = 8, units = "in")

### PC3 - ACI and BIO
ggplot(data = mas_graphs,
       aes(x=arid_withinf, y=pc3_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  # ggtitle("Datetime Summarized - PC3 - Acoustic Complexity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc3_mean-pc3_se, 
                    ymax = pc3_mean+pc3_se), width = 0.5,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  # scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_x_discrete(name = "Aridity - Normalized Within") +
  scale_y_continuous(name = "PC3 - Acoustic Complexity")+
  facet_grid(. ~ mas_bin) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "bottom")+
  # facet_grid(vars(mas_bin)) + 
  theme(strip.text.y = element_text(angle = 0))
ggsave('results/arid_pc3_mas.png', dpi = 600, height = 6, width = 8, units = "in")


# Water Supp - Load Data ---------------------------------------

load("data_clean/water_audio_and_weather_data.Rdata")

ww = water_weather3 %>%
  dplyr::mutate(gh = ((25+(19*ws2m))* 1 *(max_sat(temp)-(relh/100)))) %>%
  dplyr::filter(date_time < "2021-08-16") %>%
  dplyr::filter(year(date_time) == 2021) %>%
  dplyr::mutate(rain = replace_na(rain,0)) %>%
  dplyr::filter(rain == 0) %>%
  arrange(desc(aci))

ww_bad1 = ww %>% dplyr::filter(as_date(date_time) == "2021-06-26" & site == "sswma") 
ww_bad2 = ww %>% dplyr::filter(as_date(date_time) == "2021-06-27" & site == "sswma")
ww_bad3 = ww %>% dplyr::filter(as_date(date_time) == "2021-07-02" & site == "sswma") 
ww_bad4 = ww %>% dplyr::filter(as_date(date_time) == "2021-07-07" & site == "cbma")
ww_bad5 = ww %>% dplyr::filter(as_date(date_time) == "2021-07-18" & site == "sswma")
ww_bad6 = ww %>% dplyr::filter(as_date(date_time) == "2021-08-01" & site == "sswma")

ww_badtotal = rbind(ww_bad1, ww_bad2, ww_bad3, ww_bad4, ww_bad5, ww_bad6)

ww2 = setdiff(ww, ww_badtotal)
ww2$site = factor(ww2$site, levels = c("lwma","sswma","cbma","kiowa"))

ww3 = ww2 %>% 
  dplyr::filter(is.na(mas_bin) == FALSE) %>%
  dplyr::filter(is.na(aci) == FALSE) %>%
  mutate(sound_atten04 = att_coef(4000, temp, relh, (pres/1000)),
         sound_atten08 = att_coef(8000, temp, relh, (pres/1000)),
         sound_atten12 = att_coef(12000, temp, relh, (pres/1000)))

save(ww3, file = "data_clean/filtered_water_supp_data.Rdata")

# Water Supp - SSWMA - Subset and create PCAs ------------------------------------

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

save(sswma_water, file = "data_clean/sswma_water.Rdata")

# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lmer(pc1 ~ ws_site*water*arid_withinf + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m1)
assump(m1)

emmeans(m1, pairwise ~ ws_site:water|arid_withinf)


# PC2: Num vocals and species diversity
m2 = lmer(pc2 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m2)
assump(m2)
emmeans(m2, pairwise ~ ws_site:water|arid_within)

# emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons


# PC3: ACI and BIO
m3 = lmer(pc3 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m3)
assump(m3)
emmeans(m3, pairwise ~ ws_site:water|arid_within)

# emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons



# Water Supp - SSWMA - Full Dataset - Rectangle Plots -------------------------

### SSWMA Water Supplementation Rectangle Graphs - PC1
sswma_pc1graph = sswma_rectangle_graph(sswma_water, 
                                       sswma_water$pc1, 
                                       # sswma_water$arid_within,
                                       "PC1 - Acoustic Diversity"); sswma_pc1graph
ggsave('results/sswma_fullwater_rectangle_graph_pc1.png',
       dpi = 600, height = 6, width = 8, units = "in")

### SSWMA Water Supplementation Rectangle Graphs - PC2
sswma_pc2graph = sswma_rectangle_graph(sswma_water, 
                                       sswma_water$pc2, 
                                       "PC2 - Avian Abundance"); sswma_pc2graph
ggsave('results/sswma_fullwater_rectangle_graph_pc2.png',
       dpi = 600, height = 6, width = 8, units = "in")

### SSWMA Water Supplementation Rectangle Graphs - pc3
sswma_pc3graph = sswma_rectangle_graph(sswma_water, 
                                       sswma_water$pc3, 
                                       "PC3 - Acoustic Complexity"); sswma_pc3graph
ggsave('results/sswma_fullwater_rectangle_graph_pc3.png',
       dpi = 600, height = 6, width = 8, units = "in")


# Water Supp - SSWMA - Date and MAS - Statistical Analysis - Pairwise-------------

sswma_watermas = sswma_water %>%
  mutate(date = date(date_time)) %>%
  group_by(site, ws_site, water, arid_withinf, date, mas_bin) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))
# Could try a gam?
sswmawmas_gam1 = gam(pc2 ~ ws_site*water + s(arid_within, bs = "cs", k = -1) + 
                                    s(as.numeric(date), bs = "cs", k = -1), 
                     data = sswma_watermas)
summary(sswmawmas_gam1)
plot(sswmawmas_gam1)
emmeans(sswmawmas_gam1, pairwise ~ ws_site*water)

# PC1: ADI, AEI, positive  values more likely to have higher ADI
sswma_pairwise_pc1 = sswma_water_contrasts(data = sswma_watermas,
                                           pc = sswma_watermas$pc1); sswma_pairwise_pc1
sswma_pairwise_pc1[[5]] %>% gtsave("results/sswma_water_pc1_pairwise.png")
plot(sswma_pairwise_pc1[[4]])

# PC2: Num vocals and species diversity
sswma_pairwise_pc2 = sswma_water_contrasts(data = sswma_watermas,
                                           pc = sswma_watermas$pc2); sswma_pairwise_pc2
sswma_pairwise_pc2[[5]] %>% gtsave("results/sswma_water_pc2_pairwise.png")
plot(sswma_pairwise_pc2[[4]])

# PC3: ACI and BIO
sswma_pairwise_pc3 = sswma_water_contrasts(data = sswma_watermas,
                                           pc = sswma_watermas$pc3); sswma_pairwise_pc3
sswma_pairwise_pc3[[5]] %>% gtsave(paste0("results/sswma_water_pc3_pairwise.png"))
plot(sswma_pairwise_pc3[[4]])

sswma_pc_table = sswma_water_table2(sswma_pairwise_pc1[[3]],
                                    sswma_pairwise_pc2[[3]],
                                    sswma_pairwise_pc3[[3]])
sswma_pc_table %>% gtsave("results/sswma_water_allpcs_pairwise.png",
                          expand = 100,
                          vwidth = 2000, 
                          vheight = 1500)


# Water Supp - SSWMA - Data Organization - Lag Analysis --------

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

sswma_wlag = rbind(sswma_wlag1, sswma_wlag2, sswma_wlag3)  %>%
  dplyr::mutate(date = as_date(date_time),
                ws_site = as.factor(ws_site),
                water = as.factor(water))

# sswma_fullwater = prcomp(sswma_wlag[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
# 
# sswma_fullwater_pcadf = as.data.frame(sswma_fullwater[["x"]])
# ggbiplot(sswma_fullwater, choices = c(1,2),ellipse = TRUE, alpha = 0) # Plot PCs
# ggbiplot(sswma_fullwater, choices = c(1,3),ellipse = TRUE, alpha = 0) # Plot PCs

### PC1: ADI and AEI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and BIO

# sswma_wlag$pc1 = sswma_fullwater_pcadf$PC1
# sswma_wlag$pc2 = sswma_fullwater_pcadf$PC2
# sswma_wlag$pc3 = sswma_fullwater_pcadf$PC3


# Try to analyze water supp data with GAM ---------------------------------


library(mgcv)
library(tidymv)

ggplot(data = sswma_wlag,
       aes(x = date_time,
           y = pc1, color = ws_site)) +
  geom_smooth(method = "gam")
sswma_gam1 = gam(pc1 ~ ws_site + s(gh, bs = "cs", k = -1) + 
                   s(as.numeric(date_time), bs = "cs", k = -1), data = sswma_wlag);sswma_gam1
summary(sswma_gam1)
plot(sswma_gam1, se=TRUE,col="blue")
predict_model = predict_gam(sswma_gam1) %>%
  ggplot(aes(as.numeric(date_time), fit)) +
  scale_x_continuous(sec.axis = sec_axis(~as_datetime(.), name = 'Actual Datetime'))+
  geom_smooth_ci(ws_site);predict_model


# sswma_waterpca = prcomp(sswmawl[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
# 
# sswma_waterpcadf = as.data.frame(sswma_waterpca[["x"]])
# ggbiplot(sswma_waterpca, choices = c(1,2),ellipse = TRUE, alpha = 0, groups = sswmawl$site) # Plot PCs

# #3D pCA Plot
# pca3d(sswma_waterpca, biplot = true) # only run this on windows machine
# snapshotPCA3d("sswma_water_lag_pca.png")

### PC1: ADI and AEI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and BIO

# sswmawl$pc1 = sswma_waterpcadf$PC1*-1
# sswmawl$pc2 = sswma_waterpcadf$PC2*-1
# sswmawl$pc3 = sswma_waterpcadf$PC3

# Water Supp - SSWMA - Date and MAS - Statistical Analysis - Lag ----------------------
# Create Lag dataframe, only focusing on last week of each water supplementation period to account for habituation period
sswmawl = sswma_wlag %>%
  dplyr::filter(date(date_time)>= "2021-05-23" & date(date_time) <"2021-05-30"| date(date_time) >= "2021-06-22" & date(date_time) < "2021-07-02" & date(date_time) >= "2021-06-06" & date(date_time) <"2021-06-12"| date(date_time) >= "2021-07-21" & date(date_time) < "2021-08-07")

sswma_maslag = sswmawl %>%
  mutate(date = date(date_time),
         ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, arid_withinf,date, mas_bin) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))

### Creating Labels for Graphs
# mas labels
sswma_maslag$mas_labels = factor(sswma_maslag$mas_bin, levels = c("0","1","2","3"),
                        labels = c("Predawn","Early","Mid","Late"))

# site labels
sswma_maslag$wssite_labels = factor(sswma_maslag$ws_site, levels = c("1","2","3"),
                         labels = c("Water Site 1", "Water Site 2", "Water Site 3"))



# PC1: ADI, AEI, positive  values more likely to have higher ADI
sswma_lag_pc1 = sswma_water_contrasts(data = sswma_maslag,
                                           pc = sswma_maslag$pc1); sswma_lag_pc1
# sswma_lag_pc1[[5]] %>% gtsave("results/sswma_water_pc1_lag.png")
# plot(sswma_lag_pc1[[4]])

# Create graph to show water site slopes
sswma_water_site_paper(sswma_maslag,
                       sswma_maslag$pc1,
                       sswma_maslag$gh,
                       "PC1 - Acoustic Diversity",
                       "Evaporation Rate (kg of water/h)")


# PC2: Num vocals and species diversity
sswma_lag_pc2 = sswma_water_contrasts(data = sswma_maslag,
                                           pc = sswma_maslag$pc2); sswma_lag_pc2
# sswma_lag_pc2[[5]] %>% gtsave("results/sswma_water_pc2_lag.png")
# plot(sswma_lag_pc2[[4]])
# Create graph to show water site slopes
sswma_water_site_paper(sswma_maslag,
                       sswma_maslag$pc2,
                       sswma_maslag$gh,
                       "PC2 - Avian Abundance",
                       "Evaporation Rate (kg of water/h)")

# PC3: ACI and BIO
sswma_lag_pc3 = sswma_water_contrasts(data = sswma_maslag,
                                           pc = sswma_maslag$pc3); sswma_lag_pc3
# sswma_lag_pc3[[5]] %>% gtsave(paste0("results/sswma_water_pc3_lag.png"))
# plot(sswma_lag_pc3[[4]])
sswma_pc_table = sswma_water_table3(sswma_lag_pc1[[4]],
                                    sswma_lag_pc2[[4]],
                                    sswma_lag_pc3[[4]]); sswma_pc_table
sswma_pc_table %>% gtsave("results/sswma_water_allpcs_lag.png",
                          expand = 100,
                          vwidth = 2000, 
                          vheight = 1500)

# Water Supp - CBMA - Full Dataset - Data Organization -------------

load("data_clean/filtered_water_supp_data.Rdata")

cbma_water = ww3 %>%
  dplyr::filter(site == "cbma") %>%
  mutate(ws_site = as.factor(ws_site),
         water = as.factor(water),
         date = date(date_time),
         week = week(date_time),
         sound_atten) %>%
  arrange(date_time,ws_site,water)

cbma_water_pca = prcomp(cbma_water[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
summary(cbma_water_pca) #PC1 and PC2 have highest proportion of variance
cbma_water_pcadf = as.data.frame(cbma_water_pca[["x"]])
ggbiplot(cbma_water_pca, choices = c(1,2), ellipse = TRUE, alpha = 0, groups = cbma_water$ws_site) # need to multiply pc1 by -1
ggbiplot(cbma_water_pca, choices = c(1,3), ellipse = TRUE, alpha = 0, groups = cbma_water$ws_site) # need to multiply pc3 by -1

### PC1: ADI and AEI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and higher BIO

cbma_water$pc1 = cbma_water_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
cbma_water$pc2 = cbma_water_pcadf$PC2 # Multiply PC2 by -1 to make num-vocals and species diversity have positive values
cbma_water$pc3 = cbma_water_pcadf$PC3*-1

save(cbma_water, file = "data_clean/cbma_water.Rdata")

# Water Supp - CBMA - Rectangle Graphs ------------------------------------

### CBMA Water Supplementation Rectangle Graphs - PC1
cbma_pc1graph = cbma_rectangle_graph(cbma_water, 
                                       cbma_water$pc1, 
                                       # cbma_water$arid_within,
                                       "PC1 - Acoustic Diversity"); cbma_pc1graph
ggsave('results/cbma_fullwater_rectangle_graph_pc1.png',
       dpi = 600, height = 6, width = 8, units = "in")

### CBMA Water Supplementation Rectangle Graphs - PC2
cbma_pc2graph = cbma_rectangle_graph(cbma_water, 
                                       cbma_water$pc2, 
                                       "PC2 - Avian Abundance"); cbma_pc2graph
ggsave('results/cbma_fullwater_rectangle_graph_pc2.png',
       dpi = 600, height = 6, width = 8, units = "in")

### CBMA Water Supplementation Rectangle Graphs - pc3
cbma_pc3graph = cbma_rectangle_graph(cbma_water, 
                                     cbma_water$pc3, 
                                       "PC3 - Acoustic Complexity"); cbma_pc3graph
ggsave('results/cbma_fullwater_rectangle_graph_pc3.png',
       dpi = 600, height = 6, width = 8, units = "in")

# Water Supp - CBMA - Date and MAS - Data Organization --------------------

cbma_watermas = cbma_water %>%
  mutate(date = date(date_time)) %>%
  group_by(site, ws_site, water, arid_withinf, date, mas_bin) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))


# Water Supp - CBMA - Date and MAS - Pairwise Analysis --------------------


# PC1: ADI, AEI, positive  values more likely to have higher ADI
cbma_pairwise_pc1 = cbma_water_contrasts(data = cbma_watermas,
                                           pc = cbma_watermas$pc1); cbma_pairwise_pc1
cbma_pairwise_pc1[[5]] %>% gtsave("results/cbma_water_pc1_pairwise.png")
plot(cbma_pairwise_pc1[[4]])

# PC2: Num vocals and species diversity
cbma_pairwise_pc2 = cbma_water_contrasts(data = cbma_watermas,
                                           pc = cbma_watermas$pc2); cbma_pairwise_pc2
cbma_pairwise_pc2[[5]] %>% gtsave("results/cbma_water_pc2_pairwise.png")
plot(cbma_pairwise_pc2[[4]])

# PC3: ACI and BIO
cbma_pairwise_pc3 = cbma_water_contrasts(data = cbma_watermas,
                                           pc = cbma_watermas$pc3); cbma_pairwise_pc3
cbma_pairwise_pc3[[5]] %>% gtsave(paste0("results/cbma_water_pc3_pairwise.png"))
plot(cbma_pairwise_pc3[[4]])


# Water Supp - CBMA - Data Organization - Lag ------------------
### Only analyzing half of water supplementation period
# Separating out CBMA water sites

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

# cbmawl = rbind(cbma_wlag1, cbma_wlag2) %>% 
#     dplyr::filter(date >="2021-05-24"  & date < "2021-06-15" | # ws1 water open, closed on 2021-06-04
#                   date >= "2021-06-15" & date < "2021-06-25" | # ws1 water closed, opened on 2021-06-25
#                   date >= "2021-07-07" & date < "2021-07-19" | # ws1 water open, closed on 2021-07-19
#                   date >= "2021-07-26" & date < "2021-08-02" | # ws1 water closed, open on 2021-08-02
#                   date >= "2021-08-08" & date < "2021-08-16") # ws1 water open, end of experiment on 2021-08-15

# Lag by week
cbmawl = rbind(cbma_wlag1, cbma_wlag2) %>% 
  dplyr::filter(date >="2021-05-28"  & date < "2021-06-04" | # ws1 water open, closed on 2021-06-04
                  date >= "2021-06-18" & date < "2021-06-25" | # ws1 water closed, opened on 2021-06-25
                  date >= "2021-07-12" & date < "2021-07-19" | # ws1 water open, closed on 2021-07-19
                  date >= "2021-07-27" & date < "2021-08-02" | # ws1 water closed, open on 2021-08-02
                  date >= "2021-08-09" & date < "2021-08-16") # ws1 water open, end of experiment on 2021-08-15

# cbma_waterlagpca = prcomp(cbmawl[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
# 
# cbma_waterpcadf = as.data.frame(cbma_waterlagpca[["x"]])
# 
# ggbiplot(cbma_waterlagpca, choices = c(1,2),ellipse = TRUE, alpha = 0) # pc2 needs to be multiplied by -1
# ggbiplot(cbma_waterlagpca, choices = c(1,3),ellipse = TRUE, alpha = 0) # Plot PCs

### PC1: ADI, AEI, ACI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity higher positive values = higher num vocals and species diversity (after running line 699)
### PC3: BIO, higher values = higher BIO

# cbmawl$pc1 = cbma_waterpcadf$PC1 # Higher ADI increases with positive values already
# cbmawl$pc2 = cbma_waterpcadf$PC2 * -1 # switching direction of Num Vocals/Species Diversity so that it is positive
# cbmawl$pc3 = cbma_waterpcadf$PC3 # Higher ACI and BIO with higher positive values

# Water Supplementation - CBMA - Date and MAS - Statistical Analysis - Lag ----------------------

cbma_maslag = cbmawl %>%
  mutate(date = date(date_time),
         ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, arid_withinf, date, mas_bin) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))


# PC1: ADI, AEI, positive  values more likely to have higher ADI
cbma_lag_pc1 = cbma_water_contrasts2(data = cbma_maslag,
                                      pc = cbma_maslag$pc1); cbma_lag_pc1

# cbma_lag_pc1[[5]] %>% gtsave("results/cbma_water_pc1_lag.png")
# plot(cbma_lag_pc1[[4]])

# PC2: Num vocals and species diversity
cbma_lag_pc2 = cbma_water_contrasts2(data = cbma_maslag,
                                      pc = cbma_maslag$pc2); cbma_lag_pc2
# cbma_lag_pc2[[5]] %>% gtsave("results/cbma_water_pc2_lag.png")
# plot(cbma_lag_pc2[[4]])

# PC3: ACI and BIO
cbma_lag_pc3 = cbma_water_contrasts2(data = cbma_maslag,
                                      pc = cbma_maslag$pc3); cbma_lag_pc3
# cbma_lag_pc3[[5]] %>% gtsave(paste0("results/cbma_water_pc3_lag.png"))
# plot(cbma_lag_pc3[[4]])

# Combining all CBMA Mas-binned data into one table
# cbma_pc_table = cbma_water_table2(cbma_lag_pc1[[3]],
#                                   cbma_lag_pc2[[3]],
#                                   cbma_lag_pc3[[3]]);cbma_pc_table
cbma_pc_table = cbma_water_table3(cbma_lag_pc1[[4]],
                                  cbma_lag_pc2[[4]],
                                  cbma_lag_pc3[[4]]);cbma_pc_table
cbma_pc_table %>% gtsave("results/cbma_water_allpcs_lag.png",
                          expand = 100,
                          vwidth = 20000, 
                          vheight = 15000)

### Plotting CBMA Water Supp Lag data
### Creating Labels for Graphs
# mas labels
cbma_maslag$mas_labels = factor(cbma_maslag$mas_bin, levels = c("0","1","2","3"),
                                labels = c("Predawn","Early","Mid","Late"))

# site labels
cbma_maslag$wssite_labels = factor(cbma_maslag$ws_site, levels = c("1","2"),
                                   labels = c("Water Site 1", "Water Site 2"))
cbma_water_site_paper(cbma_maslag,
                      cbma_maslag$pc1,
                      cbma_maslag$gh,
                      "PC1 - Acoustic Diversity",
                      "Evaporation Rate (kg of water/h)")

cbma_water_site_paper(cbma_maslag,
                      cbma_maslag$pc2,
                      cbma_maslag$gh,
                      "PC2 - Acoustic Diversity",
                      "Evaporation Rate (kg of water/h)")

cbma_water_site_paper(cbma_maslag,
                      cbma_maslag$pc3,
                      cbma_maslag$gh,
                      "PC3 - Acoustic Complexity",
                      "Evaporation Rate (kg of water/h)")
