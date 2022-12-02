library(dplyr) #data manipulation
library(tibble) #data manipulation
library(lubridate) #manipulating date and time
library(hms) #manipulate time
library(zoo) #for na.approx to approximate missing values in weather dataset
library(ggplot2) #graphs
library(gridExtra) #ggplot multi panels
library(cowplot)
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
library(pca3d)
library(gt)
library(htmltools)
library(webshot2)

### Install ggbiplot ###
library(devtools)
install_github("vqv/ggbiplot")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
library(ggbiplot)

load("audio_and_weather_data.Rdata")

# Aridity Gradient - Create PCA of Audio Variables, filter out files with NA ACI and  --------

aw_bad = aw2 %>%
  dplyr::filter(as_date(date_time) == "2021-07-09" & site == "lwma" & aru == "aru04") 
aw_bad2 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-07-10" & site == "lwma" & aru == "aru04")
aw_bad3 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-28" & site == "kiowa" & aru == "aru05") 
aw_bad4 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-29" & site == "kiowa" & aru == "aru05")
aw_bad5 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-30" & site == "kiowa" & aru == "aru05")
aw_bad6 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-05-28" & site == "cbma" & aru == "aru03")

aw_bad_total = rbind(aw_bad,aw_bad2,aw_bad3,aw_bad4,aw_bad5,aw_bad6)
aw3 = setdiff(aw2, aw_bad_total)
aw3$site = factor(aw3$site, levels = c("lwma","sswma","cbma","kiowa"))
aw3 = aw3 %>% dplyr::filter(is.na(mas_bin) == FALSE)

aw4 = aw3 %>%
  dplyr::filter(is.na(aci) == FALSE) %>%
  # dplyr::filter(aci < 3000) %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16")

aw4$site = factor(aw4$site, levels = c("lwma","sswma","cbma","kiowa"))

audio_pca = prcomp(aw4[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
summary(audio_pca) #PC1 and PC2 have highest proportion of variance
audio_pcadf = as.data.frame(audio_pca[["x"]])
ggbiplot(audio_pca, choices = c(1,3),ellipse = TRUE, alpha = 0, groups = aw4$site) # Plot PCs

#3D pCA Plot
pca3d(audio_pca, biplot = true) # only run this on windows machine
snapshotPCA3d("audio_pca.png")

### PC1: ADI and AEI, higher values mean higher diversity (after running line 65)
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI

aw4$pc1 = audio_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
aw4$pc2 = audio_pcadf$PC2 
aw4$pc3 = audio_pcadf$PC3

# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lmer(pc1 ~ site*arid_within + mas_bin + scale(date_time) + (1|site), data = aw4)
summary(m1)
assump(m1)
# emm_options(pbkrtest.limit = 54931) # run this R will crash
emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons

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
load("audio_and_weather_data.R")

## Create PCA of Audio Variables, filter out files with NA ACI and greater than 3000
### Summarize by site
aw6 = aw4 %>%
  dplyr::filter(year(date_time)==2021) %>%
  dplyr::filter(as_date(date_time) < "2021-08-16") %>%
  mutate(date = as_date(date_time)) %>%
  group_by(site, date, mas_bin) %>%
  dplyr::summarise(n = n(),
                   aci_mean = mean(aci, na.rm = TRUE),
                   bio_mean = mean(bio, na.rm = TRUE),
                   adi_mean = mean(adi, na.rm = TRUE),
                   aei_mean = mean(aei, na.rm = TRUE),
                   vocals_mean = mean(num_vocals),
                   species_mean = mean(species_diversity),
                   pc1_mean = mean(pc1),
                   pc2_mean = mean(pc2),
                   pc3_mean = mean(pc3),
            mean_aridwithin = factor(round(mean(as.numeric(arid_within))), levels = c(1,2,3,4,5)),
            mean_aridacross = as.factor(round(mean(as.numeric(arid_across)))),
            mean_histwithin = as.factor(round(mean(as.numeric(hist_within)))),
            mean_histacross = as.factor(round(mean(as.numeric(hist_across)))))

# audio_pca3 = prcomp(aw6[,c(5:10)], center = TRUE, scale. = TRUE)
# summary(audio_pca3) #PC1 and PC2 have highest proportion of variance
# audio_pcadf3 = as.data.frame(audio_pca3[["x"]])
# ggbiplot(audio_pca3, choices = c(2,3),ellipse = TRUE, alpha = 0, groups = aw6$site) # Plot PCs
# #3D Plot of PCAs
# pca3d(audio_pca3, biplot = true)
# snapshotPCA3d("audio_pca_datetime.png")
# aw6$pc1 = audio_pcadf3$PC1*-1 # multiplied by -1 to reverse direction of PC1
# aw6$pc2 = audio_pcadf3$PC2*-1 # multiplied by -1 to reverse direction of PC2, higher PC2 values indicate higher ACI, BIO, num vocals, and species diversity

# Aridity Gradient - Date and MAS - Statistical Analysis ------------------
# PC1: ADI, AEI, positive values more likely to have higher ADI 
# (after being multiplied by -1)
m1 = lm(pc1_mean ~ site*mas_bin + scale(date), data = aw6)
# m1 = lm(pc1 ~ site*mas_bin + scale(date), data = aw6)
summary(m1)
assump(m1)
Anova(m1)
pairs(emmeans(m1, ~site|mas_bin, data = aw6)) # across site comparisions
pairs(emmeans(m1, ~mas_bin|site, data = aw6)) # within site comparisons


# PC2: Vocalization Number, Species Diversity higher with positive values
# (after being multiplied by -1)
m2 = lm(pc2_mean ~ site*mas_bin + scale(date), data = aw6)
summary(m2)
assump(m2)
Anova(m2)
pairs(emmeans(m2, ~site|mas_bin, data = aw6)) #across site comparisons
pairs(emmeans(m2, ~mas_bin|site, data = aw6)) # within site comparisons

# PC3: ACI, BIO higher positive values have higher ACI and lower BIO
# (after being multiplied by -1)
m3 = lm(pc2_mean ~ site*mas_bin + scale(date), data = aw6)
summary(m3)
assump(m3)
Anova(m3)
pairs(emmeans(m3, ~site|mas_bin, data = aw6)) #across site comparisons
pairs(emmeans(m3, ~mas_bin|site, data = aw6)) # within site comparisons

# Aridity Gradient - Dot Plots - Datetime - PC1 --------------------------------------

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs

dt_graphs = aw4 %>%
  group_by(site, arid_within) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
                   pc1_se = (sd(pc1))/sqrt(n()),
                   pc2_mean = mean(pc2),
                   pc2_se = (sd(pc2))/sqrt(n()),
                   pc3_mean = mean(pc3),
                   pc3_se = (sd(pc3))/sqrt(n()))

ggplot(data = dt_graphs,
       aes(x=arid_within, y=pc1_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc1_mean-pc1_se, 
                    ymax = pc1_mean+pc1_se), width = 0.2,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_y_continuous(name = "PC1 - Evenness to Diversity")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")

### PC2 - Num vocals and Species Diversity
ggplot(data = dt_graphs,
       aes(x=arid_within, y=pc2_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  ggtitle("Datetime Summarized - PC2 - Avian Vocal Abundance")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc2_mean-pc2_se, 
                    ymax = pc2_mean+pc2_se), width = 0.2,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_y_continuous(name = "PC2 - Num. Vocals and Species Diversity")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")

### PC3 - ACI and BIO
ggplot(data = dt_graphs,
       aes(x=arid_within, y=pc3_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  ggtitle("Datetime Summarized - PC3 - Acoustic Complexity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc3_mean-pc3_se, 
                    ymax = pc3_mean+pc3_se), width = 0.2,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_y_continuous(name = "PC3 - Simple to Complex")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")

# Aridity Graident - Dot Plots - Date and MAS -----------------------------

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs

mas_graphs = aw4 %>%
  group_by(site, arid_within, mas_bin) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
                   pc1_se = (sd(pc1))/sqrt(n()),
                   pc2_mean = mean(pc2),
                   pc2_se = (sd(pc2))/sqrt(n()),
                   pc3_mean = mean(pc3),
                   pc3_se = (sd(pc3))/sqrt(n()))

ggplot(data = mas_graphs,
       aes(x=arid_within, y=pc1_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  ggtitle("Date and MAS Summarized - PC1 - Acoustic Diversity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  facet_wrap(~ mas_bin) +
  geom_errorbar(aes(ymin = pc1_mean-pc1_se, 
                    ymax = pc1_mean+pc1_se), width = 0.2,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_y_continuous(name = "PC1 - Acoustic Diversity")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")

### PC2 - Num Vocals and Species Diversity - Date an MAS summarized
ggplot(data = mas_graphs,
       aes(x=arid_within, y=pc2_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  ggtitle("Date and MAS Summarized - PC2 - Num. Vocals and Species Diversity")+
  facet_wrap(~ mas_bin) +
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc2_mean-pc2_se, 
                    ymax = pc2_mean+pc2_se), width = 0.2,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_y_continuous(name = "PC2 - Num. Vocals and Species Diversity")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")

### PC3 - Num Vocals and Species Diversity - Date an MAS summarized
ggplot(data = mas_graphs,
       aes(x=arid_within, y=pc3_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  ggtitle("Date and MAS Summarized - PC3 - Acoustic Complexity")+
  facet_wrap(~ mas_bin) +
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0))+
  geom_errorbar(aes(ymin = pc3_mean-pc3_se, 
                    ymax = pc3_mean+pc3_se), width = 0.2,
                position = position_dodge(0))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid"))+
  scale_y_continuous(name = "PC3 - Simple to Complex")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")



# Water Supplementation - Load Data ---------------------------------------

load("water_audio_and_weather_data.Rdata")

ww = water_weather3 %>%
  dplyr::filter(date_time < "2021-08-16") %>%
  dplyr::filter(year(date_time) == 2021) %>%
  dplyr::filter(aci < 3000) # check audio files to see if they actually need to be filtered and if certain dates have damaged files

# Water Supplementation - Create PCA of Audio Variables, filter out files with NA ACI and  --------

# aw_bad = aw2 %>%
#   dplyr::filter(as_date(date_time) == "2021-07-09" & site == "lwma" & aru == "aru04") 
# aw_bad2 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-07-10" & site == "lwma" & aru == "aru04")
# aw_bad3 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-28" & site == "kiowa" & aru == "aru05") 
# aw_bad4 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-29" & site == "kiowa" & aru == "aru05")
# aw_bad5 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-06-30" & site == "kiowa" & aru == "aru05")
# aw_bad6 = aw2 %>% dplyr::filter(as_date(date_time) == "2021-05-28" & site == "cbma" & aru == "aru03")
# 
# aw_bad_total = rbind(aw_bad,aw_bad2,aw_bad3,aw_bad4,aw_bad5,aw_bad6)
# aw3 = setdiff(aw2, aw_bad_total)
ww$site = factor(ww$site, levels = c("lwma","sswma","cbma","kiowa"))
ww2 = ww %>% dplyr::filter(is.na(mas_bin) == FALSE)

ww3 = ww2 %>%
  dplyr::filter(is.na(aci) == FALSE)

water_pca = prcomp(ww3[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)
summary(water_pca) #PC1 and PC2 have highest proportion of variance
water_pcadf = as.data.frame(water_pca[["x"]])
ggbiplot(water_pca, choices = c(1,3),ellipse = TRUE, alpha = 0, groups = ww3$site) # Plot PCs

#3D pCA Plot
pca3d(water_pca, biplot = true) # only run this on windows machine
snapshotPCA3d("water_pca.png")

### PC1: ADI and AEI, higher values mean higher diversity (after running line 65)
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and higher BIO

ww3$pc1 = water_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
ww3$pc2 = water_pcadf$PC2 
ww3$pc3 = water_pcadf$PC3*-1


# SSWMA Water Supp - Statisical Analysis - Pairwise ------------------------------------

sswma_water = ww3 %>%
  dplyr::filter(site == "sswma") %>%
  mutate(ws_site = as.factor(ws_site),
         water = as.factor(water),
         date = date(date_time),
         week = week(date_time)) %>%
  arrange(date_time,ws_site,water)

### Convert SSWMA df from long to wide

# sswater_wide = sswma_water %>% 
#   dplyr::select(date_time,ws_site,water,pc1,pc2,pc3) %>%
#   pivot_wider(names_from = c(ws_site, water), 
#               values_from = c(pc1,pc2,pc3),
#               values_fn = mean)
# 
# t.test(sswater_wide$pc1_1_0, sswater_wide$pc1_2_1, paired = TRUE)

# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lmer(pc1 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m1)
assump(m1)

emmeans(m1, pairwise ~ ws_site:water|arid_within)


# PC2: Num vocals and species diversity
m2 = lmer(pc2 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m2)
assump(m2)
emmeans(m2, pairwise ~ ws_site:water|arid_within)

emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons


# PC3: ACI and BIO
m3 = lmer(pc3 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m3)
assump(m3)
emmeans(m3, pairwise ~ ws_site:water|arid_within)

emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons


# Water Supplementation - Datetime - Statistical Analyses  - Pairwise -----------------

sswma_watermas = sswma_water %>%
  mutate(date = date(date_time)) %>%
  group_by(site, ws_site, water, arid_within, date, mas_bin) %>%
  summarise_at(c("pc1","pc2","pc3"), mean) 


# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lmer(pc1 ~ ws_site*water*arid_within + date + (1|ws_site), data = sswma_watermas)
summary(m1)
assump(m1)

emmeans(m1, pairwise ~ ws_site:water|arid_within)
emm_options(pbkrtest.limit = 3000) # run this R will crash
emm_options(lmerTest.limit = 11778) # set lmerTest limit so you can do the within site comparisons


# PC2: Num vocals and species diversity
m2 = lmer(pc2 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m2)
assump(m2)
emmeans(m2, pairwise ~ ws_site:water|arid_within)

emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons


# PC3: ACI and BIO
m3 = lmer(pc3 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m3)
assump(m3)
emmeans(m3, pairwise ~ ws_site:water|arid_within)

emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons


# Water Supplementation - SSWMA - Date and MAS - Statistical Analysis - Pairwise-------------
sswma_watermas = sswma_water %>%
  mutate(date = date(date_time)) %>%
  group_by(site, ws_site, water, arid_within, date, mas_bin) %>%
  summarise_at(c("pc1","pc2","pc3"), mean) 


# PC1: ADI, AEI, positive  values more likely to have higher ADI
sswma_pairwise_pc1 = sswma_water_contrasts(data = sswma_watermas,
                                           pc = sswma_watermas$pc1); sswma_pairwise_pc1
sswma_pairwise_pc1[[5]] %>% gtsave("sswma_water_pc1_pairwise.png")
plot(sswma_pairwise_pc1[[4]])

# PC2: Num vocals and species diversity
sswma_pairwise_pc2 = sswma_water_contrasts(data = sswma_watermas,
                                           pc = sswma_watermas$pc2); sswma_pairwise_pc2
sswma_pairwise_pc2[[5]] %>% gtsave("sswma_water_pc2_pairwise.png")
plot(sswma_pairwise_pc2[[4]])

# PC3: ACI and BIO
sswma_pairwise_pc3 = sswma_water_contrasts(data = sswma_watermas,
                                  pc = sswma_watermas$pc3); sswma_pairwise_pc3
sswma_pairwise_pc3[[5]] %>% gtsave(paste0("sswma_water_pc3_pairwise.png"))
plot(sswma_pairwise_pc3[[4]])

# Water Supplementation - SSWMA - Data Organization - Lag Analysis --------
### Only analyzing half of water supplementation period
load("raw_water_audio_weather.Rdata")

### SSWMA
#Separating out SSWMA water sites
sswma_wlag1 = water_weather2 %>%
  filter(aru == "ws01"| aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05")%>%
  mutate(water = ifelse(date(date_time) >= "2021-05-23" & date(date_time) <"2021-05-30"| date(date_time) >= "2021-06-22" & date(date_time) < "2021-07-02", 1,0),
         ws_site = 1)

sswma_wlag2 = water_weather2 %>%
  filter(aru == "ws06"| aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10") %>%
  mutate(water = ifelse(date(date_time) >= "2021-06-06" & date(date_time) <"2021-06-12"| date(date_time) >= "2021-07-21" & date(date_time) < "2021-08-07", 1,0),
         ws_site = 2)

sswma_wlag3 = water_weather2 %>%
  filter(aru == "ws11"| aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15") %>%
  mutate(water = 0,
         ws_site = 3)

sswma_wlag = rbind(sswma_wlag1, sswma_wlag2, sswma_wlag3)
sswmawl = sswma_wlag %>%
  dplyr::filter(date(date_time)>= "2021-05-23" & date(date_time) <"2021-05-30"| date(date_time) >= "2021-06-22" & date(date_time) < "2021-07-02" & date(date_time) >= "2021-06-06" & date(date_time) <"2021-06-12"| date(date_time) >= "2021-07-21" & date(date_time) < "2021-08-07")

sswma_waterpca = prcomp(sswmawl[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)

sswma_waterpcadf = as.data.frame(sswma_waterpca[["x"]])
ggbiplot(sswma_waterpca, choices = c(1,3),ellipse = TRUE, alpha = 0, groups = sswmawl$site) # Plot PCs

#3D pCA Plot
pca3d(sswma_waterpca, biplot = true) # only run this on windows machine
snapshotPCA3d("sswma_water_lag_pca.png")

### PC1: ADI and AEI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO, higher values = higher ACI and BIO

sswmawl$pc1 = sswma_waterpcadf$PC1 # Higher ADI increases with positive values already
sswmawl$pc2 = sswma_waterpcadf$PC2 
sswmawl$pc3 = sswma_waterpcadf$PC3


# Water Supplementation - SSWMA - Datetime - Stats Analysis - Lag -----------------

sswma_dtlag = sswmawl %>%
  mutate(ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, arid_within, date_time) %>%
  summarise_at(c("pc1","pc2","pc3"), mean) 


# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lm(pc1 ~ ws_site*water*arid_within + scale(date_time), data = sswma_dtlag)
summary(m1)
assump(m1)
###stick with lms over lmer
emmeans(m1, pairwise ~ ws_site*water|arid_within)
# emm_options(pbkrtest.limit = 3000) # run this R will crash
# emm_options(lmerTest.limit = 11778) # set lmerTest limit so you can do the within site comparisons

# PC2: Num vocals and species diversity
m2 = lm(pc2 ~ ws_site*water*arid_within + date_time, data = sswma_dtlag)
summary(m2)
assump(m2)
emmeans(m2, pairwise ~ ws_site:water|arid_within)

# PC3: ACI and BIO
m3 = lm(pc3 ~ ws_site*water*arid_within + date_time, data = sswma_dtlag)
summary(m3)
assump(m3)
emmeans(m3, pairwise ~ ws_site*water|arid_within)

# Water Supplementation - SSWMA - Date and MAS - Statistical Analysis - Lag ----------------------

sswma_maslag = sswmawl %>%
  mutate(date = date(date_time),
         ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, arid_within, date, mas_bin) %>%
  summarise_at(c("pc1","pc2","pc3"), mean) 

# PC1: ADI, AEI, positive  values more likely to have higher ADI
sswma_lag_pc1 = sswma_water_contrasts(data = sswma_maslag,
                                           pc = sswma_maslag$pc1); sswma_lag_pc1
sswma_lag_pc1[[5]] %>% gtsave("sswma_water_pc1_lag.png")
plot(sswma_lag_pc1[[4]])

# PC2: Num vocals and species diversity
sswma_lag_pc2 = sswma_water_contrasts(data = sswma_maslag,
                                           pc = sswma_maslag$pc2); sswma_lag_pc2
sswma_lag_pc2[[5]] %>% gtsave("sswma_water_pc2_lag.png")
plot(sswma_lag_pc2[[4]])

# PC3: ACI and BIO
sswma_lag_pc3 = sswma_water_contrasts(data = sswma_maslag,
                                           pc = sswma_maslag$pc3); sswma_lag_pc3
sswma_lag_pc3[[5]] %>% gtsave(paste0("sswma_water_pc3_lag.png"))
plot(sswma_lag_pc3[[4]])

# Water Supplementation - CBMA - Data Organization - Lag ------------------
### Only analyzing half of water supplementation period
load("raw_water_audio_weather.Rdata")

#Separating out CBMA water sites
cbma_wlag1 = water_weather2 %>%
  dplyr::filter(year(date_time) == 2021) %>%
  mutate(date = date(date_time)) %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date < "2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

cbma_wlag2 = water_weather2 %>%
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

cbma_waterlagpca = prcomp(cbmawl[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)

cbma_waterpcadf = as.data.frame(cbma_waterlagpca[["x"]])
ggbiplot(cbma_waterlagpca, choices = c(1,3),ellipse = TRUE, alpha = 0) # Plot PCs

#3D pCA Plot
pca3d(cbmawl, biplot = true) # only run this on windows machine
snapshotPCA3d("cbma_water_lag_pca.png")

### PC1: ADI, AEI, ACI, higher values mean higher diversity
### PC2: Num Vocals and Species Diversity higher positive values = higher num vocals and species diversity (after running line 699)
### PC3: BIO, higher values = higher BIO

cbmawl$pc1 = cbma_waterpcadf$PC1 # Higher ADI increases with positive values already
cbmawl$pc2 = cbma_waterpcadf$PC2 * -1 # switching direction of Num Vocals/Species Diversity so that it is positive
cbmawl$pc3 = cbma_waterpcadf$PC3 # Higher ACI and BIO with higher positive values

# Water Supplementation - CBMA - Date and MAS - Statistical Analysis - Lag ----------------------

cbma_maslag = cbmawl %>%
  mutate(date = date(date_time),
         ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, arid_within, date, mas_bin) %>%
  summarise_at(c("pc1","pc2","pc3"), mean) 

# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lm(pc1 ~ ws_site*water*arid_within + mas_bin + date, data = cbma_maslag)
summary(m1)
assump(m1)
###stick with lms over lmer
emm1= emmeans(m1, pairwise ~ ws_site*water|arid_within)
contrast(emm1, "trt.vs.ctrl", ref = "ws_site2 water1", exclude = 2)

# PC2: Num vocals and species diversity
m2 = lm(pc2 ~ ws_site*water*arid_within + mas_bin+ date, data = cbma_maslag)
summary(m2)
assump(m2)
emm2 = emmeans(m2, pairwise ~ ws_site:water|arid_within)
contrast(emm2, "trt.vs.ctrl", ref = "ws_site2 water1", exclude = 2)

# PC3: ACI and BIO
m3 = lm(pc3 ~ ws_site*water*arid_within + mas_bin + date, data = cbma_maslag)
summary(m3)
assump(m3)
emm3 = emmeans(m3, pairwise ~ ws_site*water|arid_within)
contrast(emm3, "trt.vs.ctrl", ref = "ws_site2 water1", exclude = 2)

library(gt)
