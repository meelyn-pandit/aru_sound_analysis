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

### Install ggbiplot ###
library(devtools)
install_github("vqv/ggbiplot")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
library(ggbiplot)

load("audio_and_weather_data.Rdata")

# Create PCA of Audio Variables, filter out files with NA ACI and  --------

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
ggbiplot(audio_pca, choices = c(1,2),ellipse = TRUE, alpha = 0, groups = aw4$site) # Plot PCs

#3D pCA Plot
pca3d(audio_pca, biplot = true)
snapshotPCA3d("audio_pca.png")

### PC1: ADI and AEI 
### PC2: Num Vocals and Species Diversity
### PC3: ACI and BIO

aw4$pc1 = audio_pcadf$PC1*-1 # Multiply PC1 by -1 to make adi diversity have positive values
aw4$pc2 = audio_pcadf$PC2 
aw4$pc3 = audio_pcadf$PC3

# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lmer(pc1 ~ site*arid_within + scale(date_time) + (1|site), data = aw4)
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



# Plot Principal Componenets ----------------------------------------------

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

ggbiplot(audio_pca2, choices = c(2,3),ellipse = TRUE, alpha = 0, groups = aw5$site) # Plot PCs
#3D Plot of PCAs
pca3d(audio_pca2, biplot = true)
snapshotPCA3d("audio_pca_datetime.png")

summary(audio_pca2) #PC1 and PC2 have highest proportion of variance
audio_pcadf2 = as.data.frame(audio_pca2[["x"]]) # Creating dataframe of PCA variance table
ggbiplot(audio_pca2, ellipse = TRUE, alpha = 0, groups = aw5$site) #Plotting PCAs to see directions
# Displaying PCAs 1 and 3
ggbiplot(audio_pca2, choices=c(1,2,3),ellipse = TRUE, alpha = 0, groups = aw5$site) #Plotting PCAs to see directions


aw5$pc1 = audio_pcadf2$PC1 # Higher PC1 leads to higher ADI i.e. acoustic diversity
aw5$pc2 = audio_pcadf2$PC2 # Higher PC2 leads to higher num_vocals and species_diversity

## Statistical Analysis
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

# Make Good Statistics Tables

library(tidyverse)
library(kable)
library(kableExtra)

kbl(m1_across_sites, caption = "Aridity Gradient\nSummarized by Datetime\nAcross Sites\nAcoustic Diversity") %>%
  kable_classic(full_width = FALSE, html_font = "Calibri")

kbl(m1_within_sites, caption = "Aridity Gradient\nSummarized by Datetime\nWithin Sites\nAcoustic Diversity") %>%
  kable_classic(full_width = F, html_font = "Calibri")

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

# Good Tables in Knitr
kbl(m2_across_sites, caption = "Aridity Gradient\nSummarized by Datetime\nAcross Sites\nNum Vocals") %>%
  kable_classic(full_width = FALSE, html_font = "Calibri")

kbl(m2_within_sites, caption = "Aridity Gradient\nSummarized by Datetime\nWithin Sites\nNum Vocals") %>%
  kable_classic(full_width = F, html_font = "Calibri")



# Aridity Gradient - Summarized by Date and MAS ---------------------------

setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/data_clean")
load("audio_and_weather_data.R")

## Create PCA of Audio Variables, filter out files with NA ACI and greater than 3000
### Summarize by site
aw6 = aw3 %>%
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
            mean_aridwithin = factor(round(mean(as.numeric(arid_within))), levels = c(1,2,3,4,5)),
            mean_aridacross = as.factor(round(mean(as.numeric(arid_across)))),
            mean_histwithin = as.factor(round(mean(as.numeric(hist_within)))),
            mean_histacross = as.factor(round(mean(as.numeric(hist_across)))))

audio_pca3 = prcomp(aw6[,c(5:10)], center = TRUE, scale. = TRUE)
summary(audio_pca3) #PC1 and PC2 have highest proportion of variance

audio_pcadf3 = as.data.frame(audio_pca3[["x"]])

ggbiplot(audio_pca3, choices = c(2,3),ellipse = TRUE, alpha = 0, groups = aw6$site) # Plot PCs
#3D Plot of PCAs
pca3d(audio_pca3, biplot = true)
snapshotPCA3d("audio_pca_datetime.png")
aw6$pc1 = audio_pcadf3$PC1*-1 # multiplied by -1 to reverse direction of PC1
aw6$pc2 = audio_pcadf3$PC2*-1 # multiplied by -1 to reverse direction of PC2, higher PC2 values indicate higher ACI, BIO, num vocals, and species diversity

# Aridity Gradient - Date and MAS - Statistical Analysis ------------------
# PC1: ACI, ADI, AEI, positive values more likely to have higher ADI 
# (after being multiplied by -1)
m1 = lmer(pc1 ~ site*mas_bin + scale(date) + (1|site), data = aw6)
# m1 = lm(pc1 ~ site*mas_bin + scale(date), data = aw6)
summary(m1)
assump(m1)
Anova(m1)
pairs(emmeans(m1, ~site|mas_bin, data = aw6)) # across site comparisions
pairs(emmeans(m1, ~mas_bin|site, data = aw6)) # within site comparisons
kbl(mas_across_sites, caption = "Aridity Gradient\nSummarized by Date and MAS\nAcross Sites\nAcoustic Diversity") %>%
  kable_classic(full_width = FALSE, html_font = "Calibri")

kbl(mas_within_sites, caption = "Aridity Gradient\nSummarized by Date and MAS\nWithin Sites\nAcoustic Diversity") %>%
  kable_classic(full_width = F, html_font = "Calibri")

# PC2: ACI, BIO, Vocalization Number, Species Diversity higher with positive values
# (after being multiplied by -1)
m2 = lm(pc2 ~ site*mas_bin + scale(date), data = aw6)
summary(m2)
assump(m2)
Anova(m2)
pairs(emmeans(m2, ~site|mas_bin, data = aw6)) #across site comparisons
pairs(emmeans(m2, ~mas_bin|site, data = aw6)) # within site comparisons


# Aridity Gradient - Dot Plots - Datetime - PC1 --------------------------------------

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs

dt_graphs = aw5 %>%
  group_by(site, mean_aridwithin) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
            pc1_se = (sd(pc1))/sqrt(n()),
            pc2_mean = mean(pc2),
            pc2_se = (sd(pc2))/sqrt(n()))

ggplot(data = dt_graphs,
       aes(x=mean_aridwithin, y=pc1_mean, color = site)) +
  geom_point(position = position_dodge(0.5))+
  ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = pc1_mean-pc1_se, 
                    ymax = pc1_mean+pc1_se), width = 0.2,
                position = position_dodge(0.5))+
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

### Datetime Graphs - PC2 - Num Vocals and Species Diversity
cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs

dt_graphs = aw5 %>%
  group_by(site, mean_aridwithin) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
                   pc1_se = (sd(pc1))/sqrt(n()),
                   pc2_mean = mean(pc2),
                   pc2_se = (sd(pc2))/sqrt(n()))

ggplot(data = dt_graphs,
       aes(x=mean_aridwithin, y=pc2_mean, color = site)) +
  geom_point(position = position_dodge(0.5))+
  ggtitle("Datetime Summarized - PC2 - Acoustic Diversity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = pc2_mean-pc2_se, 
                    ymax = pc2_mean+pc2_se), width = 0.2,
                position = position_dodge(0.5))+
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


# Aridity Graident - Dot Plots - Date and MAS -----------------------------

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs

mas_graphs = aw6 %>%
  group_by(site, mean_aridwithin, mas_bin) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
                   pc1_se = (sd(pc1))/sqrt(n()),
                   pc2_mean = mean(pc2),
                   pc2_se = (sd(pc2))/sqrt(n()))

ggplot(data = mas_graphs,
       aes(x=mean_aridwithin, y=pc1_mean, color = site)) +
  geom_point(position = position_dodge(0.5))+
  ggtitle("Date and MAS Summarized - PC1 - Acoustic Diversity")+
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0.5))+
  facet_wrap(~ mas_bin) +
  geom_errorbar(aes(ymin = pc1_mean-pc1_se, 
                    ymax = pc1_mean+pc1_se), width = 0.2,
                position = position_dodge(0.5))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", 
                   labels = c("Extremely Humid","Humid","Normal","Arid", "Extremely Arid"))+
  scale_y_continuous(name = "PC1 - Acoustic Diversity")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")

mas_graphs = aw6 %>%
  group_by(site, mas_bin) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
                   pc1_se = (sd(pc1))/sqrt(n()),
                   pc2_mean = mean(pc2),
                   pc2_se = (sd(pc2))/sqrt(n()))

### PC2 - Num Vocals and Species Diversity - Date an MAS summarized
ggplot(data = mas_graphs,
       aes(x=mean_aridwithin, y=pc2_mean, color = site)) +
  geom_point(position = position_dodge(0.5))+
  ggtitle("Date and MAS Summarized - PC2 - Num. Vocals and Species Diversity")+
  facet_wrap(~ mas_bin) +
  geom_line(aes(group = site, 
                color = site),
            position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = pc2_mean-pc2_se, 
                    ymax = pc2_mean+pc2_se), width = 0.2,
                position = position_dodge(0.5))+
  scale_color_manual(values = cbpalette, 
                     name = "Site",
                     labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
  scale_x_discrete(name = "Aridity - Normalized Within", 
                   labels = c("Predawn","Early","Mid","Late"))+
  scale_y_continuous(name = "PC2 - Num. Vocals and Species Diversity")+
  # facet_grid(~facet_type) +
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")
