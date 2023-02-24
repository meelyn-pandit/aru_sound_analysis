# ECE - Impact - MCP LMM Piecewise -------------------------------------
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


# Aridity Gradient - Date and MAS - Statistical Analysis - LINEAR Mixed MODELs REGRESSION ONLY -----------------------------------------
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

ggplot(data = aw4,
       aes(x = gh, y = pc1, color = site)) +
  geom_smooth(method = lm) +
  # geom_point() +
  facet_wrap(~mas_bin)

m1_lmm = lmer(pc1 ~ arid_across + (arid_across|site/mas_bin), 
              data = aw4, REML = FALSE,
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))

summary(m1_lmm)
lattice::dotplot(lme4::ranef(m1))

broom.mixed::tidy(m1_lmm, effects = "ran_coefs", conf.int = TRUE) %>% print(n = 100) #fixed + random effects
broom.mixed::tidy(m1_lmm, effects = "fixed", conf.int = TRUE) %>% print(n = 100) #fixed effects
broom.mixed::tidy(m1_lmm, effects = "ran_vals", conf.int = TRUE) %>% print(n = 100)# random effects intercepts and slopes
broom.mixed::tidy(m1_lmm, effects = "ran_pars", conf.int = TRUE) %>% print(n = 100)

m2_lmm = lmer(pc2 ~ arid_across + (arid_across|site/mas_bin), 
              data = aw4, REML = FALSE,
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
m2_lmm = lmer(pc2 ~ arid_across*scale(date_time) + (1|site), 
              data = aw4, REML = FALSE,
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))

summary(m2_lmm)

broom.mixed::tidy(m2_lmm, effects = "ran_coefs", conf.int = TRUE) %>% print(n = 100) #fixed + random effects
broom.mixed::tidy(m2_lmm, effects = "fixed", conf.int = TRUE) %>% print(n = 100) #fixed effects
broom.mixed::tidy(m2_lmm, effects = "ran_vals", conf.int = TRUE) %>% print(n = 100)# random effects intercepts and slopes
broom.mixed::tidy(m2_lmm, effects = "ran_pars", conf.int = TRUE) %>% print(n = 100)

m3_lmm = lmer(pc3 ~ arid_across + (arid_across|site/mas_bin), 
              data = aw4, REML = FALSE,
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))

summary(m3_lmm)

broom.mixed::tidy(m3_lmm, effects = "ran_coefs", conf.int = TRUE) %>% print(n = 100) #fixed + random effects
broom.mixed::tidy(m3_lmm, effects = "fixed", conf.int = TRUE) %>% print(n = 100) #fixed effects
broom.mixed::tidy(m3_lmm, effects = "ran_vals", conf.int = TRUE) %>% print(n = 100)# random effects intercepts and slopes
broom.mixed::tidy(m3_lmm, effects = "ran_pars", conf.int = TRUE) %>% print(n = 100)
# Making a dot and whisker plot with data from ran_vals
m1_re = broom.mixed::tidy(m1, effects = "ran_vals", conf.int = TRUE)
ggplot(data = m1_re,
       aes(x = estimate, y = level, 
           color = term, group = term)) +
  geom_errorbar(aes(xmax = conf.high, xmin = conf.low, 
                    width = 0)) +
  geom_point() +
  facet_wrap(~term)

## PC2 - Linear mixed model Regression with full dataset
ggplot(data = aw4,
       aes(x = gh, y = pc2, color = site)) +
  geom_smooth(method = lm)

m2 = lmer(pc2 ~ gh + scale(date_time) + (gh|site), 
          data = aw4, REML = FALSE,
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))

summary(m2)
lattice::dotplot(lme4::ranef(m2))

broom.mixed::tidy(m2, effects = "ran_coefs", conf.int = TRUE) #fixed + random effects
broom.mixed::tidy(m2, effects = "fixed", conf.int = TRUE) #fixed effects
broom.mixed::tidy(m2, effects = "ran_vals", conf.int = TRUE) # random effects intercepts and slopes
broom.mixed::tidy(m2, effects = "ran_pars", conf.int = TRUE)

# Making a dot and whisker plot with data from ran_vals
m2_re = broom.mixed::tidy(m2, effects = "ran_vals", conf.int = TRUE)
ggplot(data = m2_re,
       aes(x = estimate, y = level, 
           color = term, group = term)) +
  geom_errorbar(aes(xmax = conf.high, xmin = conf.low, 
                    width = 0)) +
  geom_point() +
  facet_wrap(~term)

m3 = lmer(pc3 ~ gh + scale(date_time) + (gh|site), 
          data = aw4, REML = FALSE,
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))

summary(m3)
lattice::dotplot(lme4::ranef(m3))

broom.mixed::tidy(m3, effects = "ran_coefs", conf.int = TRUE) #fixed + random effects
broom.mixed::tidy(m3, effects = "fixed", conf.int = TRUE) #fixed effects
broom.mixed::tidy(m3, effects = "ran_vals", conf.int = TRUE) # random effects intercepts and slopes
broom.mixed::tidy(m3, effects = "ran_pars", conf.int = TRUE)

# Making a dot and whisker plot with data from ran_vals
m3_re = broom.mixed::tidy(m3, effects = "ran_vals", conf.int = TRUE)
ggplot(data = m3_re,
       aes(x = estimate, y = level, 
           color = term, group = term)) +
  geom_errorbar(aes(xmax = conf.high, xmin = conf.low, 
                    width = 0)) +
  geom_point() +
  facet_wrap(~term)




# Aridity Gradient - Dot Plots - Datetime --------------------------------------

cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs

dt_graphs = aw6 %>%
  group_by(site, date_time, arid_withinf) %>%
  dplyr::summarise(pc1_mean = mean(pc1),
                   pc1_se = (sd(pc1))/sqrt(n()),
                   pc2_mean = mean(pc2),
                   pc2_se = (sd(pc2))/sqrt(n()),
                   pc3_mean = mean(pc3),
                   pc3_se = (sd(pc3))/sqrt(n()))

ggplot(data = dt_graphs,
       aes(x=arid_withinf, y=pc1_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  # ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
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
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right") +
  facet_grid(vars(mas_bin)) + 
  theme(strip.text.y = element_text(angle = 0))
ggsave('results/arid_pc1_dt.png', dpi = 600, height = 11, width = 8, units = "in")

### PC2 - Num vocals and Species Diversity
ggplot(data = dt_graphs,
       aes(x=arid_withinf, y=pc2_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  # ggtitle("Datetime Summarized - PC2 - Avian Vocal Abundance")+
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
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right") +
  facet_grid(vars(mas_bin)) + 
  theme(strip.text.y = element_text(angle = 0))
ggsave('results/arid_pc2_dt.png', dpi = 600, height = 11, width = 8, units = "in")

### PC3 - ACI and BIO
ggplot(data = dt_graphs,
       aes(x=arid_withinf, y=pc3_mean, color = site)) +
  geom_point(position = position_dodge(0))+
  # ggtitle("Datetime Summarized - PC3 - Acoustic Complexity")+
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
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "right")+
  facet_grid(vars(mas_bin)) + 
  theme(strip.text.y = element_text(angle = 0))
ggsave('results/arid_pc3_dt.png', dpi = 600, height = 11, width = 8, units = "in")


# Water Supp - SSWMA - Datetime - Statistical Analyses  - Pairwise -----------------

sswma_watermas = sswma_water %>%
  mutate(date = date(date_time)) %>%
  group_by(site, ws_site, water, arid_withinf, date, mas_bin) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))


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


# Water Supp - SSWMA - Datetime - Stats Analysis - Lag -----------------

sswma_dtlag = sswmawl %>%
  mutate(ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, arid_withinf, date_time) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))


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


