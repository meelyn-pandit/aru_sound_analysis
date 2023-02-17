################Stats Analysis###################
library(nlme) #AICc tests
library(lme4) #linear mixed models
library(lmerTest) #get p-values for LMMs
library(ggplot2) #graphing
library(ggthemes) #classical them for ggplot2
library(gridExtra) #???
library(nortest) #???
library(car) #Anova() function
library(dplyr) #data organization
library(emmeans) #post-hoc comparison tests
library(multcomp) #posthoc tests for ANOVA type III effects


# Loading Datasets --------------------------------------------------------
setwd("dir") #set working directory that contain the population level ABM results
conds = as.list(c("current","current_extreme","climate_change","climate_change_extreme"))

ewl = lapply(conds, function(x){
  data = load(paste0(x,"_1km_freq_ewl.Rdata"))
  ABMout_final$model = "ewl"
  ABMout_final$condition = x
  return(ABMout_final)
}) %>% bind_rows

noewl = lapply(conds, function(x){
  data = load(paste0(x,"_1km_freq_noewl.Rdata"))
  ABMout_final$model = "noewl"
  ABMout_final$condition = x
  return(ABMout_final)
}) %>% bind_rows


all_data = rbind(ewl,noewl)
all_data$timeCnt = as.numeric(all_data$timeCnt)

completions_means = all_data %>%
  group_by(iter,condition,Frequency,model,timeCnt) %>%
  summarize(n = n(),
            PercentMean = mean(percent_comp),
            PercentsE = (sd(percent_comp)/sqrt(n))
  )

# Estimates for Contact Rates ---------------------------------------------
completions_means$Frequency = factor(completions_means$Frequency, levels=c("4000","8000","12000"))
completions_means$model = factor(completions_means$model, levels = c("noewl","ewl"))
completions_means$condition = factor(completions_means$condition, levels=c("current","current_extreme","climate_change","climate_change_extreme"))
completions_means$timeCnt = as.numeric(completions_means$timeCnt)

completions_means$SHD = interaction(completions_means$condition,completions_means$Frequency,completions_means$model)
completions_means$freq_mod = interaction(completions_means$Frequency,completions_means$model)
completions_means$cond_mod = interaction(completions_means$condition,completions_means$model)


completions_means$percent2 = ifelse(completions_means$PercentMean==0,0,1)
completions_means$percent3 = log(completions_means$PercentMean+1)

# Analyze Each Frequency Independently - Only care about Same Freq --------


# model = lmer(PercentMean ~ SHD + (1|iter), data=completions_means, REML = FALSE)
# assump(model)
# summary(model)
# Anova(model, type ="III")
# contrasts = glht(model, linfct = mcp(SHD = "Tukey"))
# options(max.print = 999999)
# summary(contrasts)

m1 = lm((PercentMean*100) ~ SHD, data=completions_means)
assump(m1)
summary(m1)
Anova(m1, type ="III")
contrasts = glht(m1, linfct = mcp(SHD = "Tukey"))
summary(contrasts)
emm1 <- emmeans(m1, "SHD", type = "response")

#just use interaction post-hoc


# Graphs ------------------------------------------------------------------
graph_means = all_data %>%
  group_by(condition,Frequency,model,timeCnt) %>%
  summarize(n = n(),
            PercentMean = mean(percent_comp),
            PercentsE = (sd(percent_comp)/sqrt(n))
  )
graph_means$Frequency = factor(graph_means$Frequency, levels=c("4000","8000","12000"))
graph_means$model = factor(graph_means$model, levels = c("noewl","ewl"))
graph_means$condition = factor(graph_means$condition, levels=c("current","current_extreme","climate_change","climate_change_extreme"))
graph_means$timeCnt = as.numeric(graph_means$timeCnt)
graph_means$condition2 = recode_factor(graph_means$condition, current = "A. Mean Contemporary",
                                             current_extreme = "B. Contemporary Drought", climate_change = "C. Mean Climate Change",
                                             climate_change_extreme = "D. Climate Change Drought")


# Completion Rates - Multiple Frequencies ---------------------------------
ggplot(data = graph_means,
       aes(x=timeCnt, y=PercentMean*100,color = as.factor(Frequency))) +
  # ggtitle("climate_change_extreme") +
  geom_line(aes(linetype = model),size = 1.5) +
  # geom_smooth(method=lm,   # Add linear regression lines
  #             # se=TRUE,    # Don't add shaded confidence region
  #             fullrange=TRUE) +# Extend regression lines
  scale_linetype_manual(name = "Model",values=c("solid","dotted"),labels = c("No EWL Included","EWL Included"))+
  # scale_size_manual(values=c(2,1))+
  scale_y_continuous(name = "Completion Rate\n(Percent Contacted All Neighbors)", limits=c(0.0, 100.0))+
  scale_x_continuous(name =  "Time After Sunrise(min)")+
  scale_colour_manual(name = "Frequency", 
                      values = c("#E69F00", "#56B4E9", "#999999"), #orange, blue, gray) ,
                      labels = c("4kHz","8kHz","12kHz"))+
  facet_wrap(~condition2) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +#change angle to 0 for presentations
  theme(legend.position = "bottom")

setwd("dir")
ggsave("completions_all_conditions_paper.jpg", dpi=600, height=10, width=12, units="in")


# Worst Case Scenarios ----------------------------------------------------
setwd("dir") #set working directory to files with the "Worst Case Scenarios" ABM results

load("medium_cc_extreme_1km_8freq_ewl.Rdata")
med_ewl = ABMout_final
med_ewl$condition = "medium"
med_ewl$model = "ewl"

load("bad_cc_extreme_1.5km_8freq_ewl.Rdata")
bad_ewl = ABMout_final
bad_ewl$condition = "bad"
bad_ewl$model = "ewl"

load("worst_cc_extreme_3km_8freq_ewl.Rdata")
worst_ewl= ABMout_final
worst_ewl$condition = "worst"
worst_ewl$model = "ewl"

load("medium_cc_extreme_1km_8freq_noewl.Rdata")
med_noewl = ABMout_final
med_noewl$condition = "medium"
med_noewl$model = "no_ewl"

load("bad_cc_extreme_1.5km_8freq_noewl.Rdata")
bad_noewl = ABMout_final
bad_noewl$condition = "bad"
bad_noewl$model = "no_ewl"

load("worst_cc_extreme_3km_8freq_noewl.Rdata")
worst_noewl = ABMout_final
worst_noewl$condition = "worst"
worst_noewl$model = "no_ewl"

freqs2 = rbind(med_ewl, bad_ewl, worst_ewl,
                  med_noewl, bad_noewl, worst_noewl)
freqs2$condition = factor(freqs2$condition,
                         levels = c("medium",
                                    "bad",
                                    "worst"))
freqs2$model = factor(freqs2$model, levels = c("no_ewl","ewl"))
freqs2$timeCnt = as.numeric(freqs2$timeCnt)

worst_cases = freqs2 %>%
  group_by(iter, Frequency,timeCnt, condition, model) %>%
  # group_by(Frequency,timeCnt) %>%
  dplyr::summarize(n = n(),
            PercentMean = mean(percent_comp),
            PercentsE = (sd(percent_comp)/sqrt(n))
            # ,
            # contacts = contacts
  )

worst_cases2 = freqs2 %>%
  dplyr::group_by(condition, model) %>%
  dplyr::summarize(n = n(),
            PercentMean = mean(percent_comp),
            PercentsE = (sd(percent_comp)/sqrt(n))
  )


shd = interaction(worst_cases$condition, worst_cases$model)
w_model<-lm((PercentMean*100) ~ shd, data=worst_cases)
summary(model)
assump(model)

Anova(w_model, type ="III")
contrasts = glht(model, linfct = mcp(shd = "Tukey"))
options(max.print = 999999)
summary(contrasts)

graph_means2 = freqs2 %>%
  group_by(condition, Frequency,timeCnt, model) %>%
  # group_by(Frequency,timeCnt) %>%
  summarize(n = n(),
            PercentMean = mean(percent_comp),
            PercentsE = (sd(percent_comp)/sqrt(n)),
            contacts = mean(contacts))

graph_means2$timeCnt = as.numeric(graph_means2$timeCnt)
graph_means2$model = factor(graph_means2$model, levels=c("no_ewl","ewl"))


# Graphing Mean Contacts over Time - Medium, Bad, Worst Case Scena --------

ggplot(data = graph_means2,
       aes(x=timeCnt, y=contacts, color = as.factor(condition))) +
  # ggtitle("climate_change_extreme") +
  geom_line(size = 1.5,aes(linetype=model)) +
  # geom_smooth(method=lm,   # Add linear regression lines
  #             # se=TRUE,    # Don't add shaded confidence region
  #             fullrange=TRUE) +# Extend regression lines
  scale_linetype_manual(name = "Model",values=c("solid","dotted"), labels = c("No EWL", "EWL"))+
  scale_y_continuous(name = "Total Contacts Per Min")+
  scale_x_continuous(name =  "Time After Sunrise(min)")+
  scale_colour_manual(name = "Territory Size", 
                      values = c("#E69F00", "#56B4E9", "#999999"), #orange, blue, gray) ,
                      labels = c("1 km","1.5 km","3 km"))+
  # ggtitle(paste0("Frequency ", 8000, " Hz ", "Extreme Climate Change \nAcross Territory Size")) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))+ #change angle to 0 for presentations
  theme(legend.position = "bottom")
setwd("dir")
ggsave("contacts_worst_cases_completions_paper.jpg", dpi=600, height=6, width=8, units="in")

# Calculating Mean Endpoints ----------------------------------------------
endpoints = completions_means %>%
  dplyr::filter(timeCnt == 360) %>%
  group_by(condition,Frequency,model)%>%
  rename(frequency = "Frequency")%>%
  summarise(n = n(),
            percent = mean(PercentMean),
            se = sd(PercentMean)/sqrt(n))

endpoints_w = worst_cases %>%
  dplyr::filter(timeCnt == 360) %>%
  group_by(condition,Frequency,model)%>%
  rename(frequency = "Frequency")%>%
  summarise(n = n(),
            percent = mean(PercentMean),
            se = sd(PercentMean)/sqrt(n))

endpoints$condition2 = recode_factor(endpoints$condition, 
                                     current = "Mean Contemporary",
                                       current_extreme = "Contemporary Drought", 
                                     climate_change = "Mean Climate Change",
                                       climate_change_extreme = "Climate Change Drought")

a1 = aov(lm(percent ~ condition*frequency*model, data = endpoints))
a1 = lm(percent_end~ condition, data = slope_start)

TukeyHSD(a1)
setwd("dir")
write.csv(endpoints, "model_endpoints.csv", row.names = FALSE)


###Graphing dot plots of endpoints
ggplot(data = endpoints,
       aes(x=condition2, y=percent, color = as.factor(frequency))) +
  geom_point(size = 5, aes(shape = as.factor(model)))+
  geom_errorbar(aes(ymin=percent-se, ymax=percent+se), width=1,
                 position=position_dodge(0.0))+
  scale_shape_discrete(name = "Model",
                     labels = c("No EWL", "EWL"))+
  scale_y_continuous(name = "Mean Completion Rate\n(Mean Percent Contacted All Neighbors)")+
  scale_x_discrete(name =  "Weather Conditions")+
  scale_colour_manual(name = "Frequency (Hz)",
                      values = c("#E69F00", "#56B4E9", "#999999"), #org, blue,gray
                      labels = c("4000","8000","12000"))+
  theme_classic(base_size = 10) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))+ #change angle to 0 for presentations
  theme(legend.position = "bottom")

setwd("dir")
ggsave("mean_completions_endpoints.jpg", dpi=600, height=10, width=12, units="in")
