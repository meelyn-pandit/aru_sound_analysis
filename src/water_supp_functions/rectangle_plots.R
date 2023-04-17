# SSWMA Water Supplementation - Date Bin - Plots ---------------------------------------

sswma1_rec1 <- data.frame (xmin=as_date("2021-05-17"), 
                           xmax=as_date("2021-05-30"), 
                           ymin=-Inf, ymax=Inf) #start of water site 1 with water
sswma2_rec1 <- data.frame (xmin=as_date("2021-05-30"), 
                           xmax=as_date("2021-06-13"), 
                           ymin=-Inf, ymax=Inf) #start of water site 2 with water
sswma1_rec2 = data.frame (xmin=as_date("2021-06-13"), 
                          xmax=as_date("2021-07-02"), 
                          ymin=-Inf, ymax=Inf) #start of water site 1 with water
sswma2_rec2 = data.frame (xmin=as_date("2021-07-03"), 
                          xmax=as_date("2021-08-07"), 
                          ymin=-Inf, ymax=Inf) #start of water at water site 2

#SSWMA Vocals Graph
ggplot(data = sswater_date,
       # wsvocals_day = ggplot(data = water_date %>%dplyr::filter(site == "sswma"), #uncomment to summarize by date only
       aes(x=date, y=log(mean_vocals), 
           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=sswma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#E69F00", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.1, inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean Num. Vocals)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_vocals_rectangle_plots.jpg", width = 8, height = 6, units = "in")

#Boxplot for Water SSWMA vocals Diversity
ggplot(data = sswater_date,
       aes(x=as.factor(ws_site), y=log(mean_vocals), 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean\nNum.\nVocals)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# sswmaw_vocals_plots = plot_grid(wsvocals_day, boxplot_sswma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_vocals_plots
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_vocals_boxplots.jpg", width = 8, height = 6, units = "in")

ggplot(data = sswater_date,
       # wsspecies_day = ggplot(data = water_date %>%dplyr::filter(site == "sswma"), #uncomment to summarize by date only
       aes(x=date, y=log(mean_species), 
           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=sswma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#E69F00", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=sswma2_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#E69F00", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),
                     name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Log\n(Mean Species Diversity")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_species_rectangle_plots.jpg", width = 8, height = 6, units = "in")

#Boxplot for Water SSWMA Species Diversity
ggplot(data = sswater_date,
       aes(x=as.factor(ws_site), y=log(mean_species), 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Log\n(Mean Species Diversity)")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# sswmaw_species_plots = plot_grid(wsspecies_day, boxplot_sswma_species,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_species_plots
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("sswma_water_species_boxplots.jpg", width = 8, height = 7.5, units = "in")

# CBMA Water Supplementation - Date Bin - Plots --------------------------------------------------------
full_water1 = full_water %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = full_water %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)


cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), xmax=as_date("2021-06-04"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), xmax=as_date("2021-07-19"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2

#CBMA Vocals Graph
ggplot(data = cbwater_date,
       # wsvocals_day = ggplot(data = water_date %>%dplyr::filter(site == "cbma"), #uncomment to summarize by date only
       aes(x=date, y=mean_species, 
           color = as.factor(ws_site))) +
  geom_point(size = 3, position = position_dodge())+
  geom_rect(data=cbma1_rec1, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  geom_rect(data=cbma1_rec2, 
            aes(xmin=xmin, 
                xmax=xmax, 
                ymin=ymin, 
                ymax=ymax), 
            fill="#56B4E9", 
            alpha=0.1, 
            inherit.aes = FALSE) +
  
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = "Water Station")+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Mean Species Diversity)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
        # axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right")
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("cbma_water_vocals_rectangle_plots.jpg", width = 8, height = 6, units = "in")

#Boxplot for Water cbma vocals Diversity
ggplot(data = cbwater_date,
       aes(x=as.factor(ws_site), y=mean_species, 
           color = as.factor(ws_site),
           fill=as.factor(water))) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 2,
               aes(group=as.factor(water)),
               position = position_dodge(0.6)) +
  scale_y_continuous(name = "Mean Species Diversity")+
  scale_color_manual(name = "Water Site",
                     values = c("#009E73","#D55E00","#CC79A7"))+
  scale_fill_manual(name = "Water Access", 
                    labels = c("Closed","Open"),
                    values = c("#ffffff", "#56B4E9"))+
  xlab(label="Water Site")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0
        axis.title.x=element_text(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.margin=margin(t=-20))
# cbmaw_vocals_plots = plot_grid(wcvocals_day, boxplot_cbma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));cbmaw_vocals_plots
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/results/")
ggsave("cbmaw_vocals_boxplots.jpg",width = 8, height = 6, units = "in")
