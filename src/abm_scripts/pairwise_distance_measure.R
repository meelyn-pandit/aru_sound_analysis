library(sp)

terr = seq(25,1500, by = 25) #60 different territory sizes
freq = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000)

setwd() #set working directory here
source("Atmospheric_sound_attenuation.R") #load up sound attenuation equation in the src directory

#Change working directory to data_clean
setwd()

current = "ERIC_current.Rdata" #load current weather data
extreme = "ERIC_current_extreme.Rdata" #load extreme weather data
climate_change = "ERIC_climate_change.Rdata" #load climate change weather data
extreme_climate_change = "ERIC_climate_change_extreme.Rdata" #load extreme climate change data

weather = as.list(c(current,extreme,climate_change,extreme_climate_change))

iter = 5
data_final = NULL
for(i in 1:iter){
  for(w in weather){
    weather_data <- paste0("dir",w,sep = "") #replace "dir" with working directory
    
    load(weather_data)
    for(t in 1:length(terr)){
      for(f in 1:length(freq)){
        Song_volume = 85 #volume of song/call in dB
        Song_detection = 30 #threshold for detecting a song
        SProb = 0.33 #Singing probability
        MProb = 0.33 #moving probability
        Song_freq = freq[f] #frequency or pitch of song/call in Hz
        
        #establish hexigon radius and number of random points
        rad = terr[t] #500 #hexigon radius.
        pts = 1000 #how many random points in each hxigon
        
        #Generate coordinates for hexigons 1 and 2 based on radius
        H1x <- c(-sin(pi/3)*rad, -sin(pi/3)*rad, 0, sin(pi/3)*rad, sin(pi/3)*rad, 0) + sin(pi/3)*rad
        H1y <- c(rad/2, -rad/2, -rad, -rad/2, rad/2, rad) + rad
        H2x <- c(-sin(pi/3)*rad, -sin(pi/3)*rad, 0, sin(pi/3)*rad, sin(pi/3)*rad, 0) + sin(pi/3)*rad + max(H1x)
        #y coordinates are the same for both hexigons
        
        # plot(H1x, H1y, xlim=c(-100, 4*rad+100), asp=1)
        # points(H2x, H1y, col="red")
        
        #Make spatial polygon of first Hexigon
        H1 = matrix(data = c(H1x, H1y), ncol = 2) #start with square
        PH1 = Polygons(list(Polygon(H1)), ID = "a")
        SH1 = SpatialPolygons(list(PH1))
        
        #Make spatial polygon of second Hexigon
        H2 = matrix(data = c(H2x, H1y), ncol = 2) #start with square
        PH2 = Polygons(list(Polygon(H2)), ID = "a")
        SH2 = SpatialPolygons(list(PH2))
        
        #genearate random points within each hexigon
        HP1 <-spsample(SH1, n=10000, type = "random") #
        HP2 <-spsample(SH2, n=10000, type = "random") #
        
        #make a data frame with all pairs of random points.
        HPall <- data.frame(H1x = HP1@coords[,1], H1y = HP1@coords[,2], H2x = HP2@coords[,1], H2y = HP2@coords[,2])
        #Calculate the distance between each pair of points.
        HPall$dist <- sqrt((HPall$H1x-HPall$H2x)^2 + (HPall$H1y-HPall$H2y)^2)
        
        # hist(HPall$dist)
        # mean(HPall$dist)
        # sd(HPall$dist)
        
        
        # Call Distance on 06/01 (average current weather data) -------------------
        wdavg$CallRad <- mapply(aud_range, Song_volume, Song_detection, Song_freq,
                                wdavg$TAIR, wdavg$RELH, wdavg$PRES)
        
        # Combining Random Points and Weather Data --------------------------------
        
        points = HPall[1:288,] 
        points2 = cbind(wdavg$CallRad,wdavg$bin1,points, row.names = NULL)
        names(points2) = c("call_rad","bin1", "h1x","h1y","h2x","h2y","dist")
        
        # Calculating Successful Contact Percent ----------------------------------
        points2 = points2[1:72, ] #limiting to the 6hr period of the ABM
        points2$contact = ifelse(points2$dist <= points2$call_rad,1,0) #placing a 1 if the distance is less than or equal to the call radius
        sum(points2$contact)
        data = data.frame(freq = Song_freq,
                          terr_size = rad,
                          # call_rad = points2$call_rad,
                          success_contact = sum(points2$contact),
                          iteration = i,
                          condition = w
        )
        data_final = rbind(data_final,data)
        print(paste0(freq," kHz, ",w, ", ", i, " iteration is done"))
      } #end of frequency loop
    }#end of territory loop
  }#end of weather loop
}#end of iter loop


data_final$percent = data_final$success_contact/length(points2$contact)
aggregate(data.frame(count = data_final$condition), list(value = data_final$condition), length)
data_final$condition = recode_factor(data_final$condition, ERIC_current.Rdata = "current",
                                      ERIC_current_extreme.Rdata = "extreme", ERIC_climate_change.Rdata = "climate_change",
                                      ERIC_climate_change_extreme.Rdata = "extreme_climate_change")

#Change working directory to data_clean
setwd(dir)
save(data_final, file = "individual_contact_full.Rdata")

# Heatmap of Success Rate Frequency x Territory Size

# Library
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(RColorBrewer)
library(viridis)
library(dplyr)
library(lme4)
library(multcomp) #posthoc tests for ANOVA type III effects
library(car)


# # Full Individual Contact Data
setwd("dir") #replace with working directory that contains Individual Contact Data
load("individual_contact_full.Rdata")

ind_means = data_final %>%
  group_by(condition,freq,terr_size) %>%
  summarise(n = n(),
            mean_percent = mean(percent*100),
            se_percent = sd(percent*100)/sqrt(n))

ind_means$condition2 = factor(ind_means$condition, levels=c("current","extreme","climate change","extreme climate change"))
ind_means$condition3 = recode_factor(ind_means$condition2, current = "Contemporary",
                                     extreme = "Extreme", "climate change" = "Climate Change",
                                     "extreme_climate_change" = "Extreme Climate Change")
#Interaction Terms
ind_means$freq_terr = interaction(ind_means$freq,ind_means$terr_size)

#Linear model to determine if contact percentages were affected by territory size, frequency, and weather conditions
m1<-lm((percent*100) ~ terr_size*freq*condition, data=data_final) #this is what is in the manuscript
summary(m1)
assump(m1)
Anova(m1, type = 'III')
summary(a1)
TukeyHSD(aov(m1))
plot(TukeyHSD(aov(m1)))
contrasts = glht(m1, linfct = mcp(condition = "Tukey"))
options(max.print = 999999)
summary(contrasts)

#Plotting territory size against frequency
ggplot(ind_means,
       aes(x = terr_size,
           y = freq,
           fill= percent)) +
  geom_tile() +
  scale_fill_continuous(type = "viridis"
                        # labels = "Successful Contacts"
  )+
  scale_y_continuous(name = "Frequency (Hz)",
                     limits=c(0,12000),
                     breaks = seq(0, 12000, by = 4000))+
  scale_x_continuous(name =  "Territory Size (m)")+
  facet_wrap(~condition2) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))+
  ggtitle("Current - Current Extreme \n% Frequencies affected by Extreme Conditions") +
  labs(fill = "% Contact Rate") +
  theme_classic()
setwd("dir")
ggsave("individual_contact_rate_heatmap_full.jpg", dpi=600, height=6, width=12, units="in")

# Differences between Regular and Extreme Data ----------------------------
cur_data = ind_means %>% #create current conditions only
  dplyr::filter(condition == "current")

cur_extreme_data = ind_means %>%
  dplyr::filter(condition == "extreme")

cc_data = ind_means %>%
  dplyr::filter(condition == "climate change")

cc_extreme_data = ind_means %>%
  dplyr::filter(condition == "extreme climate change")

cur_data$subtractions = cur_extreme_data$mean_percent-cur_data$mean_percent

ggplot(cur_data,
       aes(x = terr_size,
           y = freq,
           fill= subtractions)) +
  geom_tile() +
  scale_fill_gradientn(colours = c('#5749a0', '#0f7ab0', '#00bbb1','#bef0b0', '#fdf4af'),
                       limits=c(-100,100),
                       breaks = seq(-100,100, by = 100),
                       labels = c(-100,0,100))+
  scale_y_continuous(name = "Frequency (Hz)",
                     limits=c(0,12000),
                     breaks = seq(0, 12000, by = 4000))+
  scale_x_continuous(name =  "Territory Size (m)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))+
  # ggtitle("Extreme - Contemporary \n% Frequencies affected by Extreme Conditions") +
  labs(fill = "% Contact Rate") +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +#change angle to 0 for presentations
  theme(legend.position = "bottom", legend.spacing.x = unit(1, unit = "cm"))
setwd("dir")
ggsave("current_subtractions_heatmap_full.jpg", dpi=600, height=10, width=12, units="in")


# #Climate Change Weather Data
cc_data$subtractions = cc_extreme_data$mean_percent-cc_data$mean_percent

cur_data$condition2 = "A. Contemporary"
cc_data$condition2 = "B. Climate Change"


cc_data2 = rbind(cur_data,cc_data)
cc_data2$condition3 = factor(cc_data2$condition2, levels=c("A. Contemporary","B. Climate Change"))

ggplot(cc_data2,
       aes(x = terr_size,
           y = freq,
           fill= subtractions)) +
  geom_tile() +
  # scale_fill_continuous(type = okabe
  #                       # labels = "Successful Contacts"
  #                       )+
  scale_fill_gradientn(colours = c('#5749a0', '#0f7ab0', '#00bbb1','#bef0b0', '#fdf4af'),
                       limits=c(-100,100),
                       breaks = seq(-100,100, by = 100),
                       labels = c(-100,0,100))+
  # scale_fill_manual(values = okabe)+
  scale_y_continuous(name = "Frequency (Hz)",
                     limits=c(0,12000),
                     breaks = seq(0, 12000, by = 4000))+
  scale_x_continuous(name =  "Territory Size (m)")+
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))+
  # ggtitle("Climate Change - Climate Change Extreme \n% Frequencies affected by Extreme Conditions") +
  labs(fill = "% Contact Rate") +
  facet_grid(~condition3) +
  theme_classic(base_size = 20) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +#change angle to 0 for presentations
  theme(legend.position = "bottom", legend.spacing.x = unit(1, unit = "cm"))

setwd("dir")
ggsave("panel_extreme_subtraction_heatmap_full.tiff", dpi=600, height=10, width=15, units="in")



