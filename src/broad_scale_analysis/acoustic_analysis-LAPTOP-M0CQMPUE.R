##############################################
library(soundecology)
library(tuneR)
library(seewave)
library(osfr)
library(dplyr)
library(lubridate)
library(lme4)
library(lmerTest)
library(okmesonet)
library(ggplot2) #create good graphs
library(extrafont)
library(lsmeans)


setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/src/broad_scale_analysis")
aru = c("ws01",
        "ws02",
        "ws03",
        "ws04",
        "ws05",
        "ws06",
        "ws07",
        "ws08",
        "ws09",
        "ws10",
        "ws11",
        "ws12",
        "ws13",
        "ws14",
        "ws15")
aru_list = as.list(aru)


results = paste0(dir,"_adi_results.csv")
results2 = paste0(dir, "bio_results.csv")
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/data/")


# Obtaining Acoustic Indices ----------------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/sswma/")

adi_results = NULL
bio_results = NULL
aei_results = NULL
aci_results = NULL

for(i in 1:length(aru_list)){
  dir = aru_list[[i]]
  results = paste0(dir,"_adi_results.csv")
  multiple_sounds(directory = dir, resultfile = results, soundindex = "acoustic_diversity", max_freq = 8000, no_cores = "max", db_threshold = -70)
  adi_results_temp <- read.csv(results, header = TRUE)
  adi_results_temp$aru = dir
  adi_results = rbind(adi_results, adi_results_temp)
  
  results2 = paste0(dir, "_bio_results.csv")
  multiple_sounds(directory = dir, resultfile = results2, soundindex = "bioacoustic_index", max_freq = 8000, no_cores = "max")
  bio_results_temp <- read.csv(results2, header = TRUE)
  bio_results_temp$aru = dir
  bio_results = rbind(bio_results, bio_results_temp)
  
  results3 = paste0(dir, "_aei_results.csv")
  multiple_sounds(directory = dir, resultfile = results3, soundindex = "acoustic_evenness", max_freq = 8000, no_cores = "max", db_threshold = -70)
  aei_results_temp <- read.csv(results3, header = TRUE)
  aei_results_temp$aru = dir
  aei_results = rbind(aei_results, aei_results_temp)
  
  results4 = paste0(dir, "_aci_diversity.csv")
  multiple_sounds(directory = dir, resultfile = results4, soundindex = "acoustic_complexity", max_freq = 8000, no_cores = "max")
  aci_results_temp <- read.csv(results4, header = TRUE)
  aci_results_temp$aru = dir
  aci_results = rbind(aci_results, aci_results_temp)
}

# Renaming and creating new variables -------------------------------------
names(adi_results) = c("filename","sampling_rate","bit","duration","channels","index","fft_w","min_freq","max_freq", "left_channel","right_channel","aru")
names(bio_results) = c("filename","sampling_rate","bit","duration","channels","index","fft_w","min_freq","max_freq", "left_channel","right_channel","aru")

# aei_results = aei_results[,-c(13:22)]
names(aei_results) = c("filename","sampling_rate","bit","duration","channels","index","max_freq", "db_threshold", "freq_steps", "left_channel","right_channel","aru")
names(aci_results) = c("filename","sampling_rate","bit","duration","channels","index","fft_w","min_freq","max_freq", "j", "left_channel","right_channel","aru")

# adi_results$station = as.factor((substr(adi_results$aru, 1,4)))
adi_results$year = as.factor((substr(adi_results$filename, 1,4)))
adi_results$month = as.factor((substr(adi_results$filename,5,6)))
adi_results$day = as.factor((substr(adi_results$filename, 7,8)))
adi_results$hour = as.factor(substr(adi_results$filename, 10,11))
adi_results$min = as.factor(substr(adi_results$filename, 12,13))
adi_results$second = as.factor(substr(adi_results$filename, 14,15))

adi_results$date = as.character(paste0(adi_results$year,
                                       "-",
                                       adi_results$month,
                                       "-",
                                       adi_results$day))

adi_results$time = as.character(paste0(adi_results$hour,":",
                                       adi_results$min,":",
                                       adi_results$second))
adi_results$date_time = as.POSIXct(as.character(paste0(adi_results$date,
                                                       " ", 
                                                       adi_results$time), 
                                                format = "%Y-%m-%d %H:%M:%S"))# 

# adi_results = adi_results %>%
#   mutate(treatment = case_when(station == "ws01" | 
#                                  station == "ws02" | 
#                                  station == "ws03" |
#                                  station == "ws04" | 
#                                  station == "ws05" ~ "water",
#                                station == "ws06" 
#                                | station == "ws07" | 
#                                  station == "ws08" |
#                                  station == "ws09" | 
#                                  station == "ws10" ~ "pos_control",
#                                station == "ws11" | 
#                                  station == "ws12" | 
#                                  station == "ws13" |
#                                  station == "ws14" | 
#                                  station == "ws15" ~ "neg_control"))

# bio_results$station = as.factor((substr(bio_results$aru, 1,4)))
bio_results$year = as.factor((substr(bio_results$filename, 1,4)))
bio_results$month = as.factor((substr(bio_results$filename,5,6)))
bio_results$day = as.factor((substr(bio_results$filename, 7,8)))
bio_results$hour = as.factor(substr(bio_results$filename, 10,11))
bio_results$min = as.factor(substr(bio_results$filename, 12,13))
bio_results$second = as.factor(substr(bio_results$filename, 14,15))

bio_results$date = as.character(paste0(bio_results$year,
                                       "-",
                                       bio_results$month,
                                       "-",
                                       bio_results$day))

bio_results$time = as.character(paste0(bio_results$hour,":",
                                       bio_results$min,":",
                                       bio_results$second))
bio_results$date_time = as.POSIXct(as.character(paste0(bio_results$date,
                                                       " ", 
                                                       bio_results$time), 
                                                format = "%Y-%m-%d %H:%M:%S"))
# bio_results = bio_results %>%
#   mutate(treatment = case_when(station == "ws01" | 
#                                  station == "ws02" | 
#                                  station == "ws03" |
#                                  station == "ws04" | 
#                                  station == "ws05" ~ "water",
#                                station == "ws06" 
#                                | station == "ws07" | 
#                                  station == "ws08" |
#                                  station == "ws09" | 
#                                  station == "ws10" ~ "pos_control",
#                                station == "ws11" | 
#                                  station == "ws12" | 
#                                  station == "ws13" |
#                                  station == "ws14" | 
#                                  station == "ws15" ~ "neg_control"))

# aei_results$station = as.factor((substr(aei_results$aru, 1,4)))
aei_results$year = as.factor((substr(aei_results$filename, 1,4)))
aei_results$month = as.factor((substr(aei_results$filename,5,6)))
aei_results$day = as.factor((substr(aei_results$filename, 7,8)))
aei_results$hour = as.factor(substr(aei_results$filename, 10,11))
aei_results$min = as.factor(substr(aei_results$filename, 12,13))
aei_results$second = as.factor(substr(aei_results$filename, 14,15))

aei_results$date = as.character(paste0(aei_results$year,
                                       "-",
                                       aei_results$month,
                                       "-",
                                       aei_results$day))

aei_results$time = as.character(paste0(aei_results$hour,":",
                                       aei_results$min,":",
                                       aei_results$second))
aei_results$date_time = as.POSIXct(as.character(paste0(aei_results$date,
                                                       " ", 
                                                       aei_results$time), 
                                                format = "%Y-%m-%d %H:%M:%S"))# 

# aei_results = aei_results %>%
#   mutate(treatment = case_when(station == "ws01" | 
#                                  station == "ws02" | 
#                                  station == "ws03" |
#                                  station == "ws04" | 
#                                  station == "ws05" ~ "water",
#                                station == "ws06" 
#                                | station == "ws07" | 
#                                  station == "ws08" |
#                                  station == "ws09" | 
#                                  station == "ws10" ~ "pos_control",
#                                station == "ws11" | 
#                                  station == "ws12" | 
#                                  station == "ws13" |
#                                  station == "ws14" | 
#                                  station == "ws15" ~ "neg_control"))

# aci_results$station = as.factor((substr(aci_results$aru, 1,4)))
aci_results$year = as.factor((substr(aci_results$filename, 1,4)))
aci_results$month = as.factor((substr(aci_results$filename,5,6)))
aci_results$day = as.factor((substr(aci_results$filename, 7,8)))
aci_results$hour = as.factor(substr(aci_results$filename, 10,11))
aci_results$min = as.factor(substr(aci_results$filename, 12,13))
aci_results$second = as.factor(substr(aci_results$filename, 14,15))

aci_results$date = as.character(paste0(aci_results$year,
                                       "-",
                                       aci_results$month,
                                       "-",
                                       aci_results$day))

aci_results$time = as.character(paste0(aci_results$hour,":",
                                       aci_results$min,":",
                                       aci_results$second))
aci_results$date_time = as.POSIXct(as.character(paste0(aci_results$date,
                                                       " ", 
                                                       aci_results$time), 
                                                format = "%Y-%m-%d %H:%M:%S"))# 

# aci_results = aci_results %>%
#   mutate(treatment = case_when(station == "ws01" | 
#                                  station == "ws02" | 
#                                  station == "ws03" |
#                                  station == "ws04" | 
#                                  station == "ws05" ~ "water",
#                                station == "ws06" 
#                                | station == "ws07" | 
#                                  station == "ws08" |
#                                  station == "ws09" | 
#                                  station == "ws10" ~ "pos_control",
#                                station == "ws11" | 
#                                  station == "ws12" | 
#                                  station == "ws13" |
#                                  station == "ws14" | 
#                                  station == "ws15" ~ "neg_control"))
aci_results$aci_rate = aci_results$left_channel/aci_results$duration #rate is aci/s
# The results given are accumulative. Very long samples will return very large values for ACI. I recommend to divide by number of minutes to get a range of values easier to compare.

# Getting Weather Data ----------------------------------------------------
### Weather patterns, using data from ERIC Station from 05/18-05/25/2021
dates = as.list("2021-05-18",
          "2021-05-19",
          "2021-05-20",
          "2021-05-21",
          "2021-05-22",
          "2021-05-23",
          "2021-05-24",
          "2021-05-25",
          )

w=NULL
for(i in dates){ #beginning of weather data loop
  beginTime = paste0(i,"05:00:00", sep = " ")
  endTime = paste0(i,"12:50:00", sep = " ")

stid <- "ERIC"
stations <- read.csv("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/data/geoinfo.csv", stringsAsFactors = F)
lat = stations$nlat[which(stations$stid == stid)]
lon = stations$elon[which(stations$stid == stid)]

w <- okmts(begintime=beginTime,
            endtime=endTime, 
            variables = c("TAIR", "RELH","PRES"),
            station="eric",  
            localtime=T) #Need to download the data into UTC, the time will appear as CDT

}

plot(w$TIME,w$RELH)
plot(w$TIME,w$TAIR)

w$DT = as.POSIXct(w$TIME, tz = "UTC")
w$DTL = w$DT #Saves datetime into a new vector for local datetime
w$DATE = as.POSIXct(substr(w$TIME,0,10))
w$MONTH = as.factor(substr(w$TIME,6,7))
w$DAY = as.factor(substr(w$TIME,9,10))
w$YMD = w$DATE
attributes(w$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
w$YMDL <- as_date(w$DTL) #gives local (oklahoma) ymd date
w$DEW = w$TAIR-((100-w$RELH)/5)


# Analyzing Data ----------------------------------------------------------

adi_results2 = merge(adi_results, w, by.x = "date_time", by.y = "TIME") 
bio_results2 = merge(bio_results, w, by.x = "date_time", by.y = "TIME") 
aei_results2 = merge(aei_results, w, by.x = "date_time", by.y = "TIME") 
aci_results2 = merge(aci_results, w, by.x = "date_time", by.y = "TIME") 


# Saving Final Soundscape Dataframes --------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
write.csv(adi_results2, file = "adi_results.csv", row.names = FALSE)
write.csv(bio_results2, file = "bio_results.csv", row.names = FALSE)
write.csv(aei_results2, file = "aei_results.csv", row.names = FALSE)
write.csv(aci_results2, file = "aci_results.csv", row.names = FALSE)

plot(adi_results2$date_time, adi_results2$left_channel)
plot(bio_results2$date_time, bio_results2$left_channel)
plot(aei_results2$date_time, aei_results2$left_channel)
plot(aci_results2$date_time, aci_results2$left_channel)


shapiro.test(adi_results2$left_channel)
hist(adi_results2$left_channel)

m1 = lm(left_channel ~ date, 
          data = adi_results2)
summary(m1)
anova(m1)
lsmeans(m1, pairwise~date, adjust="tukey")

m2 = lmer(left_channel ~ date-1 +
            (1|treatment/station), 
          data = bio_results2)
summary(m2)
anova(m2)
lsmeans(m2, pairwise~date, adjust="tukey")

m3 = lmer(left_channel ~ date-1 +
            # scale(as.numeric(hms(time))) * 
            # date + 
            (1|treatment/station), 
          data = aei_results2)
summary(m3)
anova(m3)
lsmeans(m3, pairwise~date, adjust="tukey")

m4 = lmer(left_channel ~ date-1 + #boundary singular fit
            # scale(as.numeric(hms(time))) * 
            # date + 
            (1|treatment/station), 
          data = aci_results2)
summary(m4)
anova(m4)
lsmeans(m4, pairwise~date, adjust="tukey")


# Regression Diagnostics --------------------------------------------------
#Assessing Outliers
library(car)
library(predictmeans)
library(tidyverse)
library(broom)

outlierTest(m1) # Bonferonni p-value for most extreme obs
qqPlot(m1, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(m1) # leverage plots

assump<-function(model) {       #graphs for checking assumptions
  r<-residuals(model)
  ft<-fitted(model)
  par(mfrow=c(2,2))
  library(MASS)
  truehist(r,main="Histogram of Residuals",xlab="Residuals")
  curve(dnorm(x,mean=mean(r),sd=sd(r)),add=TRUE)
  qqnorm(r, ylim=range(r), main="QQNorm Plot",ylab="Quantiles of (ie, ordered) residuals", xlab="Quantiles of normal distribution")
  qqline(r,lty=2)
  plot(r~ft,main="Residuals vs Fitted",xlab="Fitted (predicted) values",ylab="Residuals");abline(h=0)
  qqnorm(ft, ylim=range(ft), main="QQNorm Plot",ylab="Quantiles of fitted values", xlab="Quantiles of normal distribution")
  qqline(ft,lty=2)
  acf(resid(model))
}
assump(m2)

par(mfrow = c(2,2))
plot(m2)


# Models Across Time ------------------------------------------------------
m5 = lmer(left_channel ~ scale(as.numeric(hms(time))) *
            date-1 +
            (1|treatment/station), 
          data = adi_results2)
summary(m5)
anova(m5)
lsmeans(m5, pairwise~scale(as.numeric(hms(time))) * date, adjust="tukey")

m6 = lmer(left_channel ~ scale(as.numeric(hms(time))) *
            date-1 +
            (1|treatment/station), 
          data = bio_results2)
summary(m6)
anova(m6)
lsmeans(m6, pairwise~date, adjust="tukey")

m7 = lmer(left_channel ~ scale(as.numeric(hms(time))) *
            date-1 +
            (1|treatment/station), 
          data = aei_results2)
summary(m7)
anova(m7)
lsmeans(m7, pairwise~date, adjust="tukey")

m8 = lmer(left_channel ~ scale(as.numeric(hms(time))) *
            date-1 +
            (1|treatment/station), 
          data = aci_results2)
summary(m8)
anova(m8)
lsmeans(m8, pairwise~date, adjust="tukey")

m9 = lmer(left_channel ~ scale(date_time) + (1|treatment/station), data = adi_results2)
summary(m9)

m10 = lmer(left_channel ~ scale(date_time) + (1|treatment/station), data = bio_results2)
summary(m10)

m11 = lmer(left_channel ~ scale(date_time) + (1|treatment/station), data = aei_results2)
summary(m11)

m12 = lmer(left_channel ~ scale(date_time) + (1|treatment/station), data = aci_results2)
summary(m12)

plot(w2$TIME,w2$TAIR)
plot(w$TIME,w$TAIR)
plot(w3$TIME,w3$TAIR)

plot(w$TIME,w$RELH)
plot(w$TIME,w$PRES)


# Creating Means for Plotting----------------------------------------------

#filter out arus that are missing recordings from one of the three dates
#ws05_aru01, ws06_aru03, ws08_aru02, ws14_aru02 do not have audio data for 07/15/20

adi_avg = adi_results2 %>%
  filter(aru != "ws05_aru01",
         aru != "ws06_aru03",
         aru != "ws08_aru02",
         aru != "ws14_aru02") %>%
  group_by(treatment, date, time) %>%
  summarise(mean_adi = mean(left_channel),
            n = n())

bio_avg = bio_results2 %>%
  filter(aru != "ws05_aru01",
         aru != "ws06_aru03",
         aru != "ws08_aru02",
         aru != "ws14_aru02") %>%
  group_by(treatment, date, time) %>%
  summarise(mean_bio = mean(left_channel),
            mean_tair = mean(TAIR),
            mean_relh = mean(RELH),
         n = n())

aei_avg = aei_results2 %>%
  filter(aru != "ws05_aru01",
         aru != "ws06_aru03",
         aru != "ws08_aru02",
         aru != "ws14_aru02") %>%
  group_by(treatment, date, time) %>%
  summarise(mean_aei = mean(left_channel),
            mean_tair = mean(TAIR),
            mean_relh = mean(RELH),
            n = n())

aci_avg = aci_results2 %>%
  filter(aru != "ws05_aru01",
         aru != "ws06_aru03",
         aru != "ws08_aru02",
         aru != "ws14_aru02") %>%
  group_by(treatment, date, time) %>%
  summarise(mean_aci = mean(aci_rate),
            mean_tair = mean(TAIR),
            mean_relh = mean(RELH),
            n = n())


# Acoustic Diversity Index ------------------------------------------------

ggplot(adi_avg, aes(x=as.numeric(hms(time)), 
                    y=mean_adi, 
                    color = date)) + 
  geom_point(size = 3) +
  # geom_smooth() +
  geom_smooth(method = 'lm', se = TRUE) +
  labs(colour = "Date",
       x = "Time (CST)",
       y = "Acoustic \nDiversity \nIndex") +
  scale_x_time(breaks=c(18000, 21000,24000,27000),
                     labels=c("05:00", "05:50", "06:40", "07:30"))+
  scale_colour_manual(values = c("#009e73", #green
                                 "#E69F00", #orange
                                 "#56B4E9" ))+ #blue
  theme_classic(base_size=10)+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) #rotate axis 90 degrees for presentations
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/results")
ggsave("adi.jpg", dpi=300, height=6, width=12, units="in")
  

# Biodiversity Index ------------------------------------------------------

ggplot(bio_avg, aes(x=as.numeric(hms(time)), 
                      y=mean_bio, 
                      color = date)) + 
  # geom_errorbar(aes(ymin=meanVisits-seVisits, 
  #                   ymax=meanVisits+seVisits), 
  #               width=.5) + 
  geom_point(size = 3) +
  # geom_point(size=2) +
  labs(colour = "Date",
       x = "Time (CST)",
       y = "Biodiversity \nIndex") +
  geom_smooth(method = 'lm', se = TRUE) +
  # geom_smooth() +
  scale_x_time(breaks=c(18000, 21000,24000,27000),
               labels=c("05:00", "05:50", "06:40", "07:30"))+
  scale_colour_manual(values = c("#009e73", #green
                                 "#E69F00", #orange
                                 "#56B4E9" ))+ #blue
  theme_classic(base_size=30)+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) #rotate axis 90 degrees for presentations
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/results")
ggsave("bio.jpg", dpi=300, height=6, width=12, units="in")

# Acoustic Evenness Index Scatterplot -------------------------------------
ggplot(aei_avg, aes(x=as.numeric(hms(time)), 
                         y=mean_aei, 
                         color = date)) + 
  # geom_errorbar(aes(ymin=meanVisits-seVisits, 
  #                   ymax=meanVisits+seVisits), 
  #               width=.5) + 
  geom_point(size = 3) +
  # geom_point(size=2) +
  labs(colour = "Date",
       x = "Time (CST)",
       y = "Acoustic \nEvenness \nIndex") +
  geom_smooth(method = 'lm', se = TRUE) +
  # geom_smooth() +
  scale_x_time(breaks=c(18000, 21000,24000,27000),
               labels=c("05:00", "05:50", "06:40", "07:30"))+
  scale_colour_manual(values = c("#009e73", #green
                                 "#E69F00", #orange
                                 "#56B4E9" ))+ #blue
  theme_classic(base_size=20)+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) #rotate axis 90 degrees for presentations
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/results")
ggsave("aei.jpg", dpi=300, height=6, width=12, units="in")

# Acoustic Complexity Index - Corrected for Time --------------------------
ggplot(aci_avg, aes(x=as.numeric(hms(time)), 
                    y=mean_aci, 
                    color = date)) + 
  geom_point(size = 3) +
  # geom_point(size=2) +
  labs(colour = "Date",
       x = "Time (CST)",
       y = "Acoustic \nComplexity \nIndex\n (Corrected \nfor \nTime)") +
  geom_smooth(method = 'lm', se = TRUE) +
  # geom_smooth() +
  scale_x_time(breaks=c(18000, 21000,24000,27000),
               labels=c("05:00", "05:50", "06:40", "07:30"))+
  scale_colour_manual(values = c("#009e73", #green
                                 "#E69F00", #orange
                                 "#56B4E9" ))+ #blue
  theme_classic(base_size=30)+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) #rotate axis 90 degrees for presentations
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/results")
ggsave("aci.jpg", dpi=300, height=6, width=12, units="in")

# Temperature over Time ---------------------------------------------------


ggplot(bio_avg, aes(x=as.numeric(hms(time)), 
                    y=mean_tair, 
                    color = date)) + 
  # geom_errorbar(aes(ymin=meanVisits-seVisits, 
  #                   ymax=meanVisits+seVisits), 
  #               width=.5) + 
  geom_point(size = 3) +
  # geom_point(size=2) +
  labs(colour = "Date",
       x = "Time (CST)",
       y = "Average Air Temp (C)") +
  geom_smooth() +
  scale_colour_manual(values = c("#009e73", #green
                                 "#E69F00", #orange
                                 "#56B4E9" ))+ #blue
  # theme_bw(base_size=12, base_family='TT Times New Roman')+
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank())
  theme_classic(base_size=30, 
                base_family='Times New Roman')

ggplot(bio_avg, aes(x=as.numeric(hms(time)), 
                    y=mean_relh, 
                    color = date)) + 
  # geom_errorbar(aes(ymin=meanVisits-seVisits, 
  #                   ymax=meanVisits+seVisits), 
  #               width=.5) + 
  geom_point(size = 3) +
  # geom_point(size=2) +
  labs(colour = "Date",
       x = "Time (CST)",
       y = "Relative Humidity (%)") +
  geom_smooth() +
  scale_colour_manual(values = c("#009e73", #green
                                 "#E69F00", #orange
                                 "#56B4E9" ))+ #blue
  # theme_bw(base_size=12, base_family='TT Times New Roman')+
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank())
  theme_classic(base_size=30, 
                base_family='Times New Roman')
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/results")
ggsave("heat_burst.jpg", dpi=300, height=4.5, width=7.0, units="in")
