# Comparing against Manual Detections in Validation Files -----------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
arus = as.list(c("aru01","aru04","aru05","ws01","ws02","ws03","ws04","ws05","ws06","ws11","ws12","ws13","ws14","ws15"))
valid_final = NULL
for(i in arus){
  setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/",
               i))
  birdnet = list.files(pattern = ".txt")
  for(j in 1:length(birdnet)){
    
    valid_temp = read.table(birdnet[[j]], header = FALSE, sep = "", fill = TRUE)
    names(valid_temp) = c("start","end","id") 
    valid_temp$id = substr(valid_temp$id,1,4)
    valid_temp$date_time = as_datetime(substr(birdnet[[j]],1,15))
    valid_temp$date = date(valid_temp$date_time)
    valid_temp$time = hms(substr(valid_temp$date_time,12,19))
    valid_temp$aru = i   
    valid_temp$site = "sswma"
    valid_final= rbind(valid_final,valid_temp)
  }
  
}

#Saving validation data
sswma_valid = valid_final
valid_species_hour = sswma_valid %>%
  filter(time@hour <=13) %>% 
  group_by(date_time,aru,site) %>%
  summarise(n = n(),
            species_diversity = n_distinct(id))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
write.csv(valid_species_hour, "validation_diversity.csv", row.names = FALSE)
# rename(time = "time@hour") %>%

mmp_valid = full_join(valid_species_hour,species_hour, by = c("date_time","aru")) %>%
  rename("mmp_num_vocals" = n,
         "mmp_species_diversity" = species_diversity.x,
         "birdnet_num_vocals" = num_vocals,
         "birdnet_species_diversity" = species_diversity.y) %>%
  select(mmp_num_vocals,
         mmp_species_diversity,
         birdnet_num_vocals,
         birdnet_species_diversity) %>%
  mutate(mmp_num_vocals = as.numeric(mmp_num_vocals),
         mmp_species_diversity = as.numeric(mmp_species_diversity),
         birdnet_num_vocals = as.numeric(birdnet_num_vocals),
         birdnet_species_diversity = as.numeric(birdnet_species_diversity)) %>%
  na.omit()
mmp_cor = cor(mmp_valid[, 3:6], use ="everything")
cor.test(mmp_valid$mmp_species_diversity,mmp_valid$birdnet_species_diversity)
cor.test(mmp_valid$mmp_num_vocals, mmp_valid$birdnet_num_vocals)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
write.csv(mmp_valid, "mmp_valid_data.csv", row.names = FALSE)

# Correlating Validation Data ---------------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")

valid_data = read.csv("validation_acoustic_indices.csv", header = TRUE) %>%
  na.omit()


valid_cor = cor(valid_data[, 4:17], use ="everything")
cor.test(valid_data$user_num_vocals,valid_data$birdnet_unfiltered_num_vocals) #r = 0.244 (0.091-0.386), p = 0.002, 
cor.test(valid_data$num_vocals,valid_data$pabu_aei) #r =-0.463, p = 0.035

melted_valid <- melt(valid_cor)
head(melted_valid)
# lower_valid <- valid_cor
# 
# # Make lower triangular matrix by setting NA to upper triangular part:
# lower_valid[upper.tri(lower_valid)] <- NA
# lower_m_valid <- melt(lower_valid, na.rm = TRUE)


# Ggplot lower triangular correlation matrix:

ggplot(data = melted_valid, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, mid ="grey70", 
                       limits = c(-1, +1)) +
  labs(title = "Correlation Matrix of Validation Data, Acoustic Indices, and BirdNet-Lite Analysis", 
       x = "", y = "", fill = "Correlation \n Measure") +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
        legend.title = element_text(face="bold", colour="brown", size = 10)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            fontface = "bold", size = 5)+
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))



cbma_data$date_time = cbma_data$local_time
miss_weather = anti_join(results_final, cbma_data, by = "date_time")
combined = full_join(results_final, cbma_data, by = "date_time")
combined2 = combined %>%
  filter(year(date_time) != 1970)
combined2$temperature = na.approx(combined2$temperature) #approximate dewpoint temperatuer
combined2$relh = na.approx(combined2$relh) #approximate air temperature
combined2$pressure = na.approx(combined2$pressure) #approximate relative humidity


setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean")
# save(combined, file = "cbma_aru_species_results.Rdata")
save(combined2, file = "cbma_wg_spcecies_results.Rdata")
write.csv(combined2, file = "cbma_wg_spcecies_results.csv", row.names = FALSE)

cbma_wg_aci = read.csv("aci_water_cbma.csv", header = TRUE)
cbma_comb_data = full_join(combined2%>%dplyr::filter(aru == "wg01"),cbma_wg_aci%>%dplyr::filter(aru == "wgo1"), by = "aru")


combined %>%
  group_by(Common.name) %>%
  filter(date_time == max(date_time)) %>%
  ungroup()

species_hour = combined2 %>%
  group_by(date, time@hour, Common.name) %>%
  summarise(n = n(),
            temp = mean(temperature),
            relh = mean(relh)) %>%
  rename(time = "time@hour") %>%
  filter(time <=13) %>% filter(is.na(Common.name) == FALSE)

ggplot(data = species_hour, aes(x = time, y = n, color = Common.name))+
  geom_line()+
  # geom_bar(stat = "identity", position = position_dodge(0.2))+
  scale_y_continuous(limits = c(0,1250)) +
  facet_wrap(~Common.name)

m1 = lmer(n ~ scale(date) * time + (1|Common.name), data = species_hour, REML= FALSE)
summary(m1)
m2 = lmer(n ~ temp*relh*scale(date) + (1|Common.name), data = species_hour, REML= FALSE)
summary(m2)
