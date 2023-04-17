
#Run the Aridity ABM
# library(pbapply)

# Environmental Conditions ------------------------------------------------

# current = "ERIC_current.Rdata"
# extreme = "ERIC_current_extreme.Rdata"
# climate_change = "ERIC_climate_change.Rdata"
# extreme_climate_change = "ERIC_climate_change_extreme.Rdata"
# 
# #Weather data, replace where it says "current" with the appropriate weather dataset from above
# weather_data = paste0("dir/",current,sep = "") #replace dir with appropriate working directory

# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis")

# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis")

load("data_clean/mesonet_data/mesonet_weather.Rdata") # loads weather dataframe "wfull"
# contains weather data for lwma, sswma, cbma, and kiowa

mass = 21.4 #average mass of house finch
# tuc = (6.130*(log10(mass))) + 28.328 #upper critical limit from Wolf et al. 2020
# tlc = 22 #from gavrilov 2017 and gavrilov 2014

# freq = as.list(c(4000, 8000, 12000))

freq = c(4000,8000,12000)
terr_size = c(1000,1500,3000)
ewl = c(T,F)
site = c("lwma","sswma","cbma","kiowa")

terr_freq_ewl_site = expand.grid(terr_size,freq,ewl,site)
names(terr_freq_ewl_site) = c("terr_size","freq","ewl","site")
terr_freq_ewl_site$terr_size = as.numeric(terr_freq_ewl_site$terr_size)

# setwd("dir") #set to working directory with "Aridity_ABM_core_model_funct.R" file in it
source("src/abm_scripts/Aridity_ABM_core_model_funct.R")

ABMout <- apply(terr_freq_ewl_site, MARGIN = 1, function(x){
  aridityABM(HexSize=x[1], #territory size in m, original territory size is 1000m
           Song_volume = 85, #song/call volume in dB
           Song_detection = 30, #song detection in dB
           SProb = 0.33, #singing probability
           MProb = 0.33, #moving probability
           Song_freq = x[2], #song/call frequency (Hz), need to test 4000, 8000, and 12000khz
           mass = mass,  #mass of bird in grams
           # tuc = tuc, #upper critical limit of thermoregulation in C
           # tlc = tlc, #lower critical limit of thermoregulation in C
           # wdata = weather_data, #weather data used from Erick Mesonet station
           wdata = wfull %>% 
             dplyr::filter(as_date(date_time) >= "2021-05-01") %>%
             dplyr::filter(as_date(date_time) <= "2021-08-31"), # weather data for lwma, sswma, cbma, and kiowa
           ewl = x[3], #use evaporative waterloss (EWL) equation in the model or not. Set to true, the total EWL lost reduces probability of singing and moving probability, and increases resting probability.
           runTime = 360, #how many min model is run. 360 min = 6 hr
           iter = 1, #how many iterations the model is run
           plot = F,
           site_lab = "sswma") #plot the grid and hexagon territories
})

#Clearing saved images on dev.list(), uncomment if you plot the territory grids
# for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
#   dev.off()
# }


# ABMout_final = rbind(ABMout[[1]],ABMout[[2]],ABMout[[3]])
# ABMout_final = as.data.frame(ABMout[1])
ABMout_final = Reduce(full_join,ABMout)
ABMout_final$percent_comp = ABMout_final$Done/ABMout_final$FullTerrs #determine the percent of birds that successfully contact their neighbors

# setwd("dir") #set to appropriate working directory
# fname = paste0("climate_change",HexSize,"ewl",ewl,sep = "_")

save(ABMout_final, file = "data_clean/abm_results/sswma_abmout.Rdata") #change file name based on which weather conditons you use and if you the EWL equation is included or not.
save(ABMout_final, file = "data_clean/abm_results/cbma_abmout.Rdata") #change file name based on which weather conditons you use and if you the EWL equation is included or not.
save(ABMout_final, file = paste0("data_clean/abm_results/",terr_freq_ewl_site$site[1],"_abmout_paste_test.Rdata")) #change file name based on which weather conditons you use and if you the EWL equation is included or not.

save(ABMout_final, file = "data_clean/abm_results/cbma_abmout.Rdata") #change file name based on which weather conditons you use and if you the EWL equation is included or not.
save(ABMout_final, file = paste0("data_clean/abm_results/",terr_freq_ewl_site$site[1],"_abmout_paste_test.Rdata")) #change file name based on which weather conditons you use and if you the EWL equation is included or not.
save(ABMout_final, file = "data_clean/abm_results/sswma_abmout.Rdata") #change file name based on which weather conditons you use and if you the EWL equation is included or not.

