###########################################################
##### Evaporation Rate with Aridity Gradient Dataset ######
###########################################################

# Load raw aridity gradient dataset
# This dataset has not been binned into the 4 acoustic morning periods (predawn, early, mid, late). Days with rain and damaged audio have been filtered out.

# I went ahead and calculated the evaporation rate with wind speed at 2m (evap_wind) and with wind speed set to 1 m/s (evap_1). Both have units as mm/5 min bin, that way we get the evaporation rate at that moment when the temperature and relative humidity were collected by the weather station.

# On days with 100 % relative humidity, the evaporation rate for both metrics is 0
# Higher evaporation rate = more arid

load("aridity_data_clean.Rdata")

# isolating relevant weather variables and the evaporation rate variables into a simpler table and running a correlation test to see which varibles are correlated
arid_comp = aw4 %>% dplyr::select(temp,relh,gh,evap_wind,evap_1)
cor(arid_comp)
# *gh is the old aridity variable

# evap_1 correlation coefficient is -0.995 against relative humidity, while evap_wind is -0.795 against relative humidity

cor.test(arid_comp$relh,arid_comp$evap_1) # p < 0.001

cor.test(arid_comp$relh, arid_comp$evap_wind) #p < 0.001

# histograms of both evaporation rate variables
hist(arid_comp$evap_1)
hist(arid_comp$evap_wind)


# Evaporation Rate - mm/day -----------------------------------------------

aw4 = aw4 %>%
  dplyr::mutate(ew_day = (evap_rate(u2 = ws2m, # evaporation rate with wind (mm/day)
                                    p = pres, 
                                    t = temp, 
                                    rh = (relh/100), 
                                    z0 = 0.03)*1000*86400),
                e1_day = evap_rate(u2 = 1, # evaporation rate with wind (mm/day)
                                   p = pres, 
                                   t = temp, 
                                   rh = (relh/100), 
                                   z0 = 0.03)*1000*86400)

arid_comp = aw4 %>% dplyr::select(temp, relh, gh, evap_wind, ew_day, evap_1, e1_day)
cor(arid_comp)

# Also need to talk about roughness height of natural surface (z0) do we use 0.03 because we are looking at evaporation from a body of water?
