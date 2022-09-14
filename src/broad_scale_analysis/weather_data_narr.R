library(RNCEP) #download data from NARR at 6 hr intervals
library(lubridate) #modify timestamps
library(NicheMapR) #
library(microclima)


devtools::install_github('mrke/NicheMapR')
install.packages("remotes")
remotes::install_github("ilyamaclean/microclima")

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/weather_data/")
dstart <- "01/05/2021"
dfinish <- "15/08/2021"
loc <- c(-104.33505, 36.05919) # Kiowa National Grasslands - Mills Canyon
kiowa<-micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)
kiowa_data = kiowa[["microclima.out"]][["hourlydata"]]
kiowa_data$local_time = format(kiowa_data$obs_time, tz = "US/Mountain", usetz=TRUE)
kiowa_data$local_time = as.POSIXct(kiowa_data$local_time, format = "%Y-%m-%d %H:%M:%S")
shadmet = as.data.frame(kiowa[["shadmet"]])
metout = as.data.frame(kiowa[["metout"]])
kiowa_data$relh1 = shadmet$RH
kiowa_data$relh2 = metout$RH
save(kiowa_data, file = "kiowa_ncep_weather.Rdata")


dstart <- "01/05/2021"
dfinish <- "15/08/2021"
loc <- c(-101.97423, 35.39651) # cbma texas
cbma<-micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)
cbma_data = cbma[["microclima.out"]][["hourlydata"]]
cbma_data$local_time = format(cbma_data$obs_time, tz = "US/Central", usetz=TRUE)
cbma_data$local_time = as.POSIXct(cbma_data$local_time, format = "%Y-%m-%d %H:%M:%S")
shadmet = as.data.frame(cbma[["shadmet"]])
cbma_data$relh = shadmet$RH
save(cbma_data, file = "cbma_ncep_weather.Rdata")

dstart <- "01/05/2021"
dfinish <- "15/08/2021"
loc <- c(-97.193914, 35.061841) # lwma ok 	
lwma<-micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)
lwma_data = lwma[["microclima.out"]][["hourlydata"]]
lwma_data$local_time = format(lwma_data$obs_time, tz = "US/Central", usetz=TRUE)
lwma_data$local_time = as.POSIXct(lwma_data$local_time, format = "%Y-%m-%d %H:%M:%S")
shadmet = as.data.frame(lwma[["shadmet"]])
lwma_data$relh = shadmet$RH
save(lwma_data, file = "lwma_ncep_weather.Rdata")


dstart <- "01/05/2021"
dfinish <- "15/08/2021"
loc <- c(-99.839768, 35.069568) # sswma ok 		
sswma<-micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)
sswma_data = sswma[["microclima.out"]][["hourlydata"]]
sswma_data$local_time = format(sswma_data$obs_time, tz = "US/Central", usetz=TRUE)
sswma_data$local_time = as.POSIXct(sswma_data$local_time, format = "%Y-%m-%d %H:%M:%S")
shadmet = as.data.frame(sswma[["shadmet"]])
sswma_data$relh = shadmet$RH
save(sswma_data, file = "sswma_ncep_weather.Rdata")



# dstart <- "02/01/2017"
# dfinish <- "30/12/2017"
# loc <- c(-91.415669, -0.287145) # Isla Fernandina Galapagos
# micro<-micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)



# # Weather data for Sitka Ecology Center -----------------------------------
# dstart <- "01/08/2021" #change dates to your start date (dd/mm/yyyy)
# dfinish <- "31/12/2021" #change dates to stop date (dd/mm/yyyy)
# loc <- c(-123.99254, 45.04760) # Sitka Ecology Center gps coordinates (taken from google maps)
# weather_data<-micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)
# weather_data2 = weather_data[["microclima.out"]][["hourlydata"]]
# weather_data2$local_time = format(weather_data2$obs_time, tz = "US/Pacific", usetz=TRUE)
# weather_data2$local_time = as.POSIXct(weather_data2$local_time, format = "%Y-%m-%d %H:%M:%S")
# shadmet = as.data.frame(weather_data[["shadmet"]])
# weather_data2$relh = shadmet$RH

# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data")
# save(weather_data2, file = "sitka_weather_data.Rdata")














#Graveyard

