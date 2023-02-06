###################################################
#### Inverse distance weighted interpolation #####
###################################################

remotes::install_github('rspatial/terra')
remotes::install_github('rspatial/rspat')
devtools::install_github('r-spatial/gstat')

library(terra)
library(rspat)
library(gst)


# Interpolation tutorial - California Data --------------------------------

# Interpolation tutorial from: https://rspatial.org/analysis/4-interpolation.html
# Temperature in California, loading data
d <- spat_data('precipitation')
head(d)

# compute annual precipitation
mnts <- toupper(month.abb)
d$prec <- rowSums(d[, mnts])
plot(sort(d$prec), ylab="Annual precipitation (mm)", las=1, xlab="Stations") # each station is in a row

# Make a quick map
dsp <- vect(d, c("LONG", "LAT"), crs="+proj=longlat +datum=NAD83")
CA <- spat_data("counties")
# define groups for mapping
cuts <- c(0,200,300,500,1000,3000)
# set up a palette of interpolated colors
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
plot(CA, col="light gray", lwd=4, border="dark gray")
plot(dsp, "prec", type="interval", col=blues(10), legend=TRUE, cex=2,
     breaks=cuts, add=TRUE, plg=list(x=-117.27, y=41.54))
lines(CA)

# Transform lon/lat to planar coordinates using California corrdinate reference system ("Teale Albers") to assure that interpolation results will alight with other data sets we have

TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=WGS84 +units=m"
dta <- project(dsp, TA)
cata <- project(CA, TA)

# Null Model - interpolate (estimate for unsampled locations) the precipitation values. Simplest way or null model is to take the mean of all observations. Root mean square error (RMSE) will be the evaluation statistic

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

# Get RMSE for the null model
null <- RMSE(mean(dsp$prec), dsp$prec) #435 is target, can we get a smaller RMSE
null

# Proximity polygons can be used to interpolate categorical variables. Another term for this is nearest neighbor interpolation

v <- voronoi(dta)
plot(v)
points(dta)

#Cut out what is not california and map precipitaiton
vca <- crop(v, cata)
plot(vca, "prec")

# Now rasterize the results:

r <- rast(vca, res=10000)
vr <- rasterize(vca, r, "prec")
plot(vr)

# use 5-fold cross-validation to evaluate this model:

set.seed(5132015)
kf <- sample(1:5, nrow(dta), replace=TRUE)
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- dta[kf == k, ]
  train <- dta[kf != k, ]
  v <- voronoi(train)
  p <- extract(v, test)
  rmse[k] <- RMSE(test$prec, p$prec)
}
rmse
## [1] 192.0568 203.1304 183.5556 177.5523 205.6921
mean(rmse)
## [1] 192.3974
# relative model performance
perf <- 1 - (mean(rmse) / null)
round(perf, 3)
## [1] 0.558

### Nearest neighbor interpolation
## here we do nearest neighbor interpolation considering multiple (5) neighbors
# use the gstat package. first we fit a model, ~1 means intercept only. in the case of spatial data, only x and y coordinates are used.
# then set maximum number of points to 5, and the inverse distance power (idp) to zero, so all five neighbors are equally weighted

library(gstat)
d <- data.frame(geom(dta)[,c("x", "y")], as.data.frame(dta))
head(d)
gs <- gstat(formula=prec~1, locations=~x+y, data=d, nmax=5, set=list(idp = 0))
nn <- interpolate(r, gs, debug.level=0)
nnmsk <- mask(nn, vr)
plot(nnmsk, 1)

# cross validate the results. use the "predict" method to get predictions for the locations of the test points
rmsenn <- rep(NA, 5)
for (k in 1:5) {
  test <- d[kf == k, ]
  train <- d[kf != k, ]
  gscv <- gstat(formula=prec~1, locations=~x+y, data=train, nmax=5, set=list(idp = 0))
  p <- predict(gscv, test, debug.level=0)$var1.pred
  rmsenn[k] <- RMSE(test$prec, p)
}
rmsenn
## [1] 215.0993 209.5838 197.0604 177.1946 189.8130
mean(rmsenn)
## [1] 197.7502
1 - (mean(rmsenn) / null)
## [1] 0.5457377

### Inverse distance weighted (idw)
## idw is more common, points that are further away get less weight in predicting a value at a location

gs <- gstat(formula=prec~1, locations=~x+y, data=d)
idw <- interpolate(r, gs, debug.level=0)
idwr <- mask(idw, vr)
plot(idwr, 1)

# cross validate again. use predict for the locations of the test points

rmse <- rep(NA, 5) # creates vector of na's
for (k in 1:5) {
  test <- d[kf == k, ]
  train <- d[kf != k, ]
  gs <- gstat(formula=prec~1, locations=~x+y, data=train)
  p <- predict(gs, test, debug.level=0)
  rmse[k] <- RMSE(test$prec, p$var1.pred)
}
rmse
## [1] 243.3255 212.6270 206.8982 180.1829 207.5789
mean(rmse)
## [1] 210.1225
1 - (mean(rmse) / null)
## [1] 0.5173167


# Inverse Distance Weighting - New Mexico ---------------------------------

# Interpolation tutorial from: https://rspatial.org/analysis/4-interpolation.html
# New Mexico wind data from iowa mesonet

d <- spat_data('precipitation')
head(d)

# compute annual precipitation
mnts <- toupper(month.abb)
d$prec <- rowSums(d[, mnts])
plot(sort(d$prec), ylab="Annual precipitation (mm)", las=1, xlab="Stations") # each station is in a row

# Make a quick map
dsp <- vect(d, c("LONG", "LAT"), crs="+proj=longlat +datum=NAD83")
CA <- spat_data("counties")
# define groups for mapping
cuts <- c(0,200,300,500,1000,3000)
# set up a palette of interpolated colors
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
plot(CA, col="light gray", lwd=4, border="dark gray")
plot(dsp, "prec", type="interval", col=blues(10), legend=TRUE, cex=2,
     breaks=cuts, add=TRUE, plg=list(x=-117.27, y=41.54))
lines(CA)

# Transform lon/lat to planar coordinates using California corrdinate reference system ("Teale Albers") to assure that interpolation results will alight with other data sets we have

TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=WGS84 +units=m"
dta <- project(dsp, TA)
cata <- project(CA, TA)

# Null Model - interpolate (estimate for unsampled locations) the precipitation values. Simplest way or null model is to take the mean of all observations. Root mean square error (RMSE) will be the evaluation statistic

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

# Get RMSE for the null model
null <- RMSE(mean(dsp$prec), dsp$prec) #435 is target, can we get a smaller RMSE
null

# Proximity polygons can be used to interpolate categorical variables. Another term for this is nearest neighbor interpolation

v <- voronoi(dta)
plot(v)
points(dta)

#Cut out what is not california and map precipitaiton
vca <- crop(v, cata)
plot(vca, "prec")

# Now rasterize the results:

r <- rast(vca, res=10000)
vr <- rasterize(vca, r, "prec")
plot(vr)

# use 5-fold cross-validation to evaluate this model:

set.seed(5132015)
kf <- sample(1:5, nrow(dta), replace=TRUE)
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- dta[kf == k, ]
  train <- dta[kf != k, ]
  v <- voronoi(train)
  p <- extract(v, test)
  rmse[k] <- RMSE(test$prec, p$prec)
}
rmse
## [1] 192.0568 203.1304 183.5556 177.5523 205.6921
mean(rmse)
## [1] 192.3974
# relative model performance
perf <- 1 - (mean(rmse) / null)
round(perf, 3)
## [1] 0.558

### Nearest neighbor interpolation
## here we do nearest neighbor interpolation considering multiple (5) neighbors
# use the gstat package. first we fit a model, ~1 means intercept only. in the case of spatial data, only x and y coordinates are used.
# then set maximum number of points to 5, and the inverse distance power (idp) to zero, so all five neighbors are equally weighted

library(gstat)
d <- data.frame(geom(dta)[,c("x", "y")], as.data.frame(dta))
head(d)
gs <- gstat(formula=prec~1, locations=~x+y, data=d, nmax=5, set=list(idp = 0))
nn <- interpolate(r, gs, debug.level=0)
nnmsk <- mask(nn, vr)
plot(nnmsk, 1)

# cross validate the results. use the "predict" method to get predictions for the locations of the test points
rmsenn <- rep(NA, 5)
for (k in 1:5) {
  test <- d[kf == k, ]
  train <- d[kf != k, ]
  gscv <- gstat(formula=prec~1, locations=~x+y, data=train, nmax=5, set=list(idp = 0))
  p <- predict(gscv, test, debug.level=0)$var1.pred
  rmsenn[k] <- RMSE(test$prec, p)
}
rmsenn
## [1] 215.0993 209.5838 197.0604 177.1946 189.8130
mean(rmsenn)
## [1] 197.7502
1 - (mean(rmsenn) / null)
## [1] 0.5457377

### Inverse distance weighted (idw)
## idw is more common, points that are further away get less weight in predicting a value at a location

gs <- gstat(formula=prec~1, locations=~x+y, data=d)
idw <- interpolate(r, gs, debug.level=0)
idwr <- mask(idw, vr)
plot(idwr, 1)

# cross validate again. use predict for the locations of the test points

rmse <- rep(NA, 5) # creates vector of na's
for (k in 1:5) {
  test <- d[kf == k, ]
  train <- d[kf != k, ]
  gs <- gstat(formula=prec~1, locations=~x+y, data=train)
  p <- predict(gs, test, debug.level=0)
  rmse[k] <- RMSE(test$prec, p$var1.pred)
}
rmse
## [1] 243.3255 212.6270 206.8982 180.1829 207.5789
mean(rmse)
## [1] 210.1225
1 - (mean(rmse) / null)
## [1] 0.5173167



# Calculate wind speeds from 3 adjacent sites from iowa mesonet (Clayton, Raton, Las Vegas)

devtools::install_github("rspatial/rspat")
devtools::install_github("r-spatial/gstat")

library(rspat)
library(gstat)
library(tidyverse)
library(sp)
library(raster)

nm_mesonet = read_csv("kiowa_wind.csv") %>% 
  dplyr::rename(date_time = "valid") %>%
  dplyr::arrange(date_time)

nm_sites = nm_mesonet %>%
  dplyr::filter(station != "kiowa") %>%
  dplyr::filter(is.na(sknt) == FALSE)

kiowa = nm_mesonet %>%
  dplyr::filter(station == "kiowa")

nm_mesonet2 = rbind(nm_sites, kiowa) %>%
  dplyr::arrange(date_time)

coordinates(nm_mesonet)=~lat+lon
# d$N[c(5,6,7)]=NA
valid = !is.na(nm_mesonet$sknt)
predictions = idw(sknt~1, 
                  locations=nm_mesonet[valid,,drop=FALSE],
                  newdata=nm_mesonet[!valid,,drop=FALSE])
nm_mesonet$sknt[!valid] = predictions$var1.pred #putting interpolated data back into dataframe

notkiowa_wind = nm_mesonet@data %>% 
  dplyr::filter(station != "kiowa")

kiowa_wind = nm_mesonet@data %>%
  dplyr::filter(station == "kiowa") %>%
  dplyr::mutate(date_time = mdy_hm(date_time)) %>%
  dplyr::select(date_time, sknt) %>%
  dplyr::arrange(date_time)

kiowa_weather1.5 = left_join(kiowa_weather, kiowa_wind[,c("date_time","sknt")], by = "date_time")
