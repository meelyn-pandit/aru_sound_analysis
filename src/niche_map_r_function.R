
nichemapr = function(
                     loc, # data location in (lng, lat)
                     usrhyt = 0.07,# Usrhyt <- 0.07 # local height (m) at which air temperature, wind speed, and humidity are to be computed for organism of interest (height of animal)
                     maxshade = 100, # max shade level to us (%), can be a single value or a vector of daily values
                     tc = 38, # core temperature (C)
                     tc_max = 43, # max core temperature (C)
                     tc_inc = 0.25, # increment by which tc is elevated (C)
                     mass = 0.0337, # mass in kg, mass of house finch is 0.0214
                     shape = 4, # use ellipsoid geometry
                     shape_b = 1.1, # start off near to a sphere
                     shape_b_max = 5, # maximum ratio of length to width/depth
                     uncurl = 0.1, # allows the animal to uncurl to shape_b_max, the value being the increment, shape_b is increased per iteration
                     dhaird = 3e-05, # hair diameter, dorsal (m)
                     dhairv = 3e-05, # hair diameter, ventral (m)
                     lhaird = 0.0231,  # hair length, dorsal (m)
                     lhairv = 0.0227,  # hair length, ventral (m)
                     zfurd = 0.0059,  # fur depth, dorsal (m)
                     zfurv = 0.0057,  # fur depth, ventral (m)
                     rhod = 5e+07, # hair density, dorsal (1/m2)
                     rhov = 5e+07, # hair density, ventral (1/m2)
                     refld = 0.248, # fur reflectivity dorsal (fractional, 0-1)
                     reflv = 0.351, # fur reflectivity ventral (fractional, 0-1)
                     pctwet = 0.1,  # base skin wetness (%)
                     pctwet_max = 0.1,  # maximum skin wetness (%)
                     pctwet_inc = 0.25,  # intervals by which skin wetness is increased (%)
                     q10 = 1,  # Q10 effect of body temperature on metabolic rate (-)
                     qbasal = 10^(-1.461 + 0.669 * log10(AMASS * 1000)),  # basal heat generation (W)
                     deltar = 5,  # offset between air temperature and breath (°C)
                     extref = 25,  # O2 extraction efficiency (%)
                     pant_inc = 0.1,  # turns on panting, the value being the increment by which the panting 
                     # multiplier is increased up to the maximum value, PANT_MAX
                     pant_max = 15,  # maximum panting rate - multiplier on air flow through the lungs above 
                     # that determined by metabolic rate
                     pant_mult = 1  # multiplier on basal metabolic rate at maximum panting level
                     ){
  # physiological responses
  
  library(NicheMapR)
  library(lubridate)
  
  # loc <- c(-104.33505, 36.05919) # Kiowa National Grasslands - Mills Canyon (lng, lat)
  # loc <- c(-101.97423, 35.39651) # Cross Bar Management Area, Potter County TX, (lng, lat)
  # loc <- c(-99.839768, 35.069568) # Sandy Sanders Wildlife Management Area, Erick, OK (lng, lat)
  # loc <- c(-97.193914, 35.061841) # Lexington Wildlife Management Area, Lexington, OK (lng, lat)
  
  dstart <- "01/05/2021"
  dfinish <- "31/08/2021"
  
  # Usrhyt <- 0.07 # local height (m) at which air temperature, wind speed, and humidity are to be computed for organism of interest (height of animal)
  Usrhyt = usrhyt
  maxshade <- maxshade
  # get.global.climate(folder = "data/global_climate/")
  # micro <- micro_global(loc = loc, 
  #                       Usrhyt = Usrhyt, 
  #                       maxshade = maxshade)
  micro = micro_usa(loc = loc,
                    dstart = dstart,
                    dfinish = dfinish,
                    maxshade=maxshade)
  
  metout <- as.data.frame(micro$metout)  # unshaded above-ground conditions
  soil <- as.data.frame(micro$soil)  # unshaded below-ground soil temperatures
  shadmet <- as.data.frame(micro$shadmet)  # shaded above-ground conditions
  shadsoil <- as.data.frame(micro$shadsoil)  # shaded below-ground soil temperatures
  # dates <- micro$dates
  dates <- as_datetime(micro$dates, tz = "UTC");dates_utc
  # date_comp = as.data.frame(cbind(dates,dates_utc))
  
  TAs <- metout$TALOC  # air temperatures at height of animal (°C)
  TAREFs <- metout$TAREF  # air temperatures at reference height (°C)
  TSKYs <- metout$TSKYC  # sky temperatures (°C)
  TGRDs <- soil$D0cm  # surface temperatures (°C)
  VELs <- metout$VLOC  # wind speeds at animal height (m/s)
  RHs <- metout$RHLOC  # relative humidity at animal height (%)
  QSOLRs <- metout$SOLR  # solar radiation (W/m2)
  Zs <- metout$ZEN  # zenith angle of the sun (degrees)
  ELEV <- micro$elev  # elevation (m)
  ABSSB <- 1 - micro$REFL  # substrate solar absorptivity (%)
  
  # core temperature
  TC <- tc  # core temperature (°C)
  TC_MAX <- tc_max  # maximum core temperature (°C)
  TC_INC <- tc_inc  # increment by which TC is elevated (°C)
  
  # size and shape
  AMASS <- mass  # mass (kg)
  SHAPE <- shape  # use ellipsoid geometry
  SHAPE_B <- shape_b  # start off near to a sphere (-)
  SHAPE_B_MAX <- shape_b_max  # maximum ratio of length to width/depth
  UNCURL <- uncurl  # allows the animal to uncurl to SHAPE_B_MAX, the value being the increment 
  # SHAPE_B is increased per iteration
  
  # feather properties
  DHAIRD <- dhaird  # hair diameter, dorsal (m)
  DHAIRV <- dhairv  # hair diameter, ventral (m)
  LHAIRD <- lhaird  # hair length, dorsal (m)
  LHAIRV <- lhairv  # hair length, ventral (m)
  ZFURD <- zfurd  # fur depth, dorsal (m)
  ZFURV <- zfurv  # fur depth, ventral (m)
  RHOD <- rhod  # hair density, dorsal (1/m2)
  RHOV <- rhov  # hair density, ventral (1/m2)
  REFLD <- refld  # fur reflectivity dorsal (fractional, 0-1)
  REFLV <- reflv  # fur reflectivity ventral (fractional, 0-1)
  
  # physiological responses
  PCTWET <- pctwet  # base skin wetness (%)
  PCTWET_MAX <- pctwet_max  # maximum skin wetness (%)
  PCTWET_INC <- pctwet_inc  # intervals by which skin wetness is increased (%)
  Q10 <- q10  # Q10 effect of body temperature on metabolic rate (-)
  QBASAL <- qbasal  # basal heat generation (W)
  DELTAR <- deltar  # offset between air temperature and breath (°C)
  EXTREF <- extref  # O2 extraction efficiency (%)
  PANT_INC <- pant_inc  # turns on panting, the value being the increment by which the panting 
  # multiplier is increased up to the maximum value, PANT_MAX
  PANT_MAX <- pant_max  # maximum panting rate - multiplier on air flow through the lungs above 
  # that determined by metabolic rate
  PANT_MULT <- pant_mult  # multiplier on basal metabolic rate at maximum panting level
  
  # run the model
  ptm <- proc.time()  # start timing
  endo.out <- lapply(1:length(TAs), function(x) {
    endoR(TA = TAs[x], TAREF = TAREFs[x], TSKY = TSKYs[x], TGRD = TGRDs[x],
          VEL = VELs[x], RH = RHs[x], QSOLR = QSOLRs[x], Z = Zs[x],
          ELEV = ELEV, ABSSB = ABSSB, TC = TC, TC_MAX = TC_MAX,
          AMASS = AMASS, SHAPE = SHAPE, SHAPE_B = SHAPE_B, SHAPE_B_MAX = SHAPE_B_MAX,
          PCTWET = PCTWET, PCTWET_INC = PCTWET_INC, Q10 = Q10,
          QBASAL = QBASAL, DELTAR = DELTAR, DHAIRD = DHAIRD, DHAIRV = DHAIRV,
          LHAIRD = LHAIRD, LHAIRV = LHAIRV, ZFURD = ZFURD, ZFURV = ZFURV,
          RHOD = RHOD, RHOV = RHOV, REFLD = REFLD, TC_INC = TC_INC,
          PANT_INC = PANT_INC, PANT_MAX = PANT_MAX, EXTREF = EXTREF,
          UNCURL = UNCURL, SAMODE = SAMODE, SHADE = 0, PANT_MULT = PANT_MULT)
  })
  proc.time() - ptm  # end timing
  
  # extract the output
  endo.out1 <- do.call("rbind", lapply(endo.out, data.frame))
  
  # thermoregulation output
  treg <- endo.out1[, grep(pattern = "treg", colnames(endo.out1))]
  colnames(treg) <- gsub(colnames(treg), pattern = "treg.", replacement = "")
  
  # morphometric output
  morph <- endo.out1[, grep(pattern = "morph", colnames(endo.out1))]
  colnames(morph) <- gsub(colnames(morph), pattern = "morph.",
                          replacement = "")
  
  # heat balance
  enbal <- endo.out1[, grep(pattern = "enbal", colnames(endo.out1))]
  colnames(enbal) <- gsub(colnames(enbal), pattern = "enbal.",
                          replacement = "")
  
  # mass aspects
  masbal <- endo.out1[, grep(pattern = "masbal", colnames(endo.out1))]
  colnames(masbal) <- gsub(colnames(masbal), pattern = "masbal.",
                           replacement = "")
  
  # extract variables for plotting
  QGEN <- enbal$QGEN  # metabolic rate (W)
  H2O <- masbal$H2OResp_g + masbal$H2OCut_g  # g/h water evaporated
  TFA_D <- treg$TFA_D  # dorsal fur surface temperature
  TFA_V <- treg$TFA_V  # ventral fur surface temperature
  TskinD <- treg$TSKIN_D  # dorsal skin temperature
  TskinV <- treg$TSKIN_V  # ventral skin temperature
  TCs <- treg$TC  # core temperature
  SkinW <- treg$SKINWET  # skin wetness (%)
  Pant <- treg$PANT  # panting multiplier (-)
  
  # plot the results
  par(mfrow = c(2, 2))
  par(oma = c(2, 1, 2, 2) + 0.1)
  par(mar = c(3, 3, 1.5, 1) + 0.1)
  par(mgp = c(2, 1, 0))
  plot(QGEN ~ dates, type = "l", ylab = "metabolic rate, W", xlab = "time",
       ylim = c(0, QBASAL * 5))
  plot(H2O ~ dates, type = "l", ylab = "water loss, g / h", xlab = "time",
       ylim = c(0, 2))
  points(masbal$H2OResp_g ~ dates, type = "l", lty = 2)
  points(masbal$H2OCut_g ~ dates, type = "l", lty = 2, col = "blue")
  legend(x = 1619852400, 
         y = 2, 
         legend = c("total", "respiratory", "cutaneous"),
         col = c("black", "black", "blue"), 
         lty = c(1, 2, 2), bty = "n")
  plot(TFA_D ~ dates, 
       type = "l", 
       col = "grey", 
       ylab = "temperature, °C",
       xlab = "time", 
       ylim = c(0, 60))
  points(TFA_V ~ dates, 
         type = "l", 
         col = "grey", 
         lty = 2)
  points(TskinD ~ dates, 
         type = "l", 
         col = "orange")
  points(TskinV ~ dates, 
         type = "l", 
         col = "orange", 
         lty = 2)
  points(TCs ~ dates, 
         type = "l", 
         col = "red")
  legend(x = 1619852400, 
         y = 50, legend = c("core", 
                            "skin dorsal", 
                            "skin ventral",
                            "feathers dorsal", 
                            "feathers ventral"), 
         col = c("red", 
                 "orange",
                 "orange", 
                 "grey", 
                 "grey"), 
         lty = c(1, 1, 2, 1, 2), 
         bty = "n",
         ncol = 2)
  
  plot((H2O/(AMASS * 1000)) * 100 ~ dates, 
       type = "l", 
       ylab = "H2O loss, % body mass / h",
       xlab = "time" #, 
       # ylim = c(0, 6)
       )
}

nichemapr(loc = c(-104.33505, 36.05919),
          mass = 0.0214)
# loc <- c(-104.33505, 36.05919) # Kiowa National Grasslands - Mills Canyon (lng, lat)
# loc <- c(-101.97423, 35.39651) # Cross Bar Management Area, Potter County TX, (lng, lat)
# loc <- c(-99.839768, 35.069568) # Sandy Sanders Wildlife Management Area, Erick, OK (lng, lat)
# loc <- c(-97.193914, 35.061841) # Lexington Wildlife Management Area, Lexington, OK (lng, lat)