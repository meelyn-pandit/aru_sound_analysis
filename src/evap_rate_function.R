library(humidity) # used to get saturation vapor pressure equation
library(Thermimage) # used to get air density equation
library(bigleaf) # used to get another air density equation
library(plantecophys)
library(REddyProc)

# Calculate saturation vapor pressure Es at temperature t using the Clausius-Clapeyron equation or the Murray equation
#result is in hectopascal (hPa) or millibar (mb) (1 hPa = 1 mb)

SVP(25, isK = FALSE, formula = c("Clausius-Clapeyron","Murray")) # 32.22615
SVP.ClaCla(298.15) # 32.22615
SVP.Murray(298.15) # 31.67688

# Calculate air density based on temperature (C), taken from: https://rdrr.io/cran/Thermimage/man/airdensity.html

## The function is currently defined as
airdensity = function (Ta = 20) 
{
  Base <- 314.156
  Exponent <- (-0.981)
  p <- Base * (Ta + 273.15)^Exponent
  p
}

airdensity(25) # 1.17 (kg/(m^3))


# Calculate air density using bigleaf equation ----------------------------

air.density(25,101.3)

# Calculate evaporation rate based on aerodynamic method ------------------
### http://mgebrekiros.github.io/IntroductoryHydrology/EvaporationAndTranspiration.pdf
## Section 3.3.2 Aerodynamic method
#Equation 3.26

#Ea = B(eas-ea): equation for aerodynamic evaporation rate method; Ea in m/s (multiply by [1000 mm/m *86400 s/day] to get in mm/day)

# es = saturation vapor pressure at the ambient temperature T (Pa)
# ea = ed = actual vapor pressure (Pa) estimated using dew point temperatuer Td or by multipling es by the relative humidity Rh

#B = the vapor transfer coefficient (m/Pa/s)

#B = (0.622*(k^2)*pa*u2)/(p*pw*(ln(z2/z0))^2)

#k = Von Karman constant = 0.4
#u2= wind velocity (m/s) measured at height z2 (cm) and z0 is from 
#pa = density of moist air (kg/m^3)
#pw = density of water (kg/m^3)
#p = atmospheric pressure in kPa
#z2 = height of wind measurement (cm)
#z0 = roughness height of natural surface (cm) based on table below

# Surface                   Roughness height Zo (cm)
#--------------------------------------------------
# ice, mud flats          | 0.001 
# water                   | 0.01-0.06
# grass (up to 10 cm high)| 0.1-2.0
# grass (10-50 cm high)   | 2-5
# vegetation (1-2m high)  | 20
# trees (10-15 m high)    | 40-70

evap_rate = function(k = 0.4,
                     u2 = 3,
                     t = 25,
                     rh = 0.4,
                     pw = 997,
                     p = 101.3,
                     z2 = 2, # in m, equation converts to cm
                     z0 = 0.03){
  # pa = airdensity(t)
  pa = air.density(t,p)
  B = (0.622*(k^2)*pa*u2)/(p*1000*pw*(log((z2*100)/z0))^2)
  # return(B)
  es = (SVP(25, isK = FALSE, formula = "Clausius-Clapeyron"))*100 # convert to Pa
  ea = es*rh
  Ea = B*(es-ea)*1000*86400 # converts to mm/day
  return(Ea)
}

# only works for water with density of 1000 kg/m3 at 20C
evap_rate(t = 25,
          rh = 0.4,
          u2 = 3)*86400*1000

evap_rate_vol = function(x) {
  #'converts evaporation rate from mm/day to volume (m^3/ha/day) and then to microliters/cm2/day
  #'@param x The evaporation rate in mm/day
  evap_vol = x*10 # converts mm/day to m^3/ha/day
  evap_vol = evap_vol*10000000000 #converts m^3 to microliter
  evap_vol = evap_vol * (1/100000000)
  return(evap_vol)
}

?evap_rate_vol # see docstrings

evap_rate_vol(7.2)


# Calculate vapor pressure deficit, another metric of aridity -------------

ex_data = data.frame("rh" = 97,
                    "temp" = 16.7,
                    "pres" = 94.693)
RHtoVPD(ex_data$rh, ex_data$temp, ex_data$pres) # plantecophys calculation, relative humidity (%), temperature (C), pressure in kPa, VPD is in kPa. VPD = 0.05723602 kPa

fCalcVPDfromRHandTair(ex_data$rh, ex_data$temp)/10 # REddyProc calculation, relative humidity (%), temperature (C), VPD is in hPa, need to divide by 10 to get it in kPa. VPD = 0.05712262

rH.to.VPD(ex_data$rh/100, ex_data$temp) # bigLeaf calculation, relative humidity as a fraction, temperature (C), VPD is in kPa. VPD = 0.05690576

### Calculate VPD in base R ###
vpd_calc = function(temp, relh){
  #'Calculate vapor pressure deficit using saturation vapor pressure in PSI, temperature in C, and relative humidity (%)
  #'@param temp temperature in C
  #'@param relh relative humidity in %
  #'@param vpsat saturation vapor pressure in hectopascal hPa
  #'@returns vpd in kPa
  vpsat = SVP(temp, isK = FALSE)
  vpd = (vpsat*(1-(relh/100)))*0.1
  return(vpd)
}

vpd_calc(ex_data$temp,ex_data$rh) # VPD = 0.05746245


# Calculate EWL/delta water vpd -------------------------
# https://royalsocietypublishing.org/doi/10.1098/rspb.2017.1478#d1e1739
# EWL equation from Albright et al. 2017
ewl_calc = function(temp, relh, pres, mass = 21.4){
  ewl_log = (0.1181515*temp) + (0.0224677*mass) - 3.8895978
  ewl = (exp(ewl_log)) 
  ewl = ewl*mass # ewl * mass to get grams of water lost, ewl is percent of body mass lost in water

# wvp = RHtoVPD(ex_data$rh, ex_data$temp, ex_data$pres) # plantecophys calculation, relative humidity, temperature (C), pressure in kPa, VPD is in kPa. VPD = 0.05723602 kPa

# ewl_per_wvp = ewl/wvp
  # return(ewl_per_wvp)
  return(ewl) # units can be g/hr or mL/hr (1g = 1mL)
}

# EWL/deltaWVP = EWL per vapor pressure deficit, a way to get how much water is lost
# https://royalsocietypublishing.org/doi/10.1098/rspb.2017.1478#RSPB20171478F2


