library(humidity) # used to get saturation vapor pressure equation
library(Thermimage) # used to get air density equation
library(bigleaf) # used to get another air density equation

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
                     z2 = 2,
                     z0 = 0.03){
  # pa = airdensity(t)
  pa = air.density(t,p)
  B = (0.622*(k^2)*pa*u2)/(p*1000*pw*(log((z2*100)/z0))^2)
  # return(B)
  es = (SVP(25, isK = FALSE, formula = "Clausius-Clapeyron"))*100 # convert to Pa
  ea = es*rh
  Ea = B*(es-ea)
  return(Ea)
}

evap_rate(t = 25,
          rh = 0.4)



