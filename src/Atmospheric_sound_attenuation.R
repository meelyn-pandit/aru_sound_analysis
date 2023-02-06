#Atmospheric Sound attenuation functions.

att_coef <- function(f, T_cel, h_rel, Pa=101.325){
  #f=frequency, d = distance in meters, T_cel = temperature in celcius
  #h_rel = Relative humidity (percent - "70" = 70% humidity)
  #Pa = ambient pressure (101.325 is standard, 1 atmosphere)
  
  Pr = 101.325  #reference pressure in kPa
  P_rel = Pa/Pr #relative pressure
  Kelvin = 273.15 #standard temp in Kelvin
  T_kel = T_cel + Kelvin #Temperature in K
  T_ref <- 293.15 #reference Temp K
  T_rel <- T_kel/T_ref #relative temperature
  T_01 = Kelvin + 0.01 #triple-point isotherm temperature in °K
  
  P_sat_over_P_ref = 10^((-6.8346 * (T_01 / T_kel)^1.261) + 4.6151)
  H = h_rel * (P_sat_over_P_ref/P_rel) #Molecular Concentration of water vapour
  Fro = P_rel * (24 + 40400 * H * (0.02 + H) / (0.391 + H)) 
  Frn = P_rel / sqrt(T_rel) * (9 + 280 * H * exp(-4.17 * (T_rel^(-1/3) - 1)))
  
  Xc = 0.0000000000184 / P_rel * sqrt(T_rel)
  Xo = 0.01275 * exp(-2239.1 / T_kel) * (Fro + (f^2 / Fro))^-1
  Xn = 0.1068 * exp(-3352 / T_kel) * (Frn + (f^2 / Frn))^-1
  Alpha = 8.68589 * f^2 * (Xc + T_rel^(-5/2) * (Xo + Xn))
  return(Alpha)
}

aud_range <- function(dbOri, 
                      dbMin, 
                      f, 
                      T_cel, 
                      h_rel, 
                      Pa #Pa=101.325
                      ) {
  #dbOri=original decible level, dmMin = threshold decible level for detection,
  #f=frequency, d = distance in meters, T_cel = temperature in celcius
  #h_rel = Relative humidity (percent - "70" = 70% humidity)
  #Pa = ambient pressure (101.325 is standard, 1 atmosphere)
  Alpha = att_coef(f, T_cel, h_rel, Pa) #Get absorption coefficient
  dbDiff = dbOri-dbMin #calculate loss of sound energy from original to threshold level
  dist <- dbDiff/Alpha #calculate the distance based on absorption coefficient
  return(dist)
}

#att_atm(f=8000, d=100, T_cel=20, h_rel=70)
#aud_range(dbOri = 110, dbMin = 50, f=5000, T_cel=20, h_rel=70)

